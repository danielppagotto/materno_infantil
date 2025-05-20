
library(tidyverse)
library(RODBC)
library(patchwork)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(scales)
library(ggspatial) 
library(sf)
library(readxl)
library(leaflet)
library(rlang)

# Leitura dos dados -------------------------------------------------------


dremio_host <- Sys.getenv("endereco")
dremio_port <- Sys.getenv("port")
dremio_uid <- Sys.getenv("uid")
dremio_pwd <- Sys.getenv("datalake")


channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;
                                     HOST=%s;
                                     PORT=%s;
                                     UID=%s;
                                     PWD=%s;
                                     AUTHENTICATIONTYPE=Basic Authentication;
                                     CONNECTIONTYPE=Direct", 
                                     dremio_host, 
                                     dremio_port, 
                                     dremio_uid, 
                                     dremio_pwd))

query_servico <- 
  'SELECT * FROM "@daniel"."servicos_NV_24_30"'


servicos <- sqlQuery(channel, 
                     query_servico, 
                     as.is = TRUE)

cobertura_projetada <- 
  read_csv("~/GitHub/materno_infantil/02_1_script_capitulo3/10_cenarios_ans/cenarios_ans_projetado.csv") |> 
  select(-`...1`) |> 
  mutate(cobertura = cobertura/100) |> 
  mutate(cod_regsaud = as.character(cod_regsaud)) |> 
  mutate(cenario = str_replace(cenario, "cenario ", "")) |> 
  filter(ano > 2023) |> 
  pivot_wider(
    id_cols = c(uf_sigla, cod_regsaud, regiao_saude, ano),
    names_from = cenario,
    values_from = cobertura
    ) |> 
  select(-uf_sigla, -regiao_saude)

foco_clinico_projetado <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv")

projecao_enfermeiros_combinados <- 
  read_csv("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_enfermeiros_combinados.csv") |> 
  rename(cenario_reducao10 = cenario_aumento,
         cenario_reducao20 = cenario_reducao)

oferta_medicos <- 
  read_csv("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_medicos_combinados.csv") |> 
  mutate(ano = year(ds),
         mes = month(ds)) |> 
  rename(cenario_reducao10 = cenario_aumento,
         cenario_reducao20 = cenario_reducao) |> 
  select(-regiao_saude)



# Parâmetros que futuramente irão para a função ---------------------------

acoes_hab <- 30
prenatal_hab <- 30
indireta <- 0.50
todos <- 1
ferias <- 16 #horas mensais - ou seja, dois dias
feriados <- 8 #horas mensais
absenteismo <- 24
prenatal_alto <- 45
alto_risco <- 0.15
acoes_alto <- 50
ferias <- 16
feriados <- 4
imunizacao <- 20
coleta_exames <- 15
visita <- 60
consulta_puerperal <- 30
consulta_cd <- 30
enf_coleta_exames <- 0.75
enf_coleta_cito <- 0.25
enf_prenatal <- 0.50 
enf_imunizacao <- 0.80
enf_puerperal <- 0.50
enf_visita <- 0.50 
enf_cd <- 0.50
enf_acoes <- 0.50
ist_medico <- 0.15
ist_enf <- 0.15

# Aplicando sem função ----------------------------------------------------

# Filtrar procedimentos de 2023

servicos24_30 <- 
  servicos |> 
  rename(mes_proc_rea = mês_procedimento_realizado,
         publico = Público,
         mes_programado = mes,
         qtd_nascidos = qtd,
         qtd_proc = quantidade) |> 
  mutate(ano_proc_rea = year(mes_proc_rea)) |> 
  filter(ano_proc_rea %in% c(2025, 2026, 2027, 
                             2028, 2029, 2030, 
                             2031, 2032)) |> 
  filter(nivel_atencao == "APS" & 
         publico != "Gestantes de Alto Risco") |> 
  filter(codigo_sigtap %in% c("0301010110","0202","0203",
                             "0101010010","0301010129",
                             "1234","0301010080",
                             "0301010137")) |> 
  filter(!(procedimento == "Visita domiciliar" & mes_programado != -7))


cobertura$cod_regsaud <- as.character(cobertura$cod_regsaud)


servicos24_30 |> 
  filter(cod_regsaud == "28001" & 
         codigo_sigtap == "0301010110") |> 
  mutate(mes_proc_rea = as.Date(mes_proc_rea)) |> 
  group_by(mes_proc_rea, procedimento) |> 
  summarise(qtd = sum(qtd_proc)) |> 
  ggplot(aes(x = mes_proc_rea, 
             y = qtd)) + geom_col() +
  theme_minimal()
  

servicos24_30 |> 
  filter(cod_regsaud == "29020") |> 
  select(competen, qtd_nascidos) |>
  mutate(competen = as.Date(competen)) |> 
  mutate(ano = year(competen)) |> 
  distinct() |>
  ungroup() |> 
  ggplot(aes(x = competen, 
             y = qtd_nascidos)) + 
  geom_line()

# Juntar dados de serviços com cobertura

servicos2430_tratado <- 
  servicos24_30 |> 
  left_join(cobertura_projetada, 
            by = c("cod_regsaud"=
                   "cod_regsaud",
                   "ano_proc_rea"="ano")) |> 
  janitor::clean_names() 

#write.csv(servicos2430_tratado,
#          "~/GitHub/materno_infantil/02_script_tratado/01_dfs/servicos2430_tratado.csv")

# se todos = 1, pegar todos 
# se todos = 0, pegar apenas SUS dependente

sus <- 1
todos <- 0

# por enquanto estamos trabalhando com ANS = constante, 
# mas este é o caso de adicionar como argumento de função

if(sus == todos){
  
  servicos2430_procedimentos <- 
    
    servicos2430_tratado |> 
    
    mutate(qtd_proc_rh = 
             case_when(
               procedimento == "Consulta pré-natal" ~ 
                 qtd_proc * (1-alto_risco),
               procedimento == "Ações Educacionais" ~
                 qtd_proc * (1-alto_risco),
               procedimento == "Visita domiciliar" ~ 
                 qtd_proc * (1-alto_risco),
               TRUE ~ qtd_proc
             )) |> 
    mutate(qtd_proc_ar = 
             case_when(
               procedimento == "Consulta pré-natal" ~ 
                 qtd_proc * (alto_risco),
               procedimento == "Ações Educacionais" ~
                 qtd_proc * (alto_risco),
               procedimento == "Visita domiciliar" ~ 
                 2 * qtd_proc * alto_risco, 
               TRUE ~ 0
             ) 
    )
} else {
  
  servicos2430_procedimentos <- 
  
    servicos2430_tratado |>
    
    mutate(qtd_proc = (qtd_nascidos - 
                      (qtd_nascidos * constante)) *  ### LEMBRAR de colocar o constante como argumento de função
                       parametro) |> 
    mutate(qtd_proc_rh = 
             case_when(
               procedimento == "Consulta pré-natal" ~ 
                 qtd_proc * (1-alto_risco),
               procedimento == "Ações Educacionais" ~
                 qtd_proc * (1-alto_risco),
               procedimento == "Visita domiciliar" ~
               qtd_proc * (1-alto_risco),
               TRUE ~ qtd_proc
             )) |> 
    mutate(qtd_proc_ar = 
             case_when(
               procedimento == "Consulta pré-natal" ~ 
                 qtd_proc * (alto_risco),
               procedimento == "Ações Educacionais" ~
                 qtd_proc * (alto_risco),
               procedimento == "Visita domiciliar" ~ 
                 2 * qtd_proc * alto_risco,
               TRUE ~ 0
             )) 
}

servicos2430_procedimentos <- 
  servicos2430_procedimentos |> 
  select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaud, 
         regiao_saude, qtd_nascidos, codigo_sigtap,
         procedimento, tipo_procedimento, mes_programado,
         publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
         qtd_proc_ar)

# Traduzir número de horas em número de profissionais necessários

ttd <- 160 - ferias - feriados - absenteismo

necessidade2430 <-
  servicos2430_procedimentos |> 
  mutate(total_horas_rh = 
           case_when(codigo_sigtap == "0301010110" ~ 
                       qtd_proc_rh * prenatal_hab/60, 
                     codigo_sigtap == "0301010129" ~ 
                       qtd_proc_rh * consulta_puerperal/60,
                     codigo_sigtap == "1234" ~ 
                       qtd_proc_rh * imunizacao/60, 
                     codigo_sigtap == "0101010010" ~ 
                       qtd_proc_rh * acoes_hab/60,
                     codigo_sigtap == "0301010137" ~
                       qtd_proc_rh * visita/60,
                     codigo_sigtap == "0202" ~ 
                       qtd_proc_rh * coleta_exames/60,
                     codigo_sigtap == "0203" ~ 
                       qtd_proc_rh * coleta_exames/60,
                     codigo_sigtap == "0301010080" ~ 
                       qtd_proc_rh * consulta_cd/60)) |> 
  mutate(total_horas_ar = 
           case_when(codigo_sigtap == "0301010110" ~ 
                       qtd_proc_ar * prenatal_alto/60, 
                     codigo_sigtap == "0301010129" ~ 
                       qtd_proc_ar * consulta_puerperal/60,
                     codigo_sigtap == "1234" ~ 
                       qtd_proc_ar * imunizacao/60, 
                     codigo_sigtap == "0101010010" ~ 
                       qtd_proc_ar * acoes_alto/60,
                     codigo_sigtap == "0301010137" ~
                       qtd_proc_ar * visita/60,
                     codigo_sigtap == "0202" ~ 
                       qtd_proc_ar * coleta_exames/60,
                     codigo_sigtap == "0203" ~ 
                       qtd_proc_ar * coleta_exames/60,
                     codigo_sigtap == "0301010080" ~ 
                       qtd_proc_ar * consulta_cd/60)) |> 
  mutate(nec_prof = (total_horas_rh + total_horas_ar)) |> 
  mutate(nec_enf = case_when(codigo_sigtap == "0301010110" ~
                               nec_prof * enf_prenatal,
                             codigo_sigtap == "0301010129" ~ 
                               nec_prof * enf_puerperal, 
                             codigo_sigtap == "1234" ~ 
                               nec_prof * enf_imunizacao, 
                             codigo_sigtap == "0101010010" ~ 
                               nec_prof * enf_acoes,
                             codigo_sigtap == "0301010137" ~
                               nec_prof * enf_visita, 
                             codigo_sigtap == "0202" ~ 
                               nec_prof * enf_coleta_exames,
                             codigo_sigtap == "0203" ~
                               nec_prof * enf_coleta_cito,
                             codigo_sigtap == "0301010080" ~
                               nec_prof * enf_cd)) |> 
  mutate(nec_med = nec_prof - nec_enf) |> 
  mutate(nec_med = (1 + ist_medico) * nec_med) |>
  mutate(nec_enf = (1 + ist_enf) * nec_enf)
  
nec_prof2430 <- 
  necessidade2430 |>
  group_by(ano_proc_rea,
           mes_proc_rea, 
           cod_regsaud,
           regiao_saude,
           uf_sigla) |> 
  summarise(nec_med = sum(nec_med),
            nec_enf = sum(nec_enf)) |> 
  mutate(nec_med_prof = nec_med/ttd,
         nec_enf_prof = nec_enf/ttd) |> 
  rename(ano = ano_proc_rea) |> 
  mutate(mes = month(mes_proc_rea))|> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  mutate(mes_proc_rea = as.Date(mes_proc_rea))

nec_prof2430 |> 
  filter(cod_regsaud == "28001") |> 
  ggplot(aes(x = mes_proc_rea, y = nec_med_prof)) + 
  geom_point() + geom_smooth() +
  ylim(0, 100)

# Trechinho para tratar a questão da priorização  
# da medicina em relação a enfermagem

nec_enf <- nec_prof2430 |> 
                ungroup() |> 
                select(ano, 
                       mes,
                       uf_sigla,
                       cod_regsaud,
                       nec_enf_prof) 


oferta_enfermeiros <- 
  projecao_enfermeiros_combinados |>
    mutate(ano = year(ds),
           mes = month(ds)) |> 
  left_join(foco_clinico_projetado, 
            by = c("cod_regsaud"="cod_regsaud",
                   "ano"="ano")) |> 
  mutate(oferta_enf = cenario_base * (1 - indireta) * perc_fc)

##### ATENÇÃO: PEGUEI O CENARIO BASE - MAIS UMA SIMULACAO

nec_vs_oferta_enfermagem <- 
  nec_enf |> 
  left_join(oferta_enfermeiros, 
            by = c("cod_regsaud"=
                   "cod_regsaud",
                   "mes"="mes",
                   "ano"="ano",
                   "uf_sigla"="uf")) |> 
  mutate(ra_enf = oferta_enf - nec_enf_prof) |> 
  mutate(rr_enf = oferta_enf/nec_enf_prof) |> 
  mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                    rr_enf >= 0.50 & 
                                    rr_enf <= 1 ~ "C50",
                                    rr_enf < 0.50 ~ "C")) |> 
  ungroup() |> 
  filter(ds > "2025-01-01")

nec_vs_oferta_enfermagem <- 
  nec_vs_oferta_enfermagem |> 
  select(cod_regsaud, regiao_saude, uf_sigla, mes, 
         ano, nec_enf_prof, 
         oferta_enf, ra_enf, rr_enf, equilibrio_enf)


oferta_vs_demanda_enf_anual <- 
  nec_vs_oferta_enfermagem |> 
  group_by(ano, uf_sigla, cod_regsaud, regiao_saude) |> 
  summarise(necessidade_media = mean(nec_enf_prof),
            oferta_media = mean(oferta_enf)) |> 
  mutate(rr_enf = 100 * (oferta_media/necessidade_media)) |> 
  mutate(ra_enf = (oferta_media - necessidade_media)) 

# voltando para a oferta 
# vamos adicionar a logica de que - caso de faltar profissional de 
# enfermagem, a carga de trabalho deverá ser jogada para o médico
# fizemos desta forma, pois a enfermagem tem maiores % nos procedimentos
# em média
# Juntar necessidades com oferta

oferta_vs_demanda_med <- 
  nec_prof |> 
    mutate(mes_proc_rea = as.Date(mes_proc_rea)) |> 
    filter(mes_proc_rea > "2025-01-01") |> 
    ungroup() |> 
    left_join(oferta_medicos , 
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico_projetado, 
            by = c("cod_regsaud"="cod_regsaud",
                   "ano"="ano")) |> 
    mutate(oferta_med = cenario_base * (1 - indireta) * perc_fc) |> 
    left_join(nec_vs_oferta_enfermagem,
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano",
                     "uf_sigla"="uf_sigla")) |>
    mutate(nec_med_realoc = 
             case_when(equilibrio_enf == "NC" ~ nec_med_prof,
                       equilibrio_enf == "C50" |
                       equilibrio_enf == "C" ~ nec_med_prof + (-1 * ra_enf))
           ) |> 
    mutate(ra_med = oferta_med - nec_med_realoc,
           rr_med = 100 * (oferta_med/nec_med_realoc)) 

oferta_vs_demanda_med_anual <- 
    oferta_vs_demanda_med |> 
    group_by(ano, uf_sigla, cod_regsaud) |> 
    summarise(necessidade_media = mean(nec_med_realoc),
              oferta_media = mean(oferta_med)) |> 
    mutate(rr_med = 100 * (oferta_media/necessidade_media)) |> 
    mutate(ra_med = (oferta_media - necessidade_media)) 

med_enf_oferta_demanda <- 
  oferta_vs_demanda_med_anual |> 
  left_join(oferta_vs_demanda_enf_anual,
             by = c("cod_regsaud","uf_sigla",
                    "ano")) |> 
  mutate(classificacao = case_when(
                         rr_med >= 100 & 
                         rr_enf >= 100 ~ "Ambos superávit",
                         rr_med >= 100 &
                         rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
                         rr_enf >= 100 &
                         rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
                         rr_med <= 100 & 
                         rr_enf <= 100 ~ "Ambos déficit")) |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  )) |> 
  rename(necessidade_med = necessidade_media.x,
         oferta_med = oferta_media.x,
         necessidade_enf = necessidade_media.y,
         oferta_enf = oferta_media.y)


# dist_uf <- med_enf_oferta_demanda |> 
#   group_by(uf_sigla, classificacao) |> 
#   count()
# 
# dist_uf |> 
#   ggplot(aes(x = uf_sigla, y = n, fill = classificacao)) + 
#   geom_col(position = "fill") +  coord_flip() + 
#   xlab("UF") +
#   theme_minimal() + 
#   theme(legend.position = "bottom") 
# 
# med_enf_oferta_demanda  |> 
#   ggplot(aes(x = rr_enf, y = rr_med,
#              col = Região)) + 
#   geom_point() + 
#   geom_hline(yintercept = 100, 
#              linetype = "dashed",
#              color = "red") +
#   geom_vline(xintercept = 100, 
#              linetype = "dashed",
#              color = "red") +
#   geom_smooth(method = 'lm', se = FALSE) + 
#   facet_wrap(~Região)  + 
#   theme_bw() +
#   xlab("Enfermeiros - RR(%)") + 
#   ylab("Médicos - RR(%)") +
#   xlim(0,400) + ylim(0,400)


# Criando uma função ------------------------------------------------------

# Partimos de alguns dados já tratados

servicos2340_tratado <- 
  read_csv("~/GitHub/materno_infantil/02_script_tratado/01_dfs/servicos2430_tratado.csv") |> 
  select(-`...1`)

cobertura_projetada <- 
  read_csv("~/GitHub/materno_infantil/02_1_script_capitulo3/10_cenarios_ans/cenarios_ans_projetado.csv") |> 
  select(-`...1`) |> 
  mutate(cobertura = cobertura/100) |> 
  mutate(cod_regsaud = as.character(cod_regsaud)) |> 
  mutate(cenario = str_replace(cenario, "cenario ", "")) |> 
  filter(ano > 2023) |> 
  pivot_wider(
    id_cols = c(uf_sigla, cod_regsaud, regiao_saude, ano),
    names_from = cenario,
    values_from = cobertura
  ) |> 
  select(-uf_sigla, -regiao_saude)

foco_clinico_projetado <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv")

projecao_enfermeiros_combinados <- 
  read_csv("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_enfermeiros_combinados.csv") |> 
  rename(cenario_reducao10 = cenario_aumento,
         cenario_reducao20 = cenario_reducao)

oferta_medicos <- 
  read_csv("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_medicos_combinados.csv") |> 
  mutate(ano = year(ds),
         mes = month(ds)) |> 
  rename(cenario_reducao10 = cenario_aumento,
         cenario_reducao20 = cenario_reducao) |> 
  select(-regiao_saude)




oferta_vs_demanda <- function(acoes_hab, prenatal_hab, indireta_enf,
                              indireta_med, todos, absenteismo, 
                              prenatal_alto, alto_risco, acoes_alto, 
                              ferias, feriados, imunizacao, coleta_exames,
                              visita, consulta_puerperal, consulta_cd, 
                              enf_coleta_exames, enf_coleta_cito, enf_prenatal,
                              enf_imunizacao, enf_puerperal, enf_visita, enf_cd, 
                              enf_acoes, ist_enf, ist_med, 
                              tipo_ans, cenario_oferta){
  

  
  sus <- 1
  
  if(sus == todos){
    
    servicos2430_procedimentos <- 
      
      servicos2430_tratado |> 
      mutate(valor_ans = .data[[tipo_ans]]) |>
      mutate(qtd_proc_rh = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (1-alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (1-alto_risco),
                 procedimento == "Visita domiciliar" ~ 
                   qtd_proc * (1-alto_risco),
                 TRUE ~ qtd_proc
               )) |> 
      mutate(qtd_proc_ar = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (alto_risco),
                 procedimento == "Visita domiciliar" ~ 
                   2 * qtd_proc * alto_risco, 
                 TRUE ~ 0
               ) 
      )
  } else {
    
    servicos2430_procedimentos <- 
      
      servicos2430_tratado |>
      mutate(valor_ans = .data[[tipo_ans]]) |>
      mutate(qtd_proc = (qtd_nascidos - 
                           (qtd_nascidos * valor_ans)) * 
               parametro) |> 
      mutate(qtd_proc_rh = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (1-alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (1-alto_risco),
                 procedimento == "Visita domiciliar" ~
                   qtd_proc * (1-alto_risco),
                 TRUE ~ qtd_proc
               )) |> 
      mutate(qtd_proc_ar = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (alto_risco),
                 procedimento == "Visita domiciliar" ~ 
                   2 * qtd_proc * alto_risco,
                 TRUE ~ 0
               )) 
  }
  
servicos2430_procedimentos <- 
    servicos2430_procedimentos |> 
    select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaud, 
         regiao_saude, qtd_nascidos, codigo_sigtap,
         procedimento, tipo_procedimento, mes_programado,
         publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
         qtd_proc_ar)
  
  # Traduzir número de horas em número de profissionais necessários
  
  ttd <- 160 - ferias - feriados - absenteismo
  
  necessidade <-
    servicos2430_procedimentos |> 
    mutate(total_horas_rh = 
             case_when(codigo_sigtap == "0301010110" ~ 
                         qtd_proc_rh * prenatal_hab/60, 
                       codigo_sigtap == "0301010129" ~ 
                         qtd_proc_rh * consulta_puerperal/60,
                       codigo_sigtap == "1234" ~ 
                         qtd_proc_rh * imunizacao/60, 
                       codigo_sigtap == "0101010010" ~ 
                         qtd_proc_rh * acoes_hab/60,
                       codigo_sigtap == "0301010137" ~
                         qtd_proc_rh * visita/60,
                       codigo_sigtap == "0202" ~ 
                         qtd_proc_rh * coleta_exames/60,
                       codigo_sigtap == "0203" ~ 
                         qtd_proc_rh * coleta_exames/60,
                       codigo_sigtap == "0301010080" ~ 
                         qtd_proc_rh * consulta_cd/60)) |> 
    mutate(total_horas_ar = 
             case_when(codigo_sigtap == "0301010110" ~ 
                         qtd_proc_ar * prenatal_alto/60, 
                       codigo_sigtap == "0301010129" ~ 
                         qtd_proc_ar * consulta_puerperal/60,
                       codigo_sigtap == "1234" ~ 
                         qtd_proc_ar * imunizacao/60, 
                       codigo_sigtap == "0101010010" ~ 
                         qtd_proc_ar * acoes_alto/60,
                       codigo_sigtap == "0301010137" ~
                         qtd_proc_ar * visita/60,
                       codigo_sigtap == "0202" ~ 
                         qtd_proc_ar * coleta_exames/60,
                       codigo_sigtap == "0203" ~ 
                         qtd_proc_ar * coleta_exames/60,
                       codigo_sigtap == "0301010080" ~ 
                         qtd_proc_ar * consulta_cd/60)) |> 
    mutate(nec_prof = (total_horas_rh + total_horas_ar)) |> 
    mutate(nec_enf = case_when(codigo_sigtap == "0301010110" ~
                                 nec_prof * enf_prenatal,
                               codigo_sigtap == "0301010129" ~ 
                                 nec_prof * enf_puerperal, 
                               codigo_sigtap == "1234" ~ 
                                 nec_prof * enf_imunizacao, 
                               codigo_sigtap == "0101010010" ~ 
                                 nec_prof * enf_acoes,
                               codigo_sigtap == "0301010137" ~
                                 nec_prof * enf_visita, 
                               codigo_sigtap == "0202" ~ 
                                 nec_prof * enf_coleta_exames,
                               codigo_sigtap == "0203" ~
                                 nec_prof * enf_coleta_cito,
                               codigo_sigtap == "0301010080" ~
                                 nec_prof * enf_cd)) |> 
    mutate(nec_enf = nec_enf * (1 + ist_enf),
           nec_med = nec_prof - nec_enf,
           nec_med = nec_med * (1 + ist_med))
  
  nec_prof2430 <- 
    necessidade |>
    group_by(ano_proc_rea,
             mes_proc_rea, 
             cod_regsaud,
             regiao_saude,
             uf_sigla) |> 
    summarise(nec_med = sum(nec_med),
              nec_enf = sum(nec_enf)) |> 
    mutate(nec_med_prof = nec_med/ttd,
           nec_enf_prof = nec_enf/ttd) |> 
    rename(ano = ano_proc_rea) |> 
    mutate(mes = month(mes_proc_rea))|> 
    mutate(cod_regsaud = as.numeric(cod_regsaud)) 
  
  
# Trechinho para tratar a questão da priorização
  
  nec_enf <- nec_prof2430 |> 
    ungroup() |> 
    select(ano, 
           mes,
           uf_sigla,
           cod_regsaud,
           nec_enf_prof) 
  
  
  oferta_enfermeiros <- 
    projecao_enfermeiros_combinados |>
    mutate(ano = year(ds),
           mes = month(ds)) |> 
    left_join(foco_clinico_projetado, 
              by = c("cod_regsaud"="cod_regsaud",
                     "ano"="ano")) |>
    mutate(valor_oferta_enf = .data[[cenario_oferta]]) |>
    mutate(oferta_enf = 
             valor_oferta_enf * (1 - indireta_enf) * perc_fc)
  
  nec_vs_oferta_enfermagem <- 
    nec_enf |> 
    left_join(oferta_enfermeiros, 
              by = c("cod_regsaud"=
                     "cod_regsaud",
                     "mes"="mes",
                     "ano"="ano",
                     "uf_sigla"="uf")) |> 
    mutate(ra_enf = oferta_enf - nec_enf_prof) |> 
    mutate(rr_enf = oferta_enf/nec_enf_prof) |> 
    mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                      rr_enf >= 0.50 & 
                                        rr_enf <= 1 ~ "C50",
                                      rr_enf < 0.50 ~ "C")) |> 
    ungroup() |> 
    filter(ds > "2025-01-01")
  
  nec_vs_oferta_enfermagem <- 
    nec_vs_oferta_enfermagem |> 
    select(cod_regsaud, regiao_saude, uf_sigla, mes, 
           ano, nec_enf_prof, 
           oferta_enf, ra_enf, rr_enf, equilibrio_enf)
  
  oferta_vs_demanda_enf_anual <- 
    nec_vs_oferta_enfermagem |> 
    group_by(ano, uf_sigla, cod_regsaud, regiao_saude) |> 
    summarise(necessidade_media = mean(nec_enf_prof),
              oferta_media = mean(oferta_enf)) |> 
    mutate(rr_enf = 100 * (oferta_media/necessidade_media)) |> 
    mutate(ra_enf = (oferta_media - necessidade_media)) 
  
  # voltando para a oferta 
  # vamos adicionar a logica de que - caso de faltar profissional de 
  # enfermagem, a carga de trabalho deverá ser jogada para o médico
  # fizemos desta forma, pois a enfermagem tem maiores % nos procedimentos
  # em média
  # Juntar necessidades com oferta
  
  oferta_vs_demanda_med <- 
    nec_prof2430 |> 
    mutate(mes_proc_rea = as.Date(mes_proc_rea)) |> 
    filter(mes_proc_rea > "2025-01-01") |> 
    ungroup() |> 
    left_join(oferta_medicos, 
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico_projetado, 
              by = c("cod_regsaud"="cod_regsaud",
                     "ano"="ano")) |> 
    mutate(valor_oferta_med = .data[[cenario_oferta]]) |>
    mutate(oferta_med = 
           valor_oferta_med * (1 - indireta_med) * perc_fc) |> 
    left_join(nec_vs_oferta_enfermagem,
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano",
                     "uf_sigla"="uf_sigla")) |>
    mutate(nec_med_realoc = 
             case_when(equilibrio_enf == "NC" ~ nec_med_prof,
                       equilibrio_enf == "C50" |
                         equilibrio_enf == "C" ~ nec_med_prof + (-1 * ra_enf))
    ) |> 
    mutate(ra_med = oferta_med - nec_med_realoc,
           rr_med = 100 * (oferta_med/nec_med_realoc)) 
  
  oferta_vs_demanda_med_anual <- 
    oferta_vs_demanda_med |> 
    group_by(ano, uf_sigla, cod_regsaud) |> 
    summarise(necessidade_media_med = mean(nec_med_realoc),
              oferta_media_med = mean(oferta_med)) |> 
    mutate(rr_med = 100 * (oferta_media_med/necessidade_media_med)) |> 
    mutate(ra_med = (oferta_media_med - necessidade_media_med)) 
  
  med_enf_oferta_demanda <- 
    oferta_vs_demanda_med_anual |> 
    left_join(oferta_vs_demanda_enf_anual,
              by = c("cod_regsaud",
                     "uf_sigla",
                     "ano")) |> 
    mutate(classificacao = case_when(
      rr_med >= 100 & 
        rr_enf >= 100 ~ "Ambos superávit",
      rr_med >= 100 &
        rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
      rr_enf >= 100 &
        rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
      rr_med <= 100 & 
        rr_enf <= 100 ~ "Ambos déficit")) |> 
    mutate(Região = case_when(
      uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
      uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
      uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
      uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      TRUE ~ "Não classificado"
    ))
  
    return(med_enf_oferta_demanda)
}


# Testando a função -------------------------------------------------------

c1 <- oferta_vs_demanda(acoes_hab = 45, 
                           prenatal_hab = 30, 
                           indireta_enf = 0.50,
                           indireta_med = 0.50, 
                           todos = 1, 
                           absenteismo = 16, 
                           prenatal_alto = 35, 
                           alto_risco = 15, 
                           acoes_alto = 50, 
                           ferias = 16, 
                           feriados = 4, 
                           imunizacao = 15, 
                           coleta_exames = 15,
                           visita = 60, 
                           consulta_puerperal = 25, 
                           consulta_cd = 25, 
                           enf_coleta_exames = 0.75, 
                           enf_coleta_cito = 0.25, 
                           enf_prenatal = 0.50,
                           enf_imunizacao = 0.75, 
                           enf_puerperal = 0.50, 
                           enf_visita = 0.50, 
                           enf_cd = 0.50, 
                           enf_acoes = 0.50,
                           ist_enf = 0.10,
                           ist_med = 0.10,
                           tipo_ans = "aumento", 
                           cenario_oferta = "cenario_base") |> 
            mutate(cenario = "Cenário 1")



c2 <- oferta_vs_demanda(acoes_hab = 45, 
                        prenatal_hab = 30, 
                        indireta_enf = 0.50,
                        indireta_med = 0.50, 
                        todos = 1, 
                        absenteismo = 16, 
                        prenatal_alto = 35, 
                        alto_risco = 15, 
                        acoes_alto = 50, 
                        ferias = 16, 
                        feriados = 4, 
                        imunizacao = 15, 
                        coleta_exames = 15,
                        visita = 60, 
                        consulta_puerperal = 25, 
                        consulta_cd = 25, 
                        enf_coleta_exames = 0.75, 
                        enf_coleta_cito = 0.25, 
                        enf_prenatal = 0.50,
                        enf_imunizacao = 0.75, 
                        enf_puerperal = 0.50, 
                        enf_visita = 0.50, 
                        enf_cd = 0.50, 
                        enf_acoes = 0.50,
                        ist_enf = 0.10,
                        ist_med = 0.10,
                        tipo_ans = "aumento", 
                        cenario_oferta = "cenario_reducao10") |> 
  mutate(cenario = "Cenário 2")


c3 <- oferta_vs_demanda(acoes_hab = 45, 
                        prenatal_hab = 30, 
                        indireta_enf = 0.50,
                        indireta_med = 0.50, 
                        todos = 1, 
                        absenteismo = 16, 
                        prenatal_alto = 35, 
                        alto_risco = 15, 
                        acoes_alto = 50, 
                        ferias = 16, 
                        feriados = 4, 
                        imunizacao = 15, 
                        coleta_exames = 15,
                        visita = 60, 
                        consulta_puerperal = 25, 
                        consulta_cd = 25, 
                        enf_coleta_exames = 0.75, 
                        enf_coleta_cito = 0.25, 
                        enf_prenatal = 0.50,
                        enf_imunizacao = 0.75, 
                        enf_puerperal = 0.50, 
                        enf_visita = 0.50, 
                        enf_cd = 0.50, 
                        enf_acoes = 0.50,
                        ist_enf = 0.10,
                        ist_med = 0.10,
                        tipo_ans = "constante", 
                        cenario_oferta = "cenario_reducao10") |> 
  mutate(cenario = "Cenário 3")

c4 <- oferta_vs_demanda(acoes_hab = 45, 
                        prenatal_hab = 30, 
                        indireta_enf = 0.50,
                        indireta_med = 0.50, 
                        todos = 1, 
                        absenteismo = 16, 
                        prenatal_alto = 35, 
                        alto_risco = 15, 
                        acoes_alto = 50, 
                        ferias = 16, 
                        feriados = 4, 
                        imunizacao = 15, 
                        coleta_exames = 15,
                        visita = 60, 
                        consulta_puerperal = 25, 
                        consulta_cd = 25, 
                        enf_coleta_exames = 0.75, 
                        enf_coleta_cito = 0.25, 
                        enf_prenatal = 0.50,
                        enf_imunizacao = 0.75, 
                        enf_puerperal = 0.50, 
                        enf_visita = 0.50, 
                        enf_cd = 0.50, 
                        enf_acoes = 0.50,
                        ist_enf = 0.10,
                        ist_med = 0.10,
                        tipo_ans = "diminuicao", 
                        cenario_oferta = "cenario_reducao20") |> 
  mutate(cenario = "Cenário 4")


# Região ----------------------------------------------------------------

cenarios <- rbind(c1, c2, c3, c4) 

cenarios <- cenarios |> 
             filter(cod_regsaud == "41011")

a <- cenarios |>
  rename(Cenário = cenario) |> 
  mutate(rr_med = round(rr_med)) |> 
  ggplot(aes(x = ano, y = rr_med,
             col = Cenário)) + 
  geom_line() +
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = rr_med),
                           col = "black",
             size = 3) +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 130, by = 10)) +
  theme_minimal() + 
  xlab("Ano") + 
  ylab("Percentual") +
  ggtitle("Região de Saúde 11ª RS CAMPO MOURAO - RS","Médicos") 
  
b <- cenarios |>
  rename(Cenário = cenario) |> 
  mutate(rr_enf = round(rr_enf)) |> 
  ggplot(aes(x = ano, y = rr_enf,
             col = Cenário)) + 
  geom_line() +
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = rr_enf),
                           col = "black",
                           size = 3) +
  scale_y_continuous(limits = c(0, 130), 
                     breaks = seq(0, 130, by = 10)) +
  theme_minimal() + 
  xlab("Ano") + 
  ylab("Percentual") +
  ggtitle("","Enfermeiros") + 
  theme(legend.position = "none")


cenario_enf_med <- a / b

ggsave(plot = cenario_enf_med,
       filename = "~/GitHub/materno_infantil/02_script_tratado/06_figuras/evolucao_cenarios.jpeg",
       dpi = 600, width = 8, height = 5)


# automatizando em uma função ---------------------------------------------


gerar_projecoes <- function(reg){
  
  cenarios <- rbind(c1, c2, c3, c4) |> 
    filter(cod_regsaud == reg)
  
  max_med <- max(cenarios$rr_med)
  regiao <- unique(cenarios$regiao_saude)
  uf <- unique(cenarios$uf_sigla)
  max_enf <- max(cenarios$rr_enf)
  
  a <- cenarios |>
    rename(Cenário = cenario) |> 
    mutate(rr_med = round(rr_med)) |> 
    ggplot(aes(x = ano, y = rr_med,
               col = Cenário)) + 
    geom_line() +
    geom_point() + 
    ggrepel::geom_text_repel(aes(label = rr_med),
                             col = "black",
                             size = 3) +
    scale_y_continuous(limits = c(0, 1.30 * max_med), 
                       breaks = seq(0, 1.30 * max_med, by = 10)) +
    theme_minimal() + 
    xlab("Ano") + 
    ylab("Percentual") +
    ggtitle(paste0("Região de Saúde ",
                   regiao,
                   " - ",
                   uf),"Médicos") 
  
  b <- cenarios |>
    rename(Cenário = cenario) |> 
    mutate(rr_enf = round(rr_enf)) |> 
    ggplot(aes(x = ano, y = rr_enf,
               col = Cenário)) + 
    geom_line() +
    geom_point() + 
    ggrepel::geom_text_repel(aes(label = rr_enf),
                             col = "black",
                             size = 3) +
    scale_y_continuous(limits = c(0, 1.30 * max_enf), 
                       breaks = seq(0, 1.30 *max_enf, by = 10)) +
    theme_minimal() + 
    xlab("Ano") + 
    ylab("Percentual") +
    ggtitle("","Enfermeiros") + 
    theme(legend.position = "none")
  
  
  cenario_enf_med <- a / b
  
  ggsave(plot = cenario_enf_med,
         filename = paste0("~/GitHub/materno_infantil/02_script_tratado/06_figuras/evolucao_cenarios_",reg,".jpeg"),
         dpi = 600, width = 8, height = 5)
  
  
}

gerar_projecoes("52001")

codigos <- unique(c1$cod_regsaud)

for(reg in codigos){
  cat("Processando região:", reg, "\n")
  try({
    gerar_projecoes(reg)
  }, silent = TRUE)
}
