
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

lista_proc <- servicos |> 
                distinct(codigo_sigtap, procedimento, 
                         tipo_procedimento, Público,
                         nivel_atencao) 

oferta_medicos <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_23.csv") |> 
  select(-`...1`) |> 
  janitor::clean_names() |> 
  mutate(mes = as.numeric(mes)) |> 
  select(-uf_sigla) |> 
  filter(categoria_profissional == "Médicos")

oferta_enfermeiros_bruto <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_23.csv") |> 
  select(-`...1`) |> 
  janitor::clean_names() |> 
  mutate(mes = as.numeric(mes)) |> 
  select(-uf_sigla) |> 
  filter(categoria_profissional == "Enfermeiros")

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) |> 
  select(-uf_sigla, -regiao_saude)

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc)


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

# Aplicando sem função ----------------------------------------------------

# Filtrar procedimentos de 2023

servicos23 <- 
  servicos |> 
  rename(mes_proc_rea = mês_procedimento_realizado,
         publico = Público,
         mes_programado = mes,
         qtd_nascidos = qtd,
         qtd_proc = quantidade) |> 
  mutate(ano_proc_rea = year(mes_proc_rea)) |> 
  filter(ano_proc_rea == 2023) |> 
  filter(nivel_atencao == "APS" & 
         publico != "Gestantes de Alto Risco") |> 
  filter(codigo_sigtap %in% c("0301010110","0202","0203",
                             "0101010010","0301010129",
                             "1234","0301010080",
                             "0301010137")) |> 
  filter(!(procedimento == "Visita domiciliar" & mes_programado != -7))


cobertura$cod_regsaud <- as.character(cobertura$cod_regsaud)

lista <- servicos23 |> 
            distinct(procedimento, mes_programado, tipo_procedimento,
                     codigo_sigtap)
  

# Juntar dados de serviços com cobertura

servicos23_tratado <- 
  servicos23 |> 
  left_join(cobertura, 
            by = c("cod_regsaud"=
                   "cod_regsaud")) |> 
  janitor::clean_names() |> 
  mutate(cobertura = cobertura/100) |> 
  rename(cobertura_ans = cobertura) 

# write.csv(servicos23_tratado,
#           "~/GitHub/materno_infantil/02_script_debug/01_dfs/servicos23_tratado.csv")

# se todos = 1, pegar todos 
# se todos = 0, pegar apenas SUS dependente

sus <- 1
todos <- 0

if(sus == todos){
  
  servicos23_procedimentos <- 
    
    servicos23_tratado |> 
    
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
  
  servicos23_procedimentos <- 
  
    servicos23_tratado |>
    
    mutate(qtd_proc = (qtd_nascidos - 
                      (qtd_nascidos * cobertura_ans)) * 
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

servicos23_procedimentos <- 
  servicos23_procedimentos |> 
  select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaud, 
         regiao_saude, qtd_nascidos, cobertura_ans, codigo_sigtap,
         procedimento, tipo_procedimento, mes_programado,
         publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
         qtd_proc_ar)

# Traduzir número de horas em número de profissionais necessários

ttd <- 160 - ferias - feriados - absenteismo

necessidade <-
  servicos23_procedimentos |> 
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
  mutate(nec_med = nec_prof - nec_enf)
  
nec_prof <- 
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
# da medicina em relação a enfermagem

nec_enf <- nec_prof |> 
                ungroup() |> 
                select(ano, 
                       mes,
                       uf_sigla,
                       cod_regsaud,
                       nec_enf_prof) 


oferta_enfermeiros <- 
  oferta_enfermeiros_bruto |> 
  left_join(foco_clinico, 
            by = c("cod_regsaud"="cod_regsaud")) |> 
  mutate(oferta_enf = fte40 * (1 - indireta) * perc_fc)

nec_vs_oferta_enfermagem <- 
  nec_enf |> 
  left_join(oferta_enfermeiros, 
            by = c("cod_regsaud"=
                   "cod_regsaud",
                   "mes"="mes",
                   "ano"="ano")) |> 
  mutate(ra_enf = oferta_enf - nec_enf_prof) |> 
  mutate(rr_enf = oferta_enf/nec_enf_prof) |> 
  mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                    rr_enf >= 0.50 & 
                                    rr_enf <= 1 ~ "C50",
                                    rr_enf < 0.50 ~ "C")) |> 
  ungroup()

nec_vs_oferta_enfermagem <- 
  nec_vs_oferta_enfermagem |> 
  select(cod_regsaud, uf_sigla, mes, 
         ano, nec_enf_prof, 
         oferta_enf, ra_enf, rr_enf, equilibrio_enf)


oferta_vs_demanda_enf_anual <- 
  nec_vs_oferta_enfermagem |> 
  group_by(uf_sigla, cod_regsaud) |> 
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
    left_join(oferta_medicos, 
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico, 
            by = c("cod_regsaud"="cod_regsaud")) |> 
    mutate(oferta_med = fte40 * (1 - indireta) * perc_fc) |> 
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
    group_by(uf_sigla, cod_regsaud, regiao_saude) |> 
    summarise(necessidade_media = mean(nec_med_realoc),
              oferta_media = mean(oferta_med)) |> 
    mutate(rr_med = 100 * (oferta_media/necessidade_media)) |> 
    mutate(ra_med = (oferta_media - necessidade_media)) 

med_enf_oferta_demanda <- 
  oferta_vs_demanda_med_anual |> 
  left_join(oferta_vs_demanda_enf_anual,
             by = c("cod_regsaud","uf_sigla")) |> 
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


dist_uf <- med_enf_oferta_demanda |> 
  group_by(uf_sigla, classificacao) |> 
  count()

dist_uf |> 
  ggplot(aes(x = uf_sigla, y = n, fill = classificacao)) + 
  geom_col(position = "fill") +  coord_flip() + 
  xlab("UF") +
  theme_minimal() + 
  theme(legend.position = "bottom") 

med_enf_oferta_demanda  |> 
  ggplot(aes(x = rr_enf, y = rr_med,
             col = Região)) + 
  geom_point() + 
  geom_hline(yintercept = 100, 
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 100, 
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~Região)  + 
  theme_bw() +
  xlab("Enfermeiros - RR(%)") + 
  ylab("Médicos - RR(%)") +
  xlim(0,400) + ylim(0,400)


# Criando uma função ------------------------------------------------------

# Partimos de alguns dados já tratados

servicos23_tratado <- 
  read_csv("02_script_debug/01_dfs/servicos23_tratado.csv") |> 
  select(-`...1`)


oferta_vs_demanda <- function(acoes_hab, prenatal_hab, indireta_enf,
                              indireta_med, todos, absenteismo, 
                              prenatal_alto, alto_risco, acoes_alto, 
                              ferias, feriados, imunizacao, coleta_exames,
                              visita, consulta_puerperal, consulta_cd, 
                              enf_coleta_exames, enf_coleta_cito, enf_prenatal,
                              enf_imunizacao, enf_puerperal, enf_visita, enf_cd, 
                              enf_acoes){
  

  
  sus <- 1
  
  if(sus == todos){
    
    servicos23_procedimentos <- 
      
      servicos23_tratado |> 
      
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
    
    servicos23_procedimentos <- 
      
      servicos23_tratado |>
      
      mutate(qtd_proc = (qtd_nascidos - 
                           (qtd_nascidos * cobertura_ans)) * 
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
  
  servicos23_procedimentos <- 
    servicos23_procedimentos |> 
    select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaud, 
           regiao_saude, qtd_nascidos, cobertura_ans, codigo_sigtap,
           procedimento, tipo_procedimento, mes_programado,
           publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
           qtd_proc_ar)
  
  # Traduzir número de horas em número de profissionais necessários
  
  ttd <- 160 - ferias - feriados - absenteismo
  
  necessidade <-
    servicos23_procedimentos |> 
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
    mutate(nec_med = nec_prof - nec_enf)
  
  nec_prof <- 
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
  
  nec_enf <- nec_prof |> 
    ungroup() |> 
    select(ano, 
           mes,
           uf_sigla,
           cod_regsaud,
           nec_enf_prof) 
  
  
  oferta_enfermeiros <- 
    oferta_enfermeiros_bruto |> 
    left_join(foco_clinico, 
              by = c("cod_regsaud"="cod_regsaud")) |> 
    mutate(oferta_enf = fte40 * (1 - indireta_enf) * perc_fc)
  
  nec_vs_oferta_enfermagem <- 
    nec_enf |> 
    left_join(oferta_enfermeiros, 
              by = c("cod_regsaud"=
                       "cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    mutate(ra_enf = oferta_enf - nec_enf_prof) |> 
    mutate(rr_enf = oferta_enf/nec_enf_prof) |> 
    mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                      rr_enf >= 0.50 & 
                                        rr_enf <= 1 ~ "C50",
                                      rr_enf < 0.50 ~ "C")) |> 
    ungroup()
  
  nec_vs_oferta_enfermagem <- 
    nec_vs_oferta_enfermagem |> 
    select(cod_regsaud, uf_sigla, mes, 
           ano, nec_enf_prof, 
           oferta_enf, ra_enf, rr_enf, equilibrio_enf)
  
  
  oferta_vs_demanda_enf_anual <- 
    nec_vs_oferta_enfermagem |> 
    group_by(uf_sigla, cod_regsaud) |> 
    summarise(necessidade_media_enf = mean(nec_enf_prof),
              oferta_media_enf = mean(oferta_enf)) |> 
    mutate(rr_enf = 100 * (oferta_media_enf/necessidade_media_enf)) |> 
    mutate(ra_enf = (oferta_media_enf - necessidade_media_enf)) 
  
  # voltando para a oferta 
  # vamos adicionar a logica de que - caso de faltar profissional de 
  # enfermagem, a carga de trabalho deverá ser jogada para o médico
  # fizemos desta forma, pois a enfermagem tem maiores % nos procedimentos
  # em média
  # Juntar necessidades com oferta
  
  oferta_vs_demanda_med <- 
    nec_prof |> 
    left_join(oferta_medicos, 
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico, 
              by = c("cod_regsaud"="cod_regsaud")) |> 
    mutate(oferta_med = fte40 * (1 - indireta_med) * perc_fc) |> 
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
    group_by(uf_sigla, cod_regsaud, regiao_saude) |> 
    summarise(necessidade_media_med = mean(nec_med_realoc),
              oferta_media_med = mean(oferta_med)) |> 
    mutate(rr_med = 100 * (oferta_media_med/necessidade_media_med)) |> 
    mutate(ra_med = (oferta_media_med - necessidade_media_med)) 
  
  med_enf_oferta_demanda <- 
    oferta_vs_demanda_med_anual |> 
    left_join(oferta_vs_demanda_enf_anual,
              by = c("cod_regsaud","uf_sigla")) |> 
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
  
  
  dist_uf <- med_enf_oferta_demanda |> 
    group_by(uf_sigla, classificacao) |> 
    count()
  
a <- dist_uf |> 
    ggplot(aes(x = uf_sigla, y = n, fill = classificacao)) + 
    geom_col(position = "fill") +  coord_flip() + 
    xlab("UF") +
    theme_minimal() + 
    theme(legend.position = "bottom") 
  
b <- med_enf_oferta_demanda  |> 
    ggplot(aes(x = rr_enf, y = rr_med,
               col = Região)) + 
    geom_point() + 
    geom_hline(yintercept = 100, 
               linetype = "dashed",
               color = "red") +
    geom_vline(xintercept = 100, 
               linetype = "dashed",
               color = "red") +
    geom_smooth(method = 'lm', se = FALSE) + 
    facet_wrap(~Região)  + 
    theme_bw() +
    xlab("Enfermeiros - RR(%)") + 
    ylab("Médicos - RR(%)") +
    xlim(0,250) + ylim(0,250)
  
    resultados <- list(med_enf_oferta_demanda, 
                       a, 
                       b)
  
    return(resultados)
  
}


# Testando a função -------------------------------------------------------

c1 <- oferta_vs_demanda(acoes_hab = 45, 
                           prenatal_hab = 30, 
                           indireta_enf = 0.50,
                           indireta_med = 0.60, 
                           todos = 1, 
                           absenteismo = 16, 
                           prenatal_alto = 35, 
                           alto_risco = 15, 
                           acoes_alto = 50, 
                           ferias = 16, 
                           feriados = 4, 
                           imunizacao = 20, 
                           coleta_exames = 15,
                           visita = 60, 
                           consulta_puerperal = 30, 
                           consulta_cd = 30, 
                           enf_coleta_exames = 0.75, 
                           enf_coleta_cito = 0.25, 
                           enf_prenatal = 0.50,
                           enf_imunizacao = 0.75, 
                           enf_puerperal = 0.50, 
                           enf_visita = 0.50, 
                           enf_cd = 0.50, 
                           enf_acoes = 0.50)

c1[[3]] + ggtitle("c1")

c2 <- oferta_vs_demanda(acoes_hab = 45, 
                        prenatal_hab = 30, 
                        indireta_enf = 0.50,
                        indireta_med = 0.60, 
                        todos = 0, 
                        absenteismo = 16, 
                        prenatal_alto = 35, 
                        alto_risco = 15, 
                        acoes_alto = 50, 
                        ferias = 16, 
                        feriados = 4, 
                        imunizacao = 20, 
                        coleta_exames = 15,
                        visita = 60, 
                        consulta_puerperal = 30, 
                        consulta_cd = 30, 
                        enf_coleta_exames = 0.75, 
                        enf_coleta_cito = 0.25, 
                        enf_prenatal = 0.50,
                        enf_imunizacao = 0.75, 
                        enf_puerperal = 0.50, 
                        enf_visita = 0.50, 
                        enf_cd = 0.50, 
                        enf_acoes = 0.50)


c2[[3]] + ggtitle("c2")



# Criando cenários ---------------------------------------------------------

cenario1 <- oferta_vs_demanda(acoes_educacionais = 30,
                              alto_risco = 0.20,
                              acoes_altorisco = 40,
                              consultas = 30, 
                              absenteismo = 24, 
                              indireta = 0.60,
                              todos = 1, # pegando todos - não só os sus dependentes
                              consultas_altorisco = 40,
                              participacao_medico = 0.60)

d1 <- cenario1$dados |> mutate(cenario = "Cenário 1")
gd1 <- cenario1$grafico_dispersao + ggtitle("Cenário 1") + 
                                    theme(axis.title.x = element_blank())
gb1 <- cenario1$grafico_barras + ggtitle("Cenário 1") + 
                                 theme(legend.position = "none")


cenario2 <- oferta_vs_demanda(acoes_educacionais = 30,
                            alto_risco = 0.20,
                            acoes_altorisco = 40,
                            consultas = 30, 
                            absenteismo = 24, 
                            indireta = 0.60,
                            todos = 0,
                            consultas_altorisco = 40,
                            participacao_medico = 0.60) 

d2 <- cenario2$dados |> mutate(cenario = "Cenário 2")
gd2 <- cenario2$grafico_dispersao + ggtitle("Cenário 2") + 
                                    theme(axis.title.x  = element_blank(),
                                          axis.title.y = element_blank())
gb2 <- cenario2$grafico_barras + ggtitle("Cenário 2") + 
                                 theme(legend.position = "none",)

gd2

cenario3 <- oferta_vs_demanda(acoes_educacionais = 30,
                              alto_risco = 0.15,
                              acoes_altorisco = 40,
                              consultas = 30, 
                              absenteismo = 16, 
                              indireta = 0.50,
                              todos = 0,
                              consultas_altorisco = 35,
                              participacao_medico = 0.40) 

d3 <- cenario3$dados |> mutate(cenario = "Cenário 3")
gd3 <- cenario3$grafico_dispersao + ggtitle("Cenário 3") 
gb3 <- cenario3$grafico_barras + ggtitle("Cenário 3") + 
                                 theme(legend.position = "none")  


cenario4 <- oferta_vs_demanda(acoes_educacionais = 25,
                              alto_risco = 0.15,
                              acoes_altorisco = 35,
                              consultas = 25, 
                              absenteismo = 16, 
                              indireta = 0.50,
                              todos = 0,
                              consultas_altorisco = 35,
                              participacao_medico = 0.40) 

d4 <- cenario4$dados |> mutate(cenario = "Cenário 4")
gd4 <- cenario4$grafico_dispersao + ggtitle("Cenário 4") + 
                                    theme(axis.text.y = element_blank())

gb4 <- cenario4$grafico_barras + ggtitle("Cenário 4") + 
                                 theme(legend.position = "right")  


cenarios <- rbind(d1, d2, d3, d4)

graficos_dispersao <- (gd1 | gd2) /  
                      (gd3 | gd4)

graficos_barras <- (gb1 | gb2) /  
                   (gb3 | gb4)


graficos_dispersao

# ggsave(plot = graficos_dispersao,
#         filename = "~/GitHub/materno_infantil/02_script/08_output_gráficos/dispersao23.jpeg",
#         dpi = 500, width = 10, height = 8)
#  
# ggsave(plot = graficos_barras,
#         filename = "~/GitHub/materno_infantil/02_script/08_output_gráficos/barras23.jpeg",
#         dpi = 500, width = 10, height = 8)
# 
# write.csv(cenarios, 
#               "~/GitHub/materno_infantil/02_script/08_output_gráficos/cenarios_selecionados23.csv")
#  
# salvando cenarios -------------------------------------------------------



cenarios_comparados <- cenario1 |> 
                          left_join(cenario2, by = "cod_regsaude") |> 
                          left_join(cenario3, by = "cod_regsaude") |>
                          left_join(cenario4, by = "cod_regsaude") |>
                          left_join(cenario5, by = "cod_regsaude") |> 
                          mutate(comp12 = perc.y - perc.x) |> 
                          mutate(comp23 = perc.x.x - perc.y) |> 
                          mutate(comp34 = perc.y.y - perc.x.x) |> 
                          mutate(comp45 = perc - perc.y.y)

mean(cenarios_comparados$comp12)
mean(cenarios_comparados$comp23)
mean(cenarios_comparados$comp34)
mean(cenarios_comparados$comp45)
