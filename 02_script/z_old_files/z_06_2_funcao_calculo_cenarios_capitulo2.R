
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

# Leitura dos dados

servicos <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script/04_servicos/servicos23.csv") |> 
  select(-`...1`) 

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
  select(-`...1`) 

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc)


acoes_educacionais <- 30
consultas <- 30
ttd <- 133
indireta <- 0.40
todos <- 1
ferias <- 16
feriados <- 4
absenteismo <- 16
consultas_altorisco <- 45
alto_risco <- 0.15
acoes_altorisco <- 50
participacao_medico <- 0.50
ferias <- 16
feriados <- 4

# Aplicando sem função ----------------------------------------------------

# Filtrar procedimentos de 2023

servicos23 <- 
  servicos |> 
  mutate(ano_proc_rea = 
           year(mes_proc_rea)) |> 
  filter(ano_proc_rea == 2023) |> 
  filter(nivel_atencao == "APS" & 
         publico != "Gestantes de Alto Risco") |>
  filter(tipo_procedimento == "Ações Educacionais" |
         tipo_procedimento == "Consultas ou Visitas") |> 
  filter(procedimento != "Avaliação odontológica") |> 
  filter(procedimento != "Visita domiciliar") |> 
  filter(mes_programado < 36)

cobertura$cod_regsaud <- as.numeric(cobertura$cod_regsaud)

# Juntar dados de serviços com cobertura

servicos23_tratado <- 
  servicos23 |> 
  left_join(cobertura, 
            by = c("cod_regsaude"=
                   "cod_regsaud")) |> 
  janitor::clean_names() |> 
  mutate(cobertura = cobertura/100) |> 
  rename(cobertura_ans = cobertura) 

# se todos = 1, pegar todos 
# se todos = 0, pegar apenas SUS dependente

sus <- 1
todos <- 0

if(sus == todos){
  servicos23_tratado <- 
    
    servicos23_tratado |> 
    
    mutate(qtd_proc_rh = 
             case_when(
               procedimento == "Consulta pré-natal" ~ 
                 qtd_proc * (1-alto_risco),
               procedimento == "Ações Educacionais" ~
                 qtd_proc * (1-alto_risco),
               TRUE ~ qtd_proc
             )) |> 
    mutate(qtd_proc_ar = 
             case_when(
               procedimento == "Consulta pré-natal" ~ 
                 qtd_proc * (alto_risco),
               procedimento == "Ações Educacionais" ~
                 qtd_proc * (alto_risco),
               TRUE ~ 0
             ) 
           
    )
} else {
  
  servicos23_tratado <- 
  
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
               TRUE ~ qtd_proc
             )) |> 
    mutate(qtd_proc_ar = 
             case_when(
               procedimento == "Consulta pré-natal" ~ 
                 qtd_proc * (alto_risco),
               procedimento == "Ações Educacionais" ~
                 qtd_proc * (alto_risco),
               TRUE ~ 0
             )) 
}

servicos_tratado <- 
  servicos23_tratado |> 
  select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaude, 
         regiao_saude, qtd_nascidos, cobertura_ans, sigtap_recod,
         procedimento, tipo_procedimento, mes_programado,
         publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
         qtd_proc_ar)

# Traduzir número de horas em número de profissionais necessários

ttd <- 160 - ferias - feriados - absenteismo

necessidade <-
  servicos_tratado |> 
  mutate(total_horas_rh = 
           case_when(tipo_procedimento == 
                       "Ações Educacionais" ~ 
                       qtd_proc_rh * acoes_educacionais/60,
                     tipo_procedimento == 
                       "Consultas ou Visitas" ~ 
                       qtd_proc_rh * consultas/60)) |> 
  mutate(total_horas_ar = 
           case_when(tipo_procedimento == 
                       "Ações Educacionais" ~ 
                       qtd_proc_ar * acoes_altorisco/60,
                     tipo_procedimento == 
                       "Consultas ou Visitas" ~ 
                       qtd_proc_ar * consultas_altorisco/60)) |> 
  mutate(nec_prof = (total_horas_rh + total_horas_ar)/ttd) |> 
  mutate(nec_cat = nec_prof * participacao_medico)

necessidade_tratada <- 
  necessidade |> 
  group_by(uf_sigla, cod_regsaude, regiao_saude,  
           mes_proc_rea) |> 
  summarise(nec_prof = sum(nec_prof),
            nec_cat = sum(nec_cat),
            nec_ch = sum(total_horas_rh + total_horas_ar)) |> 
  mutate(mes = month(mes_proc_rea),
         ano = year(mes_proc_rea), 
         .after = mes_proc_rea)

oferta_enfermeiros <- 
  oferta_enfermeiros_bruto |> 
  left_join(foco_clinico, 
            by = c("cod_regsaud"="cod_regsaud")) |> 
  mutate(oferta_enf = fte40 * (1 - indireta) * perc_fc)

nec_vs_oferta_enfermagem <- 
  necessidade_tratada |> 
  left_join(oferta_enfermeiros, 
            by = c("cod_regsaude"=
                   "cod_regsaud",
                   "mes"="mes",
                   "ano"="ano")) |> 
  mutate(ra_enf = oferta_enf - nec_cat) |> 
  mutate(rr_enf = oferta_enf/nec_cat) |> 
  mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                    rr_enf >= 0.50 & 
                                    rr_enf <= 1 ~ "C50",
                                    rr_enf < 0.50 ~ "C")) |> 
  ungroup()

nec_vs_oferta_enfermagem <- 
  nec_vs_oferta_enfermagem |> 
  select(cod_regsaude, uf_sigla, mes, 
         ano, oferta_enf, rr_enf, equilibrio_enf)

nec_vs_oferta_enfermagem_anual <- 
  necessidade_tratada |> 
  left_join(oferta_enfermeiros, 
            by = c("cod_regsaude"=
                     "cod_regsaud",
                   "mes"="mes",
                   "ano"="ano")) |> 
  group_by(uf_sigla, cod_regsaude, ano) |> 
  summarise(nec_enf = mean(nec_prof),
            oferta_enf = mean(oferta_enf)) |>
  mutate(rr_enf = oferta_enf/nec_enf)
  

# Juntar necessidades com oferta

oferta_vs_demanda_med <- 
  necessidade_tratada |> 
    left_join(oferta_medicos, 
              by = c("cod_regsaude"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico, 
            by = c("cod_regsaude"="cod_regsaud")) |> 
    mutate(oferta_direta =  fte40 * (1 - indireta),
           oferta_med = oferta_direta * perc_fc) |> 
    left_join(nec_vs_oferta_enfermagem,
              by = c("cod_regsaude"="cod_regsaude",
                     "mes"="mes",
                     "ano"="ano",
                     "uf_sigla"="uf_sigla")) |>
    mutate(nec_medicos = 
             case_when(equilibrio_enf == "NC" ~ nec_prof * participacao_medico,
                       equilibrio_enf == "C50" ~ nec_prof * ((1 - rr_enf) + participacao_medico),
                       equilibrio_enf == "C" ~ nec_prof * (1 - rr_enf))) |> 
    mutate(total_abs = oferta_med - nec_medicos,
           total_perc = 100 * (oferta_med/nec_medicos)) 

oferta_vs_demanda_med_anual <- 
    oferta_vs_demanda_med |> 
    group_by(uf_sigla, cod_regsaude, regiao_saude) |> 
    summarise(necessidade_media = mean(nec_medicos),
              oferta_media = mean(oferta_med)) |> 
    mutate(rr_med = 100 * (oferta_media/necessidade_media)) |> 
    mutate(ra_med = (oferta_media - necessidade_media)) 

med_enf_oferta_demanda <- 
  oferta_vs_demanda_med_anual |> 
  left_join(nec_vs_oferta_enfermagem_anual,
             by = c("cod_regsaude","uf_sigla")) |> 
  mutate(rr_enf = 100 * rr_enf) |> 
  mutate(classificacao = case_when(
                         rr_med >= 100 & 
                         rr_enf >= 100 ~ "Ambos superávit",
                         rr_med >= 100 &
                         rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
                         rr_enf >= 100 &
                         rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
                         rr_med <= 100 & 
                         rr_enf <= 100 ~ "Ambos déficit"))  


dist_uf <- med_enf_oferta_demanda |> 
  group_by(uf_sigla, classificacao) |> 
  count()

dist_uf |> 
  ggplot(aes(x = uf_sigla, y = n, fill = classificacao)) + 
  geom_col(position = "fill") +  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = "bottom") 

med_enf_oferta_demanda |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  )) |> 
  filter(rr_enf < 100) |> 
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
  xlim(0,200) + ylim(0,200)


# criando uma função ------------------------------------------------------


oferta_vs_demanda <- 
  
  function(acoes_educacionais,
           consultas, 
           alto_risco,
           indireta, 
           todos, 
           acoes_altorisco, 
           consultas_altorisco,
           absenteismo,
           participacao_medico){
    
    
    servicos23 <- 
      servicos |> 
      mutate(ano_proc_rea = 
               year(mes_proc_rea)) |> 
      filter(ano_proc_rea == 2023) |> 
      filter(nivel_atencao == "APS" & 
               publico != "Gestantes de Alto Risco") |>
      filter(tipo_procedimento == "Ações Educacionais" |
               tipo_procedimento == "Consultas ou Visitas") |> 
      filter(procedimento != "Avaliação odontológica") |> 
      filter(procedimento != "Visita domiciliar") |> 
      filter(mes_programado < 36)
    
    cobertura$cod_regsaud <- as.numeric(cobertura$cod_regsaud)
    
    # Juntar dados de serviços com cobertura
    
    servicos23_tratado <- 
      servicos23 |> 
      left_join(cobertura, 
                by = c("cod_regsaude"=
                         "cod_regsaud")) |> 
      janitor::clean_names() |> 
      mutate(cobertura = cobertura/100) |> 
      rename(cobertura_ans = cobertura) 
    
    # se todos = 1, pegar todos 
    # se todos = 0, pegar apenas SUS dependente
    
    sus = 1
    
    if(sus == todos){
      servicos23_tratado <- 
        
        servicos23_tratado |> 
        
        mutate(qtd_proc_rh = 
                 case_when(
                   procedimento == "Consulta pré-natal" ~ 
                     qtd_proc * (1-alto_risco),
                   procedimento == "Ações Educacionais" ~
                     qtd_proc * (1-alto_risco),
                   TRUE ~ qtd_proc
                 )) |> 
        mutate(qtd_proc_ar = 
                 case_when(
                   procedimento == "Consulta pré-natal" ~ 
                     qtd_proc * (alto_risco),
                   procedimento == "Ações Educacionais" ~
                     qtd_proc * (alto_risco),
                   TRUE ~ 0
                 ) 
               
        )
    } else {
      
      servicos23_tratado <- 
        
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
                   TRUE ~ qtd_proc
                 )) |> 
        mutate(qtd_proc_ar = 
                 case_when(
                   procedimento == "Consulta pré-natal" ~ 
                     qtd_proc * (alto_risco),
                   procedimento == "Ações Educacionais" ~
                     qtd_proc * (alto_risco),
                   TRUE ~ 0
                 )) 
    }
    
    servicos_tratado <- 
      servicos23_tratado |> 
      select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaude, 
             regiao_saude, qtd_nascidos, cobertura_ans, sigtap_recod,
             procedimento, tipo_procedimento, mes_programado,
             publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
             qtd_proc_ar)
    
    # Traduzir número de horas em número de profissionais necessários
    
    ttd <- 160 - ferias - feriados - absenteismo
    
    necessidade <-
      servicos_tratado |> 
      mutate(total_horas_rh = 
               case_when(tipo_procedimento == 
                           "Ações Educacionais" ~ 
                           qtd_proc_rh * acoes_educacionais/60,
                         tipo_procedimento == 
                           "Consultas ou Visitas" ~ 
                           qtd_proc_rh * consultas/60)) |> 
      mutate(total_horas_ar = 
               case_when(tipo_procedimento == 
                           "Ações Educacionais" ~ 
                           qtd_proc_ar * acoes_altorisco/60,
                         tipo_procedimento == 
                           "Consultas ou Visitas" ~ 
                           qtd_proc_ar * consultas_altorisco/60)) |> 
      mutate(nec_prof = (total_horas_rh + total_horas_ar)/ttd) |> 
      mutate(nec_cat = nec_prof * participacao_medico)
    
    necessidade_tratada <- 
      necessidade |> 
      group_by(uf_sigla, cod_regsaude, regiao_saude,  
               mes_proc_rea) |> 
      summarise(nec_prof = sum(nec_prof),
                nec_cat = sum(nec_cat),
                nec_ch = sum(total_horas_rh + total_horas_ar)) |> 
      mutate(mes = month(mes_proc_rea),
             ano = year(mes_proc_rea), 
             .after = mes_proc_rea)
    
    oferta_enfermeiros <- 
      oferta_enfermeiros_bruto |> 
      left_join(foco_clinico, 
                by = c("cod_regsaud"="cod_regsaud")) |> 
      mutate(oferta_enf = fte40 * (1 - indireta) * perc_fc)
    
    nec_vs_oferta_enfermagem <- 
      necessidade_tratada |> 
      left_join(oferta_enfermeiros, 
                by = c("cod_regsaude"=
                         "cod_regsaud",
                       "mes"="mes",
                       "ano"="ano")) |> 
      mutate(ra_enf = oferta_enf - nec_cat) |> 
      mutate(rr_enf = oferta_enf/nec_cat) |> 
      mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                        rr_enf >= 0.50 & 
                                          rr_enf <= 1 ~ "C50",
                                        rr_enf < 0.50 ~ "C")) |> 
      ungroup() |> 
      select(cod_regsaude, uf_sigla, mes, ano, oferta_enf, rr_enf, equilibrio_enf)
    
    nec_vs_oferta_enfermagem_anual <- 
      necessidade_tratada |> 
      left_join(oferta_enfermeiros, 
                by = c("cod_regsaude"=
                         "cod_regsaud",
                       "mes"="mes",
                       "ano"="ano")) |> 
      group_by(uf_sigla, cod_regsaude, ano) |> 
      summarise(nec_enf = mean(nec_prof),
                oferta_enf = mean(oferta_enf)) |>
      mutate(rr_enf = oferta_enf/nec_enf)
    
    
    # Juntar necessidades com oferta para medicos
    
    oferta_vs_demanda_med <- 
      necessidade_tratada |> 
      left_join(oferta_medicos, 
                by = c("cod_regsaude"="cod_regsaud",
                       "mes"="mes",
                       "ano"="ano")) |> 
      left_join(foco_clinico, 
                by = c("cod_regsaude"="cod_regsaud")) |> 
      mutate(oferta_direta =  fte40 * (1 - indireta),
             oferta_med = oferta_direta * perc_fc) |> 
      left_join(nec_vs_oferta_enfermagem,
                by = c("cod_regsaude"="cod_regsaude",
                       "mes"="mes",
                       "ano"="ano",
                       "uf_sigla"="uf_sigla")) |>
      mutate(nec_medicos = 
               case_when(equilibrio_enf == "NC" ~ nec_prof * participacao_medico,
                         equilibrio_enf == "C50" ~ nec_prof * ((1 - rr_enf) + participacao_medico),
                         equilibrio_enf == "C" ~ nec_prof * (1 - rr_enf))) |> 
      mutate(total_abs = oferta_med - nec_medicos,
             total_perc = 100 * (oferta_med/nec_medicos)) 
    
    oferta_vs_demanda_med_anual <- 
      oferta_vs_demanda_med |> 
      group_by(uf_sigla, cod_regsaude, regiao_saude) |> 
      summarise(nec_med = mean(nec_medicos),
                oferta_med = mean(oferta_med)) |> 
      mutate(rr_med = 100 * (oferta_med/nec_med)) |> 
      mutate(ra_med = (oferta_med - nec_med)) 
    
    med_enf_oferta_demanda <- 
      oferta_vs_demanda_med_anual |> 
      left_join(nec_vs_oferta_enfermagem_anual,
                by = c("cod_regsaude","uf_sigla")) |> 
      mutate(rr_enf = 100 * rr_enf) |> 
      mutate(classificacao = case_when(
                                rr_med >= 100 & 
                                rr_enf >= 100 ~ "Ambos superávit",
                                rr_med >= 100 &
                                rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
                                rr_enf >= 100 &
                                rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
                                rr_med <= 100 & 
                                rr_enf <= 100 ~ "Ambos déficit"))  
    
    
    dist_uf <- med_enf_oferta_demanda |> 
      group_by(uf_sigla, classificacao) |> 
      count()
    
    b <- dist_uf |> 
      rename(Classificação = classificacao) |> 
      ggplot(aes(x = uf_sigla, y = n, fill = Classificação)) + 
      geom_col(position = "fill") +  coord_flip() + 
      theme_minimal() + 
      ylab("Percentual") + xlab("UF") + 
      scale_fill_manual(values = c(
        "Ambos superávit" = "#4CAF50",              # Verde
        "Superávit em médicos e déficit de enfermeiros" = "#2196F3",  # Azul
        "Superávit em enfermeiros e déficit de médicos" = "#FFC107",  # Amarelo
        "Ambos déficit" = "#F44336"                 # Vermelho
      ))
    
    a <- med_enf_oferta_demanda |> 
      mutate(Região = case_when(
        uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
        uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
        uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
        uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
        uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
        TRUE ~ "Não classificado"
      )) |> 
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
      theme(legend.position = "none") +
      xlim(0, 200) + ylim(0, 200)
    
    resultados <- list(
      dados = med_enf_oferta_demanda,
      grafico_dispersao = a,
      grafico_barras = b
    )
    
    return(resultados)

}



# Testando função ---------------------------------------------------------

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
