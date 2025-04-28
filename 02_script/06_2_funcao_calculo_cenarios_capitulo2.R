
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
  vroom::vroom("~/GitHub/materno_infantil/02_script/04_servicos/servicos_2019.csv") |> 
  select(-`...1`) 

oferta <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_aps_2019.csv") |> 
  select(-`...1`) 

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) 

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc)


# Função ------------------------------------------------------------------

acoes_educacionais <- 30
consultas <- 30
ttd <- 133
indireta <- 0.40
todos <- 0
ferias <- 16
feriados <- 4
absenteismo <- 16 #novo parametro da funcao
consultas_altorisco <- 45
alto_risco <- 0.15


oferta_vs_demanda <- 
  
  function(acoes_educacionais,
           consultas, 
           alto_risco,
           indireta, 
           todos, 
           acoes_altorisco, 
           consultas_altorisco,
           absenteismo){

# se todos = 1, pegar todos 
# se todos = 0, pegar apenas SUS dependente

sus <- 1
ferias <- 16
feriados <- 4

# Filtrar procedimentos de 2019
servicos2019 <- 
  servicos |> 
  mutate(ano_proc_rea = 
           year(mes_proc_rea)) |> 
  filter(ano_proc_rea == 2019) |> 
  filter(nivel_atencao == "APS" & 
         publico != "Gestantes de Alto Risco") |>
  filter(tipo_procedimento == "Ações Educacionais" |
         tipo_procedimento == "Consultas ou Visitas") |> 
  filter(procedimento != "Avaliação odontológica") |> 
  filter(procedimento != "Visita domiciliar") 

cobertura$cod_regsaud <- 
  as.numeric(cobertura$cod_regsaud)

# Juntar dados de serviços com cobertura

servicos19_tratado <- 
  servicos2019 |> 
  left_join(cobertura, 
            by = c("cod_regsaude"=
                   "cod_regsaud")) |> 
  janitor::clean_names() |> 
  mutate(cobertura = cobertura/100) |> 
  rename(cobertura_ans = cobertura) 

sus = 1

if(sus == todos){
  servicos19_tratado <- servicos19_tratado |> 
    
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
  servicos19_tratado <- 
    servicos19_tratado |>
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
  servicos19_tratado |> 
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
  mutate(nec_prof = (total_horas_rh + total_horas_ar)/ttd) 

necessidade_tratada <- 
  necessidade |> 
  group_by(uf_sigla, cod_regsaude, regiao_saude,  
           mes_proc_rea) |> 
  summarise(nec_prof = sum(nec_prof),
            nec_ch = sum(total_horas_rh + total_horas_ar)) |> 
  mutate(mes = month(mes_proc_rea),
         ano = year(mes_proc_rea), 
         .after = mes_proc_rea)

# Juntar necessidades com oferta

oferta_vs_demanda <- 
  necessidade_tratada |> 
    left_join(oferta, 
              by = c("cod_regsaude"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico, 
            by = c("cod_regsaude"="cod_regsaud")) |> 
    mutate(oferta_direta =  fte40 * (1 - indireta),
           oferta_linha = oferta_direta * perc_fc) |> 
    mutate(total_abs = oferta_linha - nec_prof,
           total_perc = 100 * (oferta_linha/nec_prof)) |> 
    group_by(uf_sigla, cod_regsaude, regiao_saude) |> 
    summarise(necessidade_media = mean(nec_prof),
              oferta_media = mean(oferta_linha)) |> 
    mutate(perc = 100 * (oferta_media/necessidade_media)) |> 
    mutate(total = (oferta_media - necessidade_media)) 

}


# Testando função ---------------------------------------------------------

cenario1 <- oferta_vs_demanda(acoes_educacionais = 30,
                           alto_risco = 0.20,
                           acoes_altorisco = 40,
                           consultas = 30, 
                           absenteismo = 24, 
                           indireta = 0.50,
                           todos = 1,
                           consultas_altorisco = 40) |> 
            mutate(cenario = "Cenário 1")

cenario2 <- oferta_vs_demanda(acoes_educacionais = 30,
                            alto_risco = 0.20,
                            acoes_altorisco = 40,
                            consultas = 30, 
                            absenteismo = 24, 
                            indireta = 0.50,
                            todos = 0,
                            consultas_altorisco = 40) |> 
            mutate(cenario = "Cenário 2")


cenario3 <- oferta_vs_demanda(acoes_educacionais = 30,
                              alto_risco = 0.10,
                              acoes_altorisco = 40,
                              consultas = 30, 
                              absenteismo = 8, 
                              indireta = 0.40,
                              todos = 0,
                              consultas_altorisco = 40) |> 
  mutate(cenario = "Cenário 3")


cenario4 <- oferta_vs_demanda(acoes_educacionais = 20,
                              alto_risco = 0.10,
                              acoes_altorisco = 30,
                              consultas = 20, 
                              absenteismo = 8, 
                              indireta = 0.40,
                              todos = 0,
                              consultas_altorisco = 25) |> 
  mutate(cenario = "Cenário 4")

cenarios <- rbind(cenario1,
                  cenario2,
                  cenario3,
                  cenario4)

# write.csv(cenarios, 
#           "~/GitHub/materno_infantil/02_script/08_output_gráficos/cenarios_selecionados.csv")
# 

cenarios_comparados <- cenario1 |> 
                          left_join(cenario2, by = "cod_regsaude") |> 
                          left_join(cenario3, by = "cod_regsaude") |>
                          left_join(cenario4, by = "cod_regsaude") |> 
                          mutate(comp12 = perc.y - perc.x) |> 
                          mutate(comp23 = perc.x.x - perc.y) |> 
                          mutate(comp34 = perc.y.y - perc.x.x)

mean(cenarios_comparados$comp12)
mean(cenarios_comparados$comp23)
mean(cenarios_comparados$comp34)
