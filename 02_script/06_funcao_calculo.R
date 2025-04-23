
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

teste <- oferta_vs_demanda(acoes_educacionais = 30,
                           alto_risco = 0.15,
                           acoes_altorisco = 45,
                           consultas = 30, 
                           absenteismo = 8, 
                           indireta = 0.50,
                           todos = 1,
                           consultas_altorisco = 40)

teste2 <- oferta_vs_demanda(acoes_educacionais = 30,
                            alto_risco = 0.50,
                            acoes_altorisco = 45,
                            consultas = 30, 
                            absenteismo = 8, 
                            indireta = 0.50,
                            todos = 1,
                            consultas_altorisco = 40)


# Plotando resultado em mapa ----------------------------------------------


spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 

estados_br <- read_state(year = 2020,
                         showProgress = FALSE)

spdf_fortified <- 
  sf::st_as_sf(spdf)

# Definir limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude

baseline <- 
  teste |> 
  mutate(perc = if_else(perc > 100, 100, perc)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()


baseline_sus <- 
  teste2 |> 
  mutate(perc = if_else(perc > 100, 100, perc)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()


# Função para gerar mapa --------------------------------------------------


gerar_mapa <- 
  
  function(df, var_perc){
  
  var_sym <- sym(var_perc)
  
  ggplot() +
    geom_sf(data = df, 
            aes(fill = !!var_sym, 
                geometry = geometry), 
            color = "#f5f5f5") +
    geom_sf(data = estados_br, 
            fill = NA, 
            color = "#4c4d4a", 
            size = 0.1) +
    theme_minimal() +
    scale_fill_gradientn(colors = 
                           c("#FF2400","#FF7F00",  
                             "#c1c700","#7ac142",  
                             "#2c7719"),  
                         values = rescale(c(0, 50, 
                                            100)), 
                         limits = c(0, 100),
                         breaks = c(0, 50, 
                                    100)) +
    labs(fill = "Gap de profissionais") +
    annotation_north_arrow(
      location = "tr", 
      which_north = "true",
      style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = "bl", 
                     width_hint = 0.3) +
    theme(
      legend.justification = "center",
      legend.box = "horizontal",
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14),  
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      plot.title = element_text(size = 14),
      panel.border = element_rect(color = "black", 
                                  fill = NA, 
                                  size = 1), 
      plot.margin = margin(10, 10, 10, 10))
}

gerar_mapa(df = baseline_sus, 
           var_perc = "perc")
