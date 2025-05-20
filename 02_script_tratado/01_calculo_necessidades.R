

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
absenteismo <- 0.20
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
