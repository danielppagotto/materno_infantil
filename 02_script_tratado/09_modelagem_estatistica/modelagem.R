

library(tidyverse)
library(RODBC)
library(geojsonio)
library(geojsonsf)
library(readxl)

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

query <- 
  'SELECT * FROM "Analytics Layer"."Econômico"."PIB per capita por regiões de saúde"
    WHERE ANO = 2021'


pibpercapita <- sqlQuery(channel, 
                     query, 
                     as.is = TRUE) |> 
                select(-regiao_saude)

# educação ----------------------------------------------------------------

query_ies <- 
  'SELECT * FROM "@daniel"."educacao_superior_IES"'


ies <- sqlQuery(channel, 
                 query_ies, 
                 as.is = TRUE)

ies$vagas <- as.numeric(ies$vagas)
ies$matriculas <- as.numeric(ies$matriculas)

ies$vagas <- ifelse(is.na(ies$vagas), 0, ies$vagas)
ies$matriculas <- ifelse(is.na(ies$matriculas), 0, ies$matriculas)

# retenção 

query_retencao <- 
  'SELECT * FROM Dados.retencao."Médico_retencao_geral.parquet"'

retencao <- sqlQuery(channel, 
                     query_retencao, 
                     as.is = TRUE)

# VD 

rr <- read_csv("~/GitHub/materno_infantil/02_script_tratado/02_output_mc/resumo_resultados_1000.csv") |> 
          select(cod_regsaud, 
                 mediana_rr_med)
# VD 

# Juntando tudo  ----------------------------------------------------------

basesjuntas <- 
  rr |> 
  mutate(cod_regsaud = as.character(cod_regsaud)) |> 
  left_join(ies, 
            by= c("cod_regsaud"="cod_regsaud")) |> 
  left_join(retencao, 
            by = c("cod_regsaud"="regiao_saude")) |> 
  left_join(pibpercapita,
            by = c("cod_regsaud"="cod_regsaud")) |> 
  mutate(resultado = if_else(mediana_rr_med >= 100, "1","0"))

# 1. Primeiro, verificar e limpar os dados
basesjuntas <- basesjuntas %>%
  # Converter resultado para numérico
  mutate(resultado = as.numeric(resultado),
         # Tratar NAs nas variáveis preditoras
         retencao_geral = ifelse(is.na(retencao_geral), 0, retencao_geral),
         matriculas = ifelse(is.na(matriculas), 0, matriculas),
         PIB_PER_CAPITA = ifelse(is.na(PIB_PER_CAPITA), 0, PIB_PER_CAPITA)) %>%
  # Remover linhas com NA na variável dependente
  filter(!is.na(resultado)) |> 
  mutate(POPULACAO = as.numeric(POPULACAO)) |> 
  mutate(matriculas_percapita = matriculas/POPULACAO)

# 3. Rodar o modelo (SEM aspas na fórmula)
modelo <- glm(resultado ~ retencao_geral + matriculas_percapita + regiao + PIB_PER_CAPITA + POPULACAO, 
              data = basesjuntas, 
              family = binomial)


modelo <- lm("mediana_rr_med ~ regiao + matriculas + retencao_geral + PIB_PER_CAPITA + POPULACAO",
             data = basesjuntas)


# 4. Ver os resultados
summary(modelo)
