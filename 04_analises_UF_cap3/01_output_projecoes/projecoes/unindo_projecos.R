
library(tidyverse)

setwd("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes")

# Carregar pacotes necessários
library(dplyr)
library(readr)
library(purrr)


# Listar todos os arquivos CSV na pasta
arquivos_csv <- list.files(pattern = ".*_projecao\\.csv$")

# Função para ler cada arquivo e adicionar uma coluna com o nome do arquivo
ler_arquivo <- function(arquivo) {
  # Extrair o prefixo do nome do arquivo (AM, GO, MS, RR)
  prefixo <- sub("_projecao\\.csv$", "", arquivo)
  
  # Ler o arquivo CSV
  dados <- read_csv(arquivo)
  
  # Adicionar uma coluna com o identificador do arquivo
  dados$origem <- prefixo
  
  return(dados)
}

# Aplicar a função a cada arquivo e combinar em um único dataframe
dados_combinados <- arquivos_csv %>%
  map_dfr(ler_arquivo)

# Visualizar as primeiras linhas do dataframe combinado
head(dados_combinados)

# Salvar o dataframe combinado em um novo arquivo CSV
write_csv(dados_combinados, "UF_proj_ensemble.csv")
