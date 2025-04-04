
library(tidyverse)  # inclui readr e dplyr

# Definir o caminho da pasta que contém os arquivos CSV
pasta <- "~/GitHub/materno_infantil/02_script/04_servicos"

# Listar todos os arquivos CSV na pasta
arquivos <- list.files(path = pasta, pattern = "*.csv", full.names = TRUE)

# Ler todos os arquivos e combiná-los em um único dataframe
dados_combinados <- arquivos %>%
  # Ler cada arquivo
  lapply(function(arquivo) {
    # Opcionalmente, você pode adicionar o nome do arquivo como uma coluna
    df <- read.csv(arquivo) |> 
      mutate(data = as.Date(data)) |> 
      mutate(ano = year(data)) |> 
      filter(ano > 2015 & ano < 2023)
    
    df$arquivo_origem <- basename(arquivo)  # adiciona o nome do arquivo como coluna
    return(df)
  }) %>%
  # Combinar todos os dataframes em um único
  bind_rows()

# Ver as primeiras linhas do dataframe combinado
head(dados_combinados)

# Verificar a dimensão do dataframe final
dim(dados_combinados)

write.csv(dados_combinados, 
          "~/GitHub/materno_infantil/02_script/04_servicos/servicos_2019.csv")
