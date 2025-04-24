# Carregar biblioteca necessária
library(dplyr)
library(readr)
library(purrr)
library(tidyr)

# Função para ler um arquivo CSV e adicionar o código da região de saúde como coluna
ler_arquivo_csv <- function(caminho_arquivo) {
  nome_arquivo <- basename(caminho_arquivo)
  
  # Extrair apenas os números do nome do arquivo
  # Padrão esperado: metricas_XXXXX.csv, onde XXXXX são os números
  codigo_regsaud <- gsub("metricas_([0-9]+)\\.csv", "\\1", nome_arquivo)
  
  # Ler o arquivo CSV
  dados <- read_csv(caminho_arquivo, show_col_types = FALSE)
  
  # Adicionar coluna com o código da região de saúde
  dados$cod_regsaud <- codigo_regsaud
  
  return(dados)
}

# Diretório onde estão os arquivos CSV
diretorio <- "~/GitHub/materno_infantil/02_script/03_outputs_projecoes/prophet/metricas_previsoes_prophet"  # Use o diretório atual, ajuste se necessário

# Listar todos os arquivos CSV que começam com "metricas_"
arquivos_csv <- list.files(
  path = diretorio,
  pattern = "^metricas_\\d+\\.csv$",
  full.names = TRUE
)

# Verificar os arquivos encontrados
cat("Arquivos encontrados:\n")
for (arquivo in arquivos_csv) {
  cat("-", arquivo, "\n")
}

# Ler e combinar todos os arquivos
if (length(arquivos_csv) > 0) {
  # Aplicar a função de leitura para cada arquivo
  lista_dados <- lapply(arquivos_csv, ler_arquivo_csv)
  
  # Combinar todos os dataframes em um único
  dados_combinados <- bind_rows(lista_dados)
  
  # Mostrar resumo dos dados combinados
  cat("\nResumo dos dados combinados:\n")
  cat("- Número total de arquivos:", length(arquivos_csv), "\n")
  cat("- Número total de linhas:", nrow(dados_combinados), "\n")
  cat("- Colunas:", paste(colnames(dados_combinados), collapse = ", "), "\n\n")
  
  # Salvar o resultado como um novo CSV
  arquivo_saida <- "metricas_combinadas.csv"
  write_csv(dados_combinados, arquivo_saida)
  cat("Dados combinados salvos em:", arquivo_saida, "\n")
  
  # Também salvar como arquivo RDS (formato R) para preservar tipos de dados
  write_rds(dados_combinados, "metricas_combinadas.rds")
  cat("Dados combinados também salvos em: metricas_combinadas.rds\n")
  
  # Retornar o dataframe combinado (útil se estiver executando interativamente)
  dados_combinados
} else {
  cat("Nenhum arquivo CSV encontrado com o padrão especificado.\n")
}



# Dados combinados --------------------------------------------------------

best_model <- 
  dados_combinados |> 
  group_by(cod_regsaud) |> 
  slice_min(mape) 

write.csv(best_model, 
          "~/GitHub/materno_infantil/02_script/03_outputs_projecoes/prophet/metricas_previsoes_prophet/melhor_modelo.csv")
