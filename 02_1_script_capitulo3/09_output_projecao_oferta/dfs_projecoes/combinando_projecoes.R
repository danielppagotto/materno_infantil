# Caminho da pasta onde estão os arquivos
caminho_pasta <- "~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/dfs_projecoes"

# Carregar pacotes necessários
library(dplyr)
library(purrr)
library(readr)

# Combinando enfermagem ---------------------------------------------------

# Obter lista de arquivos CSV com "Enfermeiros" no nome
arquivos_enfermeiros <- list.files(
  path = caminho_pasta, 
  pattern = ".*Enfermeiros\\.csv$", 
  full.names = TRUE  # Isso retorna o caminho completo
)

# Função para ler cada arquivo e adicionar uma coluna com o nome do arquivo (sem o caminho completo)
ler_arquivo <- function(caminho) {
  df <- read_csv(caminho)
  nome_arquivo <- basename(caminho)  # Extrai apenas o nome do arquivo, sem o caminho
  df$arquivo_origem <- nome_arquivo
  return(df)
}

# Ler todos os arquivos e combinar em um único dataframe
dados_combinados <- arquivos_enfermeiros |>
  map_df(ler_arquivo)

# Visualizar as primeiras linhas do dataframe resultante
head(dados_combinados)

dados_combinados <- dados_combinados |> 
                          select(-`...1`)

# Opcional: salvar o dataframe combinado
write_csv(dados_combinados, 
          "~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_enfermeiros_combinados.csv")

# Combinando medicina ---------------------------------------------------

# Obter lista de arquivos CSV com "Médicos" no nome
arquivos_medicos <- list.files(
  path = caminho_pasta, 
  pattern = ".*Médicos\\.csv$", 
  full.names = TRUE  # Isso retorna o caminho completo
)

# Ler todos os arquivos e combinar em um único dataframe
dados_combinados_med <- arquivos_medicos |>
  map_df(ler_arquivo)

# Visualizar as primeiras linhas do dataframe resultante
head(dados_combinados_med)

dados_combinados_med <- dados_combinados_med |> 
  select(-`...1`)

# Opcional: salvar o dataframe combinado
write_csv(dados_combinados_med, 
          "~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_medicos_combinados.csv")

