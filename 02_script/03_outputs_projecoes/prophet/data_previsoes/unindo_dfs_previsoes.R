
library(tidyverse)

pasta <- "~/GitHub/materno_infantil/02_script/03_outputs_projecoes/prophet/melhor_previsao_prophet"

arquivos <- list.files(path = pasta, pattern = "*.csv", full.names = TRUE)

dados_combinados <- arquivos |> 
  lapply(function(arquivo) {
    df <- read.csv(arquivo)
    df$arquivo_origem <- basename(arquivo)  
    return(df)
  }) |> 
  bind_rows()
