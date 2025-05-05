library(tidyverse)
library(jsonlite)
library(progress)

df_combinado <- vroom::vroom("~/GitHub/materno_infantil/02_script/03_outputs_projecoes/prophet/melhor_previsao_prophet/dfs_combinadoss.csv") |> 
  select(-`...1`) |> 
  rename(data = .index,
         qtd = .value)

acompanhando <- df_combinado |> 
  mutate(estado = substr(cod_regsaude, 
                         1, 2)) |> 
  select(estado, cod_regsaude) |> 
  distinct()

ce <- c("29011")

dfs <- df_combinado |> 
  mutate(ano = year(data)) |> 
  filter(ano > 2015) |>
#  filter(cod_regsaude %in% ce)
  
  filter(substr(cod_regsaude, 1, 2) == "28")

pb <- progress_bar$new(
  format = "[:bar] :percent Concluído :current/:total (:eta restantes)",
  total = nrow(dfs),
  clear = FALSE,
  width = 80
)
 

servicos <- tibble()

for(i in 1:nrow(dfs)){
  
  pb$tick()
  
  
  row <- dfs[i,]
  url = paste("https://api-dimensionamento.face.ufg.br/calcula_procedimentos?mes_ano=", 
              substring(row$data, 1, 7), 
              "&nascidos_vivos=", 
              round(row$qtd, 0), sep = '')
  temp <- fromJSON(url)
  temp$data <- row$data
  temp$qtd <- row$qtd
  temp$ibge <- row$cod_regsaude
  
  servicos <- rbind(temp, servicos)
  
  print(paste("Chamando:",url))
}

write.csv(servicos, 
          "~/GitHub/materno_infantil/02_script/04_servicos/servicos_SE.csv")
