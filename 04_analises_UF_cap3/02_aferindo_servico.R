library(tidyverse)
library(jsonlite)
library(progress)

df_combinado <- vroom::vroom("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/UF_proj_ensemble.csv") |> 
  select(-`...1`) |> 
  rename(data = .index,
         qtd = .value,
         uf = origem)

acompanhando <- df_combinado |> 
  select(uf) |> 
  distinct()

dfs <- df_combinado |> 
  mutate(ano = year(data)) |> 
  filter(ano > 2016) 
  
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
  temp$uf <- row$uf
  
  servicos <- rbind(temp, servicos)
  
  print(paste("Chamando:",url))
}

write.csv(servicos, 
          "~/GitHub/materno_infantil/04_analises_UF_cap3/02_servicos/servicos_UF_1733.csv")
