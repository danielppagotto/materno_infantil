library(tidyverse)
library(readxl)

resumo_regiao <- read_csv("~/GitHub/materno_infantil/02_script/07_output_montecarlo/resumo_regiao.csv")

resumo_uf <- 
  resumo_regiao |> 
  group_by(uf_sigla) |> 
  summarise(mediana = median(media_percentual))

dados_mmr <- read_excel("~/GitHub/materno_infantil/02_script/10_associação/dados_mmr.xlsx")

join <- dados_mmr |> 
  left_join(resumo_uf, by = c("UF"="uf_sigla")) |>
  mutate(regiao = case_when(
    UF %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    UF %in% c("PR", "SC", "RS") ~ "Sul",
    UF %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    UF %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  ))
  
  

join |> 
  ggplot(aes(x = mediana, y = MMR)) + 
  geom_point() + 
  geom_label(aes(label = UF, fill = regiao)) +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() 


cor(join$MMR, join$mediana)
