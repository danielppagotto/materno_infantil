
library(tidyverse)
library(vroom)
library(readxl)
library(RODBC)

resultados_regioes <- 
  read_csv("~/GitHub/materno_infantil/02_script/08_output_gráficos/cenarios_uf_1105.csv") |> 
  mutate(regiao = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  )) 

rendimentos <- read_excel("~/GitHub/materno_infantil/02_script/11_rendimentos/rendimentos_uf.xlsx") |> 
                            mutate(prof = if_else(prof == "Enfermeiro",
                                                  "Enfermeiros",
                                                  "Médicos"))


# Juntando bases ----------------------------------------------------------

resultado_tratado <- 
  resultados_regioes |> 
  left_join(rendimentos, 
            by = c("uf_sigla"="uf_recod",
                   "categoria"="prof")) |> 
  mutate(lacuna = (absoluto * rendimento * 1.40)) |> 
  group_by(uf_sigla, categoria, cenario) |> 
  summarise(resultado_absoluto = sum(absoluto),
            valor = sum(lacuna)) |> 
  mutate(resultado_absoluto = (-1) * round(resultado_absoluto),
         valor = (-1) * valor/1000000,
         valor = round(valor, 2), 
         valor = if_else(valor < 0, 0, valor),
         resultado_absoluto = (-1) * round(resultado_absoluto)) |> 
  pivot_wider(
    names_from = cenario,
    values_from = c(resultado_absoluto, valor)
  ) |> 
  left_join(rendimentos, 
            by = c("uf_sigla"="uf_recod",
                   "categoria"="prof")) |> 
  select(uf_sigla, 
         categoria, 
         rendimento,
         `resultado_absoluto_Cenário 1`,
         `valor_Cenário 1`,
         `resultado_absoluto_Cenário 2`,
         `valor_Cenário 2`,
         `resultado_absoluto_Cenário 3`,
         `valor_Cenário 3`,
         `resultado_absoluto_Cenário 4`,
         `valor_Cenário 4`)


writexl::write_xlsx(resultado_tratado, 
                    "~/GitHub/materno_infantil/02_script/11_rendimentos/rendimentos_necessarios.xlsx")

