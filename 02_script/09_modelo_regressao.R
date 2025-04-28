library(tidyverse)
library(car)
library(sandwich)
library(lmtest)
library(modelsummary)
library(pandoc)

options(scipen = 999)

dados_modelo <- read_csv("02_script/07_output_montecarlo/resultados_regioes.csv")

# # criando o modelo --------------------------------------------------------


dados_modelo <- dados_modelo |> 
  mutate(alto_risco = 100 * alto_risco) |> 
  mutate(indireta = 100 * indireta) |> 
  mutate(regiao = case_when(uf_sigla %in% c("MG", "SP", "RJ", 
                                            "SP") ~ "Sudeste", 
                            uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
                            uf_sigla %in% c("AC", "AM", "AP", 
                                            "PA", "RO", "RR", "TO") ~ "Norte",
                            uf_sigla %in% c("AL", "BA", "CE", 
                                            "MA", "PB", "PE", 
                                            "PI", "RN", "SE") ~ "Nordeste",
                            uf_sigla %in% c("DF", "GO", 
                                            "MT", "MS") ~ "Centro-Oeste")) |> 
  mutate(todos = as.factor(todos))


modelo <- 
  lm("perc ~ acoes_educacionais + consultas + absenteismo + alto_risco +
      indireta + todos + acoes_altorisco + consultas_altorisco + regiao", 
     data = dados_modelo)

summary(modelo)

modelo_robusto <- coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))

summary(modelo_robusto)

modelo_robusto

hist(modelo$residuals)
lmtest::bptest(modelo)

modelsummary(modelo, 
             vcov = "HC1",              # Usar erros padrão robustos HC1
             stars = TRUE,              # Mostrar significância estatística
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "table.docx")     


planalto <- dados_modelo |> 
  filter(regiao_saude == "Região 17 - Planalto")

quantile(planalto$perc)
