library(tidyverse)


# Demanda -----------------------------------------------------------------

servicos23_32 <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/06_servicos/servicos24_30_tratado.csv")

tendencia_numerico <- servicos23_32 |> 
  select(cod_regsaude, competen, qtd_nasc) |> 
  distinct() |> 
  filter(year(competen) < 2031) |> 
  filter(competen %in% c("2024-01-01", "2030-12-01")) %>%
  group_by(cod_regsaude) %>%
  arrange(cod_regsaude, competen) %>%
  summarize(
    valor_inicial = first(qtd_nasc),
    valor_final = last(qtd_nasc),
    variacao_percentual = ((valor_final / valor_inicial) - 1) * 100
  ) |> 
  select(cod_regsaude, variacao_percentual) |> 
  mutate(direcao = case_when(
    variacao_percentual > 1 ~ "Crescimento",
    variacao_percentual < -1 ~ "Redução",
    TRUE ~ "Estacionário"
  )) 

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc) |> 
  mutate(ano = 2024) |>
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(tendencia_numerico, 
            by= c("cod_regsaud"="cod_regsaude"))


df <- foco_clinico

calcular_novo_perc_fc <- function(registro, ano_atual) {
  ano_base <- 2024
  anos_passados <- ano_atual - ano_base
  valor_base <- registro$perc_fc
  var_percentual <- registro$variacao_percentual / 100
  
  if (registro$direcao == "Estacionário") {
    return(valor_base)
  } else if (registro$direcao == "Redução") {
    # Calculamos a taxa anual de redução para distribuir a variação até 2030
    # Período de 6 anos (2025-2030)
    taxa_anual_reducao <- 1 - (1 + var_percentual)^(1/6)
    return(valor_base * (1 - taxa_anual_reducao)^anos_passados)
  } else if (registro$direcao == "Crescimento") {
    # Calculamos a taxa anual de crescimento para distribuir a variação até 2030
    taxa_anual_crescimento <- (1 + var_percentual)^(1/6) - 1
    return(valor_base * (1 + taxa_anual_crescimento)^anos_passados)
  }
  
  return(valor_base) # Caso default
}

# Criar dataframe vazio para armazenar os resultados
df_projetado <- data.frame()

# Projetar valores para cada registro original
for (i in 1:nrow(df)) {
  registro_atual <- df[i, ]
  
  # Adicionar o registro original (2024)
  df_projetado <- rbind(df_projetado, registro_atual)
  
  # Projetar valores para 2025 até 2030
  for (ano in 2025:2030) {
    novo_registro <- registro_atual
    novo_registro$ano <- ano
    novo_registro$perc_fc <- calcular_novo_perc_fc(registro_atual, ano)
    
    df_projetado <- rbind(df_projetado, novo_registro)
  }
}

# Ordenar o dataframe por cod_regsaud e ano
df_projetado <- df_projetado[order(df_projetado$cod_regsaud, df_projetado$ano), ]

# Resetar os rownames
rownames(df_projetado) <- NULL

write.csv(df_projetado, 
          "~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv")
