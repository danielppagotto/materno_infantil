library(tidyverse)


# Demanda -----------------------------------------------------------------

servicos23_32 <- 
  read_csv("~/GitHub/materno_infantil/04_analises_UF_cap3/02_servicos/servicos_UF_1733.csv") |>   
  select(-`...1`) |> 
  rename(qtd_nascidos = qtd,
         mes_nascimento = data)


tendencia_numerico <- servicos23_32 |> 
  select(uf, mes_nascimento, qtd_nascidos) |> 
  distinct() |> 
  filter(year(mes_nascimento) < 2031) |> 
  filter(mes_nascimento %in% c("2024-01-01", "2030-12-01")) %>%
  group_by(uf) %>%
  arrange(uf, mes_nascimento) %>%
  summarize(
    valor_inicial = first(qtd_nascidos),
    valor_final = last(qtd_nascidos),
    variacao_percentual = ((valor_final / valor_inicial) - 1) * 100
  ) |> 
  select(uf, variacao_percentual) |> 
  mutate(direcao = case_when(
    variacao_percentual > 1 ~ "Crescimento",
    variacao_percentual < -1 ~ "Redução",
    TRUE ~ "Estacionário"
  )) 

foco_clinico <- 
  readxl::read_excel("~/GitHub/materno_infantil/04_analises_UF_cap3/05_projecao_FC_outputs/fc_uf.xlsx") |> 
  select(UF, perc_fc) |> 
  mutate(ano = 2024) |>
  left_join(tendencia_numerico, 
            by= c("UF"="uf"))


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
df_projetado <- 
  df_projetado[order(df_projetado$UF, df_projetado$ano), ]

# Resetar os rownames
rownames(df_projetado) <- NULL

write.csv(df_projetado, 
          "~/GitHub/materno_infantil/04_analises_UF_cap3/05_projecao_FC_outputs/fc_UF_projetado.csv")
