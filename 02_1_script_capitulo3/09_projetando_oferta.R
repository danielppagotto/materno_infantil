
library(tidyverse)
library(prophet)

oferta_aps <- read_csv("~/GitHub/materno_infantil/01_dados/oferta_aps07_25.csv") |> 
              select(-`...1`)

oferta_aps$data_formatada <- 
                  ymd(paste0(substr(oferta_aps$competen, 
                                    1, 4), "-", 
                             substr(oferta_aps$competen, 
                                    5, 6), "-01"))

oferta_aps |> 
  filter(cod_regsaud == 52001 & 
         categoria_profissional == "Médicos") |> 
  ggplot(aes(x = data_formatada, 
             y = fte40)) + 
  geom_line() + 
  geom_smooth(method="loess", span=0.3)


projecao <- function(cod_rs, 
                     categoria){


df_prophet <- oferta_aps |> 
  filter(cod_regsaud == cod_rs & 
         categoria_profissional == categoria) 

max_historico <- max(df_prophet$fte40)
uf <- unique(df_prophet$uf_sigla)
regiao <- unique(df_prophet$regiao)
rs <- unique(df_prophet$regiao_saude_pad)

df_prophet <- 
  df_prophet |> 
  select(data_formatada, fte40) |>
  rename(ds = data_formatada, 
         y = fte40) |>
  mutate(ds = as.Date(ds)) |>  
  arrange(ds)

# Ajustar o modelo prophet para capturar a tendência

m <- prophet(df_prophet, 
             yearly.seasonality = FALSE)

# Criar dataframe para previsão até 2030
future <- make_future_dataframe(m, 
                                periods = 82, 
                                freq = "month") 
future$ds <- as.Date(future$ds)

forecast <- predict(m, future)
forecast$ds <- as.Date(forecast$ds)  

data_max <- as.Date(max(df_prophet$ds))

tendencia_base <- forecast |> 
  select(ds, trend) |>
  filter(ds >= data_max) 

# Criar os três cenários de previsão

# Cenário 1: Manter o número relativamente constante
# Isso significa seguir apenas a tendência do prophet

# Cenário 2: Aumentar 10% até 2030
# Calcular fator de crescimento mensal para chegar a 5% em 5 anos
fator_aumento_mensal <- (0.90)^(1/60) - 1 # 60 meses

tendencia_aumento <- tendencia_base |>
  mutate(
    meses_desde_inicio = row_number(),
    fator = (1 + fator_aumento_mensal)^meses_desde_inicio,
    trend_aumento = trend * fator
  )

# Cenário 3: Reduzir 20% até 2030
fator_reducao_mensal <- (0.80)^(1/60) - 1 # 60 meses (fator negativo)

tendencia_reducao <- tendencia_base |>
  mutate(
    meses_desde_inicio = row_number(),
    fator = (1 + fator_reducao_mensal)^meses_desde_inicio,
    trend_reducao = trend * fator
  )

# Combinar resultados
previsoes <- tendencia_base |>
  left_join(tendencia_aumento |> 
              select(ds, trend_aumento), by = "ds") |>
  left_join(tendencia_reducao |> 
              select(ds, trend_reducao), by = "ds") |>
  rename(
    cenario_base = trend,
    cenario_aumento = trend_aumento,
    cenario_reducao = trend_reducao
  ) |> 
  mutate(cod_regsaud = cod_rs,
         uf = uf, 
         regiao_saude = rs, 
         regiao = regiao)

previsoes_max <- max(previsoes$cenario_aumento)
max_total <- c(previsoes_max, max_historico)

nome_arquivo_csv <- 
  paste0("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/dfs_projecoes/projecao_", 
         cod_rs, "_", categoria, ".csv")

write.csv(previsoes, nome_arquivo_csv)

# Criar o gráfico
# Primeiro, juntar dados históricos para o gráfico
dados_historicos <- 
  df_prophet |>
  select(ds, y) 

# Dados para a tendência histórica
tendencia_historica <- forecast |> 
  filter(ds <= data_max) |>
  select(ds, trend)

a <- ggplot() +
  # Dados históricos
  geom_line(data = dados_historicos, 
            aes(x = ds, y = y), color = "black") +
  
  # Tendência histórica extraída pelo prophet
  geom_line(data = tendencia_historica, 
            aes(x = ds, y = trend), color = "blue", size = 1.2) +
  
  # Cenários futuros
  geom_line(data = previsoes, aes(x = ds, y = cenario_base), 
            color = "blue", size = 1.2) +
  geom_line(data = previsoes, aes(x = ds, y = cenario_aumento), 
            color = "green", size = 1.2) +
  geom_line(data = previsoes, aes(x = ds, y = cenario_reducao), 
            color = "red", size = 1.2) +
  ylim(0, 1.5 * max(max_total)) +
  # Adicionar legenda e rótulos com informação da região e categoria
  labs(
    title = paste0("Projeção de FTE40 até 2030 - Região ", cod_rs, " (", rs, "), Categoria: ", categoria),
    subtitle = "Três cenários: base (azul), redução em 10% da tendência de crescimento (verde), redução de 20% da tendência de crescimento (vermelho)",
    x = "Data",
    y = "FTE40"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

a

# Criar nome de arquivo de imagem com o código da região
nome_arquivo_img <- paste0("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/graficos/grafico_", cod_rs, "_", categoria, ".jpeg")

ggsave(filename = nome_arquivo_img, 
       plot = a, dpi = 500, height = 5, width = 10)

}

codigos_rs <- unique(oferta_aps$cod_regsaud)
categorias <- unique(oferta_aps$categoria_profissional)

for (cod_rs in codigos_rs) {
  for (categoria in categorias) {
    cat(paste0("\nProcessando região ", cod_rs, " - categoria ", categoria, "...\n"))
    # Tente executar a função com tratamento de erro
    tryCatch({
      projecao(cod_rs = cod_rs, categoria = categoria)
      cat(paste0("Concluído com sucesso: região ", cod_rs, " - categoria ", categoria, "\n"))
    }, error = function(e) {
      cat(paste0("ERRO ao processar região ", cod_rs, " - categoria ", categoria, ": ", e$message, "\n"))
    })
  }
}
