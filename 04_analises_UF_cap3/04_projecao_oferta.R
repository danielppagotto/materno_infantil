
library(tidyverse)
library(prophet)
library(RODBC)


# Leitura dos dados de oferta ---------------------------------------------

dremio_host <- Sys.getenv("endereco")
dremio_port <- Sys.getenv("port")
dremio_uid <- Sys.getenv("uid")
dremio_pwd <- Sys.getenv("datalake")


channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;
                                     HOST=%s;
                                     PORT=%s;
                                     UID=%s;
                                     PWD=%s;
                                     AUTHENTICATIONTYPE=Basic Authentication;
                                     CONNECTIONTYPE=Direct", 
                                     dremio_host, 
                                     dremio_port, 
                                     dremio_uid, 
                                     dremio_pwd))


query <- 'SELECT * FROM "@daniel"."Profissionais APS UF"'


oferta_aps <- sqlQuery(channel, 
                     query, 
                     as.is = TRUE) |> 
  janitor::clean_names()


oferta_aps$fte40 <- as.numeric(oferta_aps$fte40)




# Fazendo alguns tratamentos ----------------------------------------------


oferta_aps$data_formatada <- 
  ymd(paste0(substr(oferta_aps$competen, 
                    1, 4), "-", 
             substr(oferta_aps$competen, 
                    5, 6), "-01"))


oferta_aps19_25 <- oferta_aps |> 
                      filter(data_formatada >= "2019-01-01")

#write.csv(oferta_aps19_25,
#          "~/GitHub/materno_infantil/04_analises_UF_cap3/04_projecao_oferta_uf/oferta_aps_uf_2019_2025.csv")


oferta_aps |> 
  filter(uf_sigla == "GO" & 
           categoria_profissional == "Médicos") |> 
  ggplot(aes(x = data_formatada, 
             y = fte40)) + 
  geom_line() + 
  geom_smooth(method="loess", span=0.3) + 
  theme_minimal() + 
  ylim(0, 4500)


projecao <- function(uf, 
                     categoria){
  
df_prophet <- oferta_aps |> 
  filter(uf_sigla == uf & 
         categoria_profissional == categoria) 
  
  max_historico <- max(df_prophet$fte40)
  uf_sigla <- unique(df_prophet$uf_sigla)
  regiao <- unique(df_prophet$regiao)
  
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
                                  periods = 70, 
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
  
  # Cenário 2: Aumentar 5% até 2030
  # Calcular fator de crescimento mensal para chegar a 5% em 5 anos
  fator_aumento_mensal <- (1.05)^(1/60) - 1 # 60 meses
  
  tendencia_aumento <- tendencia_base |>
    mutate(
      meses_desde_inicio = row_number(),
      fator = (1 + fator_aumento_mensal)^meses_desde_inicio,
      trend_aumento = trend * fator
    )
  
  # Cenário 3: Reduzir 5% até 2030
  fator_reducao_mensal <- (0.95)^(1/60) - 1 # 60 meses (fator negativo)
  
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
    mutate(uf_sigla = uf_sigla, 
           regiao = regiao)
  
  previsoes_max <- max(previsoes$cenario_aumento)
  max_total <- c(previsoes_max, max_historico)
  
  nome_arquivo_csv <- 
    paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/04_projecao_oferta_uf/dfs_projecoes/projecao_", 
           uf_sigla, "_", categoria, ".csv")
  
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
    ylim(0, max(max_total)) +
    # Adicionar legenda e rótulos com informação da região e categoria
    labs(
      title = paste0("Projeção de FTE40 até 2030 - Região ", uf_sigla, " - ", categoria, ", categoria"),
      subtitle = "Três cenários: base (azul), aumento de 10% (verde), redução de 10% (vermelho)",
      x = "Data",
      y = "FTE40"
    ) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Criar nome de arquivo de imagem com o código da região
  nome_arquivo_img <- paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/04_projecao_oferta_uf/graficos/grafico_", uf, "_", categoria, ".jpeg")
  
  ggsave(filename = nome_arquivo_img, 
         plot = a, dpi = 500, height = 5, width = 10)
  
}


projecao(uf = "GO", 
         categoria = "Médicos")


#projecao(uf = "53001", "Médicos")

uf <- unique(oferta_aps$uf_sigla)

for (uf in uf) {
  
     projecao(uf = uf, categoria = "Médicos")
    
}
