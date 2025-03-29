library(tidyverse)
library(modeltime)
library(lubridate)
library(timetk)
library(modeltime)
library(parsnip)
library(workflows)
library(recipes)
library(earth)  
library(xgboost)  
library(rsample)

av <- nascidos |> 
  select(uf_sigla, cod_regsaud, regiao_saude) |>
  distinct() |> 
  mutate(uf_cod = substr(cod_regsaud, 1, 2)) |> 
  group_by(uf_sigla, uf_cod) |> 
  count()


nascidos <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/nascidos_regiao_saude.csv") |> 
  select(-`...1`)

avalicao <- nascidos |> 
              group_by(regiao_saude, uf_sigla) |> 
              count()



projecao <- function(regiao){

  df_regiao <- 
    nascidos |>
    filter(cod_regsaud == regiao) 
  
  splits <- time_series_split(
    df_regiao, 
    date_var = data_nascimento,
    assess = "2 years",
    cumulative = TRUE)
  
  receita <- 
    recipe(total ~ data_nascimento, 
           data = training(splits)) |>
    step_timeseries_signature(data_nascimento)
  
  # modelo_arima <-
  #   arima_reg() |>
  #   set_engine("auto_arima") |>
  #   fit(total ~ data_nascimento,
  #       data = training(splits))
  # 
  # 2. Prophet
  modelo_prophet <- 
    prophet_reg() |>
    set_engine("prophet") |>
    fit(total ~ data_nascimento, 
        data = training(splits))
  
  # # 3. Regressão Exponencial
  # modelo_exp <-
  #   exp_smoothing() |>
  #   set_engine("ets") |>
  #   fit(total ~ data_nascimento,
  #       data = training(splits))
  # 
  # # 4. MARS (Regressão Adaptativa Multivariada)
  # modelo_mars <-
  #   mars(mode = "regression") |>
  #   set_engine("earth") |>
  #   fit(total ~ data_nascimento,
  #       data = training(splits))

  # 5. XGBoost
  modelo_xgboost <-
    boost_tree(mode = "regression") |>
    set_engine("xgboost") |>
    fit(total ~ data_nascimento,
        data = training(splits))

  modelos <-
    modeltime_table(
      #modelo_arima,
      modelo_prophet,
      #modelo_exp,
      #modelo_mars,
      modelo_xgboost)
  
  calibracao <- 
    modelos |>
    modeltime_calibrate(testing(splits))
  
  tabela_precisao <- 
    calibracao |>
    modeltime_accuracy() |>
    arrange(mae)
  
  # guardando modelo com o menor MAE
  melhor_modelo <- tabela_precisao |> 
    slice_min(mae)
  
  tabela_precisao
  
  write.csv(tabela_precisao, 
            file = paste0("~/GitHub/materno_infantil/02_script/03_outputs_projecoes/metricas_previsoes/metricas_", 
                          unique(df_regiao$cod_regsaud), ".csv"),
            row.names = FALSE)
  
  # Faz a previsão
  previsao <- calibracao |>
    modeltime_forecast(
      new_data_nascimento = testing(splits),
      actual_data_nascimento = df_nascidos)
  
  teste <- testing(splits) |> 
    mutate(.model_id = 6,
           .model_desc = "real",
           .key = "actual") |>           
    rename(.index = data_nascimento,
           .value = total)  |> 
    select(.model_id, .model_desc,
           .key, .index, .value)
  
  previsao <- previsao |> 
    select(.model_id, .model_desc,
           .key, .index, .value)
  
  teste_previsto <- rbind(teste, 
                          previsao) 
  
  # Visualiza a previsão
  teste_previsto |>
    plot_modeltime_forecast(
      .legend_max_width = 250, 
      .conf_interval_show = FALSE,
      .interactive = TRUE,
      .title = "Comparação de Previsões de Nascidos Vivos")
  
  # Obter a última data dos dados
  ultima_data <- max(df_regiao$data_nascimento)
  
  # Criar um dataframe com datas futuras até 2030
  datas_futuras <- seq.Date(
    from = as.Date(ultima_data) + months(1),  
    to = as.Date("2030-12-01"),               
    by = "month")
  
  # Criar dataframe futuro
  futuro <- tibble(data_nascimento = datas_futuras)
  
  # Reajustar todos os modelos usando os dados completos (não apenas os dados de treinamento)
  modelos_reajustados <- calibracao |>
    modeltime_refit(data = df_regiao)
  
  # Fazer a projeção até 2030
  projecao_2030 <- modelos_reajustados |>
    modeltime_forecast(
      new_data = futuro,
      actual_data = df_regiao)
  
  # Visualizar a projeção até 2030 trazendo os dados históricos e todas as projecoes
  projecao_2030 |>
    ggplot(aes(x = .index, y = .value, col = .model_desc)) + 
    geom_line() + theme_minimal() + 
    xlab("Anos") + ylab("Nascidos")
  
  # Criar tabela com valores anuais (mais fácil de analisar)
  projecao_anual <- 
    projecao_2030 |>
    mutate(
      ano = lubridate::year(.index),
      mes = lubridate::month(.index)
    ) |>
    group_by(.model_id, 
             .model_desc, 
             ano) |>
    summarise(
      total_nascidos = sum(.value, 
                           na.rm = TRUE),
      .groups = "drop")
  
  # Visualizar projeção anual
  projecao_anual |>
    ggplot(aes(x = ano, 
               y = total_nascidos, 
               color = .model_desc)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Média Anual de Nascidos Vivos - Projeção até 2030",
      x = "Ano",
      y = "Média Mensal de Nascidos Vivos",
      color = "Modelo"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(min(projecao_anual$ano), 
                                    2030, 
                                    by = 2))
  
  # Tabela com os valores das projeções anuais
  projecao_anual_wide <- 
    projecao_anual |>
    filter(ano >= 2023) |>  # Filtra apenas anos futuros
    select(ano, .model_desc, total_nascidos) |>
    tidyr::pivot_wider(
      names_from = .model_desc,
      values_from = total_nascidos
    ) |>
    arrange(ano)
  
  # Exportar para CSV (opcional)
  # write.csv(projecao_anual_wide, 
  #            "projecao_nascidos_2030.csv", row.names = FALSE)
  
  # Salvar melhor modelo em objeto separado
  melhor_modelo_id <- 
    tabela_precisao |> 
    slice_min(mae) |> 
    pull(.model_id)
  
  projecao_melhor_modelo <- projecao_2030 |>
    filter(.model_id == melhor_modelo_id)
  
  # Visualizar apenas o melhor modelo
  projecao_melhor_modelo |>
    plot_modeltime_forecast(
      .legend_max_width = 250,
      .conf_interval_show = TRUE,
      .interactive = TRUE,
      .title = paste("Projeção até 2030 com o Melhor Modelo:", 
                     melhor_modelo$.model_desc),
      .x_lab = "Data",
      .y_lab = "Total de Nascidos Vivos")
  
  # 1) Juntar dados reais e projeção em um único dataframe
  # Preparar dados reais para combinar com projecao. Para isso, precisamos renomear algumas colunas da base original
  dados_reais <- 
    df_regiao |>
    select(data_nascimento, total) |>
    rename(
      .index = data_nascimento,
      .value = total
    ) |>
    mutate(
      .model_id = 0,
      .model_desc = "Dados Históricos",
      .key = "actual",
      tipo = "histórico"
    )
  
  # Adicionar coluna de tipo na projeção
  projecao_com_tipo <- 
    projecao_2030 |>
    mutate(tipo = case_when(
      .key == "actual" ~ "histórico",
      TRUE ~ "projeção"
    ))
  
  # Combinar dados reais e projeção
  serie_completa <- 
    bind_rows(
      dados_reais,
      projecao_com_tipo) |>
    # Remover duplicatas potenciais na sobreposição
    distinct(.index, .model_desc, .keep_all = TRUE) |> 
    mutate(cod_regsaude = unique(df_regiao$cod_regsaud), 
           .before = .index)
  
  # 2) Plotar gráfico completo com dados reais e projeção
  grafico_completo <- 
    serie_completa |>
    filter(
      # Manter apenas dados reais históricos e o melhor modelo
      (.model_desc == "Dados Históricos" & tipo == "histórico") |
        (.model_id == melhor_modelo_id & tipo == "projeção")
    ) |>
    ggplot(aes(x = .index, 
               y = .value, 
               color = tipo)) +
    geom_line() +
    geom_point(alpha = 0.3, size = 1) +
    labs(
      title = paste("Série Temporal Completa de Nascidos Vivos (2000-2030) - ", unique(df_regiao$regiao_saude), "-",unique(df_regiao$uf_sigla)),
      subtitle = paste("Histórico + Projeção com", melhor_modelo$.model_desc),
      x = "Data",
      y = "Total de Nascidos Vivos",
      color = "Tipo de Dado"
    ) +
    theme_minimal() +
    scale_color_manual(values = 
                         c("histórico" = "blue", 
                           "projeção" = "red")) +
    # Adicionando linha vertical no ponto de transição
    geom_vline(xintercept = as.numeric(max(dados_reais$.index)), 
               linetype = "dashed", color = "darkgray")
  
  print(grafico_completo)
  
  write.csv(serie_completa, 
            file = paste0("~/GitHub/materno_infantil/02_script/03_outputs_projecoes/data/data_", 
                          unique(serie_completa$cod_regsaude), ".csv"),
            row.names = FALSE)
  
  ggsave(filename = paste0("~/GitHub/materno_infantil/02_script/03_outputs_projecoes/plot_mensal/plot_mensal",  
                           unique(serie_completa$cod_regsaude), ".jpeg"),
         plot = grafico_completo,
         width = 10, 
         height = 7, 
         dpi = 300)
  
  # 3) Extrair tendência anual de toda a série temporal
  # Calcular médias anuais
  tendencia_anual <- serie_completa |>
    mutate(
      ano = lubridate::year(.index),
      mes = lubridate::month(.index)
    ) |>
    group_by(ano, tipo, .model_desc) |>
    summarise(
      total_nascidos = sum(.value, na.rm = TRUE),
      n = n(),
      .groups = "drop")
  
  # Filtrar apenas dados históricos do melhor modelo
  tendencia_filtrada <- tendencia_anual |>
    filter(
      (.model_desc == "Dados Históricos" & 
         tipo == "histórico") |
        (.model_desc == melhor_modelo$.model_desc & 
           tipo == "projeção"))
  
  # Aplicar suavização à tendência para extrair componente de longo prazo
  # Criar uma série temporal contínua de 2000 a 2030
  anos_completos <- 
    data.frame(
      ano = 2000:2030
    )
  
  # Juntar com dados filtrados
  tendencia_suavizada <- 
    tendencia_filtrada |>
    select(ano, total_nascidos, tipo) |>
    right_join(anos_completos, by = "ano") |>
    arrange(ano)
  
  # Aplicar suavização loess para extrair a tendência
  tryCatch({
    # Tentativa de ajustar o modelo loess
    modelo_tendencia <- loess(total_nascidos ~ ano, 
                              data = tendencia_suavizada, 
                              span = 0.5,
                              na.action = na.exclude)
    
    # Faça a previsão com tratamento para garantir o mesmo número de linhas
    tendencia_pred <- predict(modelo_tendencia, newdata = tendencia_suavizada, se = FALSE)
    
    # Verifique se o tamanho da previsão corresponde ao número de linhas
    if (length(tendencia_pred) == nrow(tendencia_suavizada)) {
      tendencia_suavizada$tendencia <- round(tendencia_pred)
    } else {
      # Se houver incompatibilidade de tamanho, use uma abordagem alternativa
      cat("Aviso: Incompatibilidade de tamanho para região", regiao, 
          ". Usando método alternativo para tendência.\n")
      
      # Método alternativo: média móvel simples
      tendencia_suavizada$tendencia <- NA  # Iniciar com NA
      
      # Preencher valores onde temos dados
      anos_com_dados <- tendencia_filtrada$ano
      valores <- tendencia_filtrada$total_nascidos
      
      for (a in anos_com_dados) {
        idx <- which(tendencia_suavizada$ano == a)
        if (length(idx) > 0) {
          tendencia_suavizada$tendencia[idx] <- round(valores[which(anos_com_dados == a)])
        }
      }
      
      # Interpolar valores ausentes
      for (i in 1:nrow(tendencia_suavizada)) {
        if (is.na(tendencia_suavizada$tendencia[i])) {
          # Encontrar valores não-NA antes e depois
          idx_antes <- max(which(!is.na(tendencia_suavizada$tendencia[1:i])), 0)
          idx_depois <- min(which(!is.na(tendencia_suavizada$tendencia[i:nrow(tendencia_suavizada)])) + i - 1, 
                            nrow(tendencia_suavizada))
          
          if (idx_antes > 0 && idx_depois <= nrow(tendencia_suavizada)) {
            # Interpolar linearmente
            val_antes <- tendencia_suavizada$tendencia[idx_antes]
            val_depois <- tendencia_suavizada$tendencia[idx_depois]
            distancia_total <- tendencia_suavizada$ano[idx_depois] - tendencia_suavizada$ano[idx_antes]
            distancia_atual <- tendencia_suavizada$ano[i] - tendencia_suavizada$ano[idx_antes]
            tendencia_suavizada$tendencia[i] <- round(val_antes + 
                                                        (val_depois - val_antes) * (distancia_atual / distancia_total))
          }
        }
      }
    }
  }, error = function(e) {
    # Se ocorrer qualquer erro, use uma abordagem mais simples
    cat("Erro ao calcular tendência para região", regiao, ":", conditionMessage(e), "\n")
    cat("Usando método alternativo simples.\n")
    
    # Usar uma tendência linear simples em vez de loess
    anos <- tendencia_suavizada$ano
    medias_anuais <- tapply(tendencia_filtrada$total_nascidos, tendencia_filtrada$ano, mean, na.rm = TRUE)
    anos_com_dados <- as.numeric(names(medias_anuais))
    
    # Modelo linear simples
    if (length(anos_com_dados) >= 2) {
      modelo_linear <- lm(unname(medias_anuais) ~ anos_com_dados)
      tendencia_suavizada$tendencia <- round(predict(modelo_linear, 
                                                     newdata = data.frame(anos_com_dados = anos)))
    } else {
      # Se não tivermos dados suficientes, apenas use a média
      media_geral <- mean(tendencia_filtrada$total_nascidos, na.rm = TRUE)
      tendencia_suavizada$tendencia <- round(rep(media_geral, nrow(tendencia_suavizada)))
    }
  })
  
  # 4) Plotar apenas a tendência de toda a série temporal
  # Modificação na parte do gráfico de tendência
  grafico_tendencia <- 
    ggplot() +
    # Dados originais históricos
    geom_point(data = tendencia_filtrada %>% filter(tipo == "histórico"),
               aes(x = ano, y = total_nascidos, color = tipo), 
               size = 3, alpha = 0.7) +
    geom_line(data = tendencia_filtrada %>% filter(tipo == "histórico"),
              aes(x = ano, y = total_nascidos, color = tipo, linetype = tipo)) +
    
    # Dados de projeção (separados para garantir que sejam plotados)
    geom_point(data = tendencia_filtrada %>% filter(tipo == "projeção"),
               aes(x = ano, y = total_nascidos, color = tipo), 
               size = 3, alpha = 0.7) +
    geom_line(data = tendencia_filtrada %>% filter(tipo == "projeção"),
              aes(x = ano, y = total_nascidos, color = tipo, linetype = tipo)) +
    
    # Tendência suavizada
    geom_smooth(data = tendencia_suavizada %>% filter(!is.na(total_nascidos)), 
                aes(x = ano, y = total_nascidos), 
                method = "loess", span = 0.5,
                se = TRUE, color = "darkblue", linetype = "dashed") +
    
    # Linha vertical no ponto de transição
    geom_vline(xintercept = 2023, linetype = "dotted") +
    
    # Anotações
    annotate("text", x = 2011, 
             y = max(tendencia_filtrada$total_nascidos, na.rm = TRUE),
             label = "Dados históricos", 
             color = "blue", 
             hjust = 0.5) +
    
    annotate("text", x = 2026, 
             y = max(tendencia_filtrada$total_nascidos, na.rm = TRUE),
             label = "Projeção", 
             color = "red", 
             hjust = 0.5) +
    
    # Estética  
    scale_color_manual(values = c("histórico" = "blue", 
                                  "projeção" = "red")) +
    scale_linetype_manual(values = c("histórico" = "solid", 
                                     "projeção" = "dashed")) +
    
    labs(
      title = paste("Série Temporal Completa de Nascidos Vivos (2000-2030) - ", 
                    unique(df_regiao$regiao_saude), "-", unique(df_regiao$uf_sigla)), 
      subtitle = "Dados históricos e projeção com tendência suavizada",
      x = "Ano",
      y = "Média Anual de Nascidos Vivos"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = seq(2000, 2030, by = 5))  
  grafico_tendencia
  
  ggsave(filename = paste0("~/GitHub/materno_infantil/02_script/03_outputs_projecoes/plot_anual/plot_anual_",
                           unique(serie_completa$cod_regsaude), ".jpeg"),
         plot = grafico_tendencia,
         width = 10, 
         height = 7, 
         dpi = 300)

}

regioes <- nascidos |> 
                filter(uf_sigla == "GO") 
                
regioes <- c("31004","15014","29016","31092","21008",
             "21018","31086","33008","43026","26009")

for (regiao in regioes) {
  # Exibir mensagem informativa (opcional)
  cat("Processando região:", regiao, "\n")
  
  # Chamar a função projecao para a região atual
  projecao(regiao)
  
  # Opcionalmente, você pode adicionar um pequeno intervalo para evitar sobrecarga
  # Sys.sleep(0.5)
}

projecao(regiao = "31092")

