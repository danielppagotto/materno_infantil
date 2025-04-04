# Simulação de Monte Carlo otimizada para a função oferta_vs_demanda
library(tidyverse)
library(future)
library(furrr)
library(tictoc)

# Carregando dados que vão alimentar a funcao -----------------------------

servicos <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script/04_servicos/servicos_2019.csv") |> 
  select(-`...1`) 

oferta <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_aps_2019.csv") |> 
  select(-`...1`) 

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) 

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc)



# Função criada para rodar simulação --------------------------------------
# ao final a função traz os resultados sintetizados e desagregados

# Função executar_simulacao modificada para retornar resultados desagregados
executar_simulacao_desagregado <- function(acoes_educacionais, 
                                           consultas, 
                                           ttd, 
                                           indireta, 
                                           todos,
                                           servicos_data, 
                                           cobertura_data, 
                                           oferta_data, 
                                           foco_clinico_data) {
  # Usamos os dataframes passados como argumentos para evitar problemas com variáveis globais
  servicos <- servicos_data
  cobertura <- cobertura_data
  oferta <- oferta_data
  foco_clinico <- foco_clinico_data
  
  # Verificações de colunas omitidas para brevidade
  
  # Filtrar procedimentos de 2019
  servicos2019 <- 
    servicos |> 
    mutate(ano_proc_rea = year(mes_proc_rea)) |> 
    filter(ano_proc_rea == 2019) |> 
    filter(nivel_atencao == "APS" & 
             publico != "Gestantes de Alto Risco") |>
    filter(tipo_procedimento == "Ações Educacionais" |
             tipo_procedimento == "Consultas ou Visitas") |> 
    filter(procedimento != "Avaliação odontológica") |> 
    filter(procedimento != "Visita domiciliar") 
  
  cobertura$cod_regsaud <- as.numeric(cobertura$cod_regsaud)
  
  # Juntar dados de serviços com cobertura
  servicos19_tratado <- 
    servicos2019 |> 
    left_join(cobertura, 
              by = c("cod_regsaude" = "cod_regsaud")) |> 
    janitor::clean_names() |> 
    mutate(cobertura = cobertura/100) |> 
    rename(cobertura_ans = cobertura) 
  
  # Aplicar filtro com base no parâmetro 'todos'
  sus <- 1
  
  if(sus == todos){
    servicos19_tratado <- servicos19_tratado |> 
      mutate(qtd_proc = qtd_proc)
  } else {
    servicos19_tratado <- servicos19_tratado |>
      mutate(qtd_proc = (qtd_nascidos - 
                           (qtd_nascidos * cobertura_ans)) * 
               parametro)
  }
  
  servicos19_tratado <- 
    servicos19_tratado |> 
    select(ano_proc_rea, uf_sigla, cod_regsaude, 
           regiao_saude, qtd_nascidos, cobertura_ans, 
           procedimento, tipo_procedimento, publico, 
           nivel_atencao, parametro, mes_proc_rea, 
           qtd_proc)
  
  # Traduzir número de horas em número de profissionais necessários
  necessidade <- 
    servicos19_tratado |> 
    mutate(total_horas = 
             case_when(tipo_procedimento == 
                         "Ações Educacionais" ~ 
                         qtd_proc * acoes_educacionais/60,
                       tipo_procedimento == 
                         "Consultas ou Visitas" ~ 
                         qtd_proc * consultas/60)) |> 
    mutate(nec_prof = total_horas/ttd) |> 
    group_by(uf_sigla, cod_regsaude, regiao_saude,  
             mes_proc_rea) |> 
    summarise(nec_prof = sum(nec_prof),
              nec_ch = sum(total_horas), .groups = "drop") |> 
    mutate(mes = month(mes_proc_rea),
           ano = year(mes_proc_rea), 
           .after = mes_proc_rea)
  
  # Juntar necessidades com oferta
  oferta_vs_demanda_result <- 
    necessidade |> 
    left_join(oferta, 
              by = c("cod_regsaude" = "cod_regsaud",
                     "mes" = "mes",
                     "ano" = "ano")) |> 
    left_join(foco_clinico, 
              by = c("cod_regsaude" = "cod_regsaud")) |> 
    mutate(oferta_direta = fte40 * (1 - indireta),
           oferta_linha = oferta_direta * perc_fc) |> 
    mutate(total_abs = oferta_linha - nec_prof,
           total_perc = 100 * (oferta_linha/nec_prof)) |> 
    group_by(uf_sigla, cod_regsaude, regiao_saude) |> 
    summarise(necessidade_media = mean(nec_prof, na.rm = TRUE),
              oferta_media = mean(oferta_linha, na.rm = TRUE),
              .groups = "drop") |> 
    mutate(perc = 100 * (oferta_media/necessidade_media)) |> 
    mutate(total = (oferta_media - necessidade_media))
  
  resultado <- list(
    por_regiao = oferta_vs_demanda_result,
    necessidade_media = mean(oferta_vs_demanda_result$necessidade_media, 
                             na.rm = TRUE),
    oferta_media = mean(oferta_vs_demanda_result$oferta_media, 
                        na.rm = TRUE),
    perc = mean(oferta_vs_demanda_result$perc, 
                na.rm = TRUE),
    total = mean(oferta_vs_demanda_result$total, 
                 na.rm = TRUE)
  )
  
  return(resultado)
}


# Função principal para executar a simulação de Monte Carlo ---------------


executar_monte_carlo_desagregado <- function(n_sim = 1000, 
                                             acoes_min = 22.5, 
                                             acoes_max = 50,
                                             consultas_min = 25, 
                                             consultas_max = 35,
                                             ttd_min = 113, 
                                             ttd_max = 153,
                                             indireta_min = 0.30, 
                                             indireta_max = 0.50,
                                             prob_todos_0 = 0.5, 
                                             prob_todos_1 = 0.5,
                                             servicos, 
                                             cobertura, 
                                             oferta, 
                                             foco_clinico) {
  
  # Verificar e ajustar os dataframes
  cat("Verificando estrutura dos dataframes...\n")
  # Verificar oferta
  if(!all(c("mes", "ano", "cod_regsaud") %in% colnames(oferta))) {
    cat("Ajustando dataframe 'oferta'...\n")
    if(!("cod_regsaud" %in% colnames(oferta)) && ("cod_regsaude" %in% colnames(oferta))) {
      oferta <- oferta |> rename(cod_regsaud = cod_regsaude)
    }
    # Se ainda faltar mes e ano, verificar se existe mes_proc_rea
    if(!all(c("mes", "ano") %in% colnames(oferta)) && ("mes_proc_rea" %in% colnames(oferta))) {
      oferta <- oferta |>
        mutate(mes = month(mes_proc_rea),
               ano = year(mes_proc_rea))
    }
  }
  
  # Verificar foco_clinico
  if(!("cod_regsaud" %in% colnames(foco_clinico)) && 
     ("cod_regsaude" %in% colnames(foco_clinico))) {
    cat("Ajustando dataframe 'foco_clinico'...\n")
    foco_clinico <- foco_clinico |> 
      rename(cod_regsaud = cod_regsaude)
  }
  
  # Informar início
  cat("Iniciando simulação de Monte Carlo com", n_sim, "iterações\n")
  cat("Usando", 
      future::nbrOfWorkers(), "workers para processamento paralelo\n")
  
  # Definir semente para reprodutibilidade
  set.seed(123)
  
  # Criar dataframe com os parâmetros para cada simulação
  parametros_simulacao <- tibble(
    simulacao = 1:n_sim,
    acoes_educacionais = runif(n_sim, min = acoes_min, max = acoes_max),
    consultas = runif(n_sim, min = consultas_min, max = consultas_max),
    ttd = runif(n_sim, min = ttd_min, max = ttd_max),
    indireta = runif(n_sim, min = indireta_min, max = indireta_max),
    todos = sample(c(0, 1), size = n_sim, replace = TRUE, 
                   prob = c(prob_todos_0, prob_todos_1))
  )
  
  # Iniciar medição de tempo
  tic("Execução da simulação Monte Carlo desagregada em paralelo")
  
  # Executar simulação em paralelo
  resultados <- future_map(1:n_sim, function(i) {
    executar_simulacao_desagregado(
      acoes_educacionais = parametros_simulacao$acoes_educacionais[i],
      consultas = parametros_simulacao$consultas[i],
      ttd = parametros_simulacao$ttd[i],
      indireta = parametros_simulacao$indireta[i],
      todos = parametros_simulacao$todos[i],
      servicos_data = servicos,
      cobertura_data = cobertura,
      oferta_data = oferta,
      foco_clinico_data = foco_clinico
    )
  }, .options = furrr_options(seed = TRUE))
  
  # Finalizar medição de tempo
  toc()
  
  # Extrair resultados agregados
  resultados_df <- tibble(
    simulacao = 1:n_sim,
    necessidade_media = map_dbl(resultados, ~.x$necessidade_media),
    oferta_media = map_dbl(resultados, ~.x$oferta_media),
    percentual_cobertura = map_dbl(resultados, ~.x$perc),
    diferenca_total = map_dbl(resultados, ~.x$total)
  )
  
  # Lista de resultados por região para cada simulação
  resultados_por_regiao <- map(1:n_sim, function(i) {
    reg_result <- resultados[[i]]$por_regiao
    # Adicionar o número da simulação e os parâmetros utilizados
    reg_result |>
      mutate(
        simulacao = i,
        acoes_educacionais = parametros_simulacao$acoes_educacionais[i],
        consultas = parametros_simulacao$consultas[i],
        ttd = parametros_simulacao$ttd[i],
        indireta = parametros_simulacao$indireta[i],
        todos = parametros_simulacao$todos[i]
      )
  })
  
  # Combinar todos os resultados regionais em um único dataframe
  resultados_regioes_completo <- 
                bind_rows(resultados_por_regiao)
  
  # Combinar resultados agregados com parâmetros
  resultados_completos <- 
    bind_cols(
      parametros_simulacao,
      resultados_df |> select(-simulacao))
  
  # Análise dos resultados
  resumo_simulacao <- 
    resultados_completos |>
      summarise(
        media_necessidade = mean(necessidade_media, na.rm = TRUE),
        media_oferta = mean(oferta_media, na.rm = TRUE),
        media_percentual = mean(percentual_cobertura, na.rm = TRUE),
        media_diferenca = mean(diferenca_total, na.rm = TRUE),
        mediana_necessidade = median(necessidade_media, na.rm = TRUE),
        mediana_oferta = median(oferta_media, na.rm = TRUE),
        mediana_percentual = median(percentual_cobertura, na.rm = TRUE),
        mediana_diferenca = median(diferenca_total, na.rm = TRUE),
        sd_necessidade = sd(necessidade_media, na.rm = TRUE),
        sd_oferta = sd(oferta_media, na.rm = TRUE),
        sd_percentual = sd(percentual_cobertura, na.rm = TRUE),
        sd_diferenca = sd(diferenca_total, na.rm = TRUE),
        q05_percentual = quantile(percentual_cobertura, 0.05, na.rm = TRUE),
        q95_percentual = quantile(percentual_cobertura, 0.95, na.rm = TRUE)
    )
  
  # NOVO: Resumo por região de saúde
  resumo_por_regiao <- 
    resultados_regioes_completo |>
      group_by(uf_sigla, cod_regsaude, regiao_saude) |>
      summarise(
        media_necessidade = mean(necessidade_media, na.rm = TRUE),
        media_oferta = mean(oferta_media, na.rm = TRUE),
        media_percentual = mean(perc, na.rm = TRUE),
        media_diferenca = mean(total, na.rm = TRUE),
        mediana_necessidade = median(necessidade_media, na.rm = TRUE),
        mediana_oferta = median(oferta_media, na.rm = TRUE),
        mediana_percentual = median(perc, na.rm = TRUE),
        mediana_diferenca = median(total, na.rm = TRUE),
        sd_necessidade = sd(necessidade_media, na.rm = TRUE),
        sd_oferta = sd(oferta_media, na.rm = TRUE),
        sd_percentual = sd(perc, na.rm = TRUE),
        sd_diferenca = sd(total, na.rm = TRUE),
        q05_percentual = quantile(perc, 0.05, na.rm = TRUE),
        q95_percentual = quantile(perc, 0.95, na.rm = TRUE),
        .groups = "drop"
      )
  
  # Análise de sensibilidade
  sensibilidade <- cor(resultados_completos |> 
                         select(acoes_educacionais, 
                                consultas, ttd, indireta, 
                                todos, percentual_cobertura),
                       use = "pairwise.complete.obs")
  
  # Resultados por valor de 'todos'
  resultados_por_todos <- resultados_completos |>
    group_by(todos) |>
    summarise(
      media_percentual = mean(percentual_cobertura, na.rm = TRUE),
      mediana_percentual = median(percentual_cobertura, na.rm = TRUE),
      sd_percentual = sd(percentual_cobertura, na.rm = TRUE),
      q05_percentual = quantile(percentual_cobertura, 0.05, na.rm = TRUE),
      q95_percentual = quantile(percentual_cobertura, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  # NOVO: Resultados por região e valor de 'todos'
  resultados_regiao_todos <- resultados_regioes_completo |>
    group_by(uf_sigla, cod_regsaude, regiao_saude, todos) |>
    summarise(
      media_percentual = mean(perc, na.rm = TRUE),
      mediana_percentual = median(perc, na.rm = TRUE),
      sd_percentual = sd(perc, na.rm = TRUE),
      q05_percentual = quantile(perc, 0.05, na.rm = TRUE),
      q95_percentual = quantile(perc, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Visualizações
  # Histograma de percentual de cobertura
  hist_perc <- 
    ggplot(resultados_completos, 
           aes(x = percentual_cobertura)) +
    geom_histogram(bins = 30, 
                   fill = "steelblue", 
                   color = "white") +
    geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
    labs(title = "Distribuição do percentual do equilíbrio entre oferta e necessidade",
         x = "Percentual de equilíbrio (%)",
         y = "Frequência") +
    theme_minimal()
  
  # Análise de sensibilidade gráfica
  parametro_maior_impacto <- colnames(sensibilidade)[
    which.max(abs(sensibilidade["percentual_cobertura", 
                                -which(colnames(sensibilidade) == "percentual_cobertura")]))
  ]
  
  plot_sensibilidade <- 
    ggplot(resultados_completos,
           aes_string(x = parametro_maior_impacto, 
                      y = "percentual_cobertura")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    labs(title = paste("Impacto de", parametro_maior_impacto, "no percentual de cobertura"),
         y = "Percentual de cobertura (%)") +
    theme_minimal()
  
  # NOVO: Mapa de calor do percentual médio de cobertura por região
  # Primeiro, ordenamos as regiões por UF e nome da região
  regioes_ordenadas <- resumo_por_regiao |>
    arrange(uf_sigla, regiao_saude) |>
    mutate(regiao_id = paste(uf_sigla, regiao_saude, sep = " - "))
  
  # Definir as 20 regiões com menor cobertura para visualização
  regioes_pior_cobertura <- regioes_ordenadas |>
    arrange(media_percentual) |>
    head(20) |>
    pull(regiao_id)
  
  # Filtramos os dados para incluir apenas essas regiões
  dados_heatmap <- resultados_regioes_completo |>
    mutate(regiao_id = paste(uf_sigla, 
                             regiao_saude, 
                             sep = " - ")) |>
    filter(regiao_id %in% regioes_pior_cobertura)
  
  # Criamos o heatmap das regiões com pior cobertura
  plot_heatmap <- 
    
    ggplot(dados_heatmap,
           aes(x = factor(simulacao), 
               y = reorder(regiao_id, -perc), 
               fill = perc)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", 
                         mid = "yellow", 
                         high = "green", 
                         midpoint = 100, 
                         name = "Cobertura (%)") +
    labs(title = "Percentual de cobertura por simulação para regiões críticas",
         x = "Simulação", 
         y = "Região de Saúde") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # Retornar todos os resultados em uma lista
  return(list(
    parametros = parametros_simulacao,
    resultados = resultados_completos,
    resultados_por_regiao = resultados_regioes_completo,  # NOVO
    resumo = resumo_simulacao,
    resumo_por_regiao = resumo_por_regiao,  # NOVO
    sensibilidade = sensibilidade,
    resultados_por_todos = resultados_por_todos,
    resultados_regiao_todos = resultados_regiao_todos,  # NOVO
    graficos = list(
      histograma_percentual = hist_perc,
      sensibilidade = plot_sensibilidade,
      heatmap_regioes = plot_heatmap  # NOVO
    )
  ))
}



# Exemplo de como chamar a função
resultado_mc <- executar_monte_carlo_desagregado(
  n_sim = 10000,
  acoes_min = 22.5, acoes_max = 37.5,
  consultas_min = 20, consultas_max = 40,
  ttd_min = 113, ttd_max = 153,
  indireta_min = 0.30, indireta_max = 0.50,
  prob_todos_0 = 0.5, prob_todos_1 = 0.5,
  servicos = servicos,
  cobertura = cobertura,
  oferta = oferta,
  foco_clinico = foco_clinico
)

# Funções auxiliares para análise de resultados desagregados

# Identificar regiões com cobertura crítica (abaixo de um limiar)
identificar_regioes_criticas <- function(resultado_mc, 
                                         limiar_percentual = 50) {
  regioes_criticas <- resultado_mc$resumo_por_regiao |>
    filter(media_percentual < limiar_percentual) |>
    arrange(media_percentual)
  
  return(regioes_criticas)
}

# Analisar sensibilidade para uma região específica
analisar_sensibilidade_regiao <- 
  
  function(resultado_mc, cod_regsaude_alvo) {
  
  # Filtrar dados para a região específica
  dados_regiao <- resultado_mc$resultados_por_regiao |>
    filter(cod_regsaude == cod_regsaude_alvo)
  
  # Calcular correlações para esta região
  correlacoes <- cor(dados_regiao |>
                       select(acoes_educacionais, 
                              consultas, 
                              ttd, indireta, 
                              todos, perc),
                     use = "pairwise.complete.obs")
  
  # Criar gráficos de dispersão para visualizar relações
  parametros <- c("acoes_educacionais", "consultas", "ttd", "indireta", "todos")
  plots <- list()
  
  for (param in parametros) {
    p <- ggplot(dados_regiao, 
                aes_string(x = param, 
                           y = "perc")) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm") +
      labs(title = paste("Impacto de", param, "no percentual de balanceamento", 
                         unique(dados_regiao$regiao_saude)),
           y = "Percentual de balanceamento (%)",
           subtitle = paste("Correlação:", 
                            round(correlacoes["perc", param], 3))) +
      theme_minimal()
    
    plots[[param]] <- p
  }
  
  return(list(
    correlacoes = correlacoes,
    plots = plots,
    dados = dados_regiao
  ))
}

# Comparar distribuições de cobertura entre regiões
comparar_regioes <- function(resultado_mc, 
                             codigos_regsaude) {
  # Filtrar dados para as regiões especificadas
  
  dados_regioes <- resultado_mc$resultados_por_regiao |>
    filter(cod_regsaude %in% codigos_regsaude)
  
  # Criar gráfico de densidade comparativo
  plot_densidade <- 
    ggplot(dados_regioes, aes(x = perc, 
                              fill = regiao_saude)) +
    geom_density(alpha = 0.4) +
    geom_vline(xintercept = 100, 
               linetype = "dashed") +
    labs(title = "Comparação da distribuição de cobertura entre regiões",
         x = "Percentual de cobertura (%)",
         y = "Densidade") +
    theme_minimal()
  
  # Criar boxplot comparativo
  plot_boxplot <- ggplot(dados_regioes, 
                         aes(x = reorder(regiao_saude, 
                                         -perc, median), 
                                         y = perc, 
                             fill = regiao_saude)) +
    geom_boxplot() +
    geom_hline(yintercept = 100, 
               linetype = "dashed") +
    labs(title = "Comparação da distribuição de cobertura entre regiões",
         x = "Região de saúde",
         y = "Percentual de cobertura (%)") +
    theme_minimal() +
    theme(axis.text.x = 
          element_text(angle = 45, 
                       hjust = 1),
          legend.position = "none")
  
  return(list(
    densidade = plot_densidade,
    boxplot = plot_boxplot,
    dados = dados_regioes
  ))
}

estado <- cobertura |> 
        filter(uf_sigla == "MG") |> 
        distinct(cod_regsaud)

estado <- estado$cod_regsaud

estado <- as.character(estado)

teste <- comparar_regioes(resultado_mc, 
                          codigos_regsaude = estado)

teste

dados <- teste$dados

analisar_sensibilidade_regiao(resultado_mc, 
                              cod_regsaude_alvo = "52001")

criticos <- identificar_regioes_criticas(resultado_mc, 
                                        limiar_percentual = 50)

analisar_sensibilidade_regiao(resultado_mc,
                              cod_regsaude_alvo = "35073")


# Exportando resultados ---------------------------------------------------

resultado_resumo_por_regiao <- resultado_mc[["resumo_por_regiao"]]

write.csv(resultado_resumo_por_regiao,
          "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resumo_regiao.csv")

parametros_usados <- resultado_mc[["parametros"]]

write.csv(parametros_usados, 
          "~/GitHub/materno_infantil/02_script/07_output_montecarlo/parametros_usados.csv")

resultados_regioes <- resultado_mc[["resultados_por_regiao"]]

write.csv(resultados_regioes, 
          "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_regioes.csv")

arrow::write_parquet(resultados_regioes,
                     "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_regioes.parquet")

