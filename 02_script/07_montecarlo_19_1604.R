# Simulação de Monte Carlo otimizada para a função oferta_vs_demanda
library(tidyverse)
library(future)
library(furrr)
library(tictoc)
library(sandwich)
library(lmtest)
library(modelsummary)

options(scipen = 999)

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
                                           absenteismo, 
                                           indireta, 
                                           todos,
                                           servicos_data, 
                                           cobertura_data, 
                                           oferta_data, 
                                           foco_clinico_data,
                                           alto_risco,
                                           acoes_altorisco,
                                           consultas_altorisco) {
  # Usamos os dataframes passados como argumentos para evitar problemas com variáveis globais
  servicos <- servicos_data
  cobertura <- cobertura_data
  oferta <- oferta_data
  foco_clinico <- foco_clinico_data
  
  # Verificações de colunas omitidas para brevidade
  
  servicos2019 <- 
    servicos |> 
    mutate(ano_proc_rea = 
             year(mes_proc_rea)) |> 
    filter(ano_proc_rea == 2019) |> 
    filter(nivel_atencao == "APS" & 
             publico != "Gestantes de Alto Risco") |>
    filter(tipo_procedimento == "Ações Educacionais" |
             tipo_procedimento == "Consultas ou Visitas") |> 
    filter(procedimento != "Avaliação odontológica") |> 
    filter(procedimento != "Visita domiciliar") |> 
    filter(mes_programado < 36)
  
  cobertura$cod_regsaud <- 
    as.numeric(cobertura$cod_regsaud)
  
  # Juntar dados de serviços com cobertura
  
  servicos19_tratado <- 
    servicos2019 |> 
    left_join(cobertura, 
              by = c("cod_regsaude"=
                       "cod_regsaud")) |> 
    janitor::clean_names() |> 
    mutate(cobertura = cobertura/100) |> 
    rename(cobertura_ans = cobertura) 
  
  sus = 1
  
  if(sus == todos){
    servicos19_tratado <- servicos19_tratado |> 
      
      mutate(qtd_proc_rh = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (1-alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (1-alto_risco),
                 TRUE ~ qtd_proc
               )) |> 
      mutate(qtd_proc_ar = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (alto_risco),
                 TRUE ~ 0
               ) 
             
      )
  } else {
    servicos19_tratado <- 
      servicos19_tratado |>
      mutate(qtd_proc = (qtd_nascidos - 
                           (qtd_nascidos * cobertura_ans)) * 
               parametro) |> 
      mutate(qtd_proc_rh = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (1-alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (1-alto_risco),
                 TRUE ~ qtd_proc
               )) |> 
      mutate(qtd_proc_ar = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (alto_risco),
                 TRUE ~ 0
               )) 
  }
  
  servicos_tratado <- 
    servicos19_tratado |> 
    select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaude, 
           regiao_saude, qtd_nascidos, cobertura_ans, sigtap_recod,
           procedimento, tipo_procedimento, mes_programado,
           publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
           qtd_proc_ar)
  
  # Traduzir número de horas em número de profissionais necessários
  ferias <- 16
  feriados <- 4
  ttd <- 160 - ferias - feriados - absenteismo
  
  necessidade <- 
    servicos_tratado |> 
    mutate(total_horas_rh = 
             case_when(tipo_procedimento == 
                         "Ações Educacionais" ~ 
                         qtd_proc * acoes_educacionais/60,
                       tipo_procedimento == 
                         "Consultas ou Visitas" ~ 
                         qtd_proc * consultas/60)) |> 
    mutate(total_horas_ar = 
             case_when(tipo_procedimento == 
                         "Ações Educacionais" ~ 
                         qtd_proc_ar * acoes_altorisco/60,
                       tipo_procedimento == 
                         "Consultas ou Visitas" ~ 
                         qtd_proc_ar * consultas_altorisco/60)) |> 
    mutate(nec_prof = (total_horas_rh + total_horas_ar)/ttd) |> 
    group_by(uf_sigla, cod_regsaude, regiao_saude,  
             mes_proc_rea) |> 
    summarise(nec_prof = sum(nec_prof),
              nec_ch = sum(total_horas_rh + total_horas_ar)) |> 
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


executar_monte_carlo_desagregado <- function(n_sim = 100, 
                                             acoes_min = 20, 
                                             acoes_max = 40,
                                             consultas_min = 20, 
                                             consultas_max = 40,
                                             absenteismo_min = 8, 
                                             absenteismo_max = 24,
                                             indireta_min = 0.40, 
                                             indireta_max = 0.50,
                                             prob_todos_0 = 0.5, 
                                             prob_todos_1 = 0.5,
                                             alto_risco_min = 0.10,
                                             alto_risco_max = 0.20,
                                             consultas_altorisco_min = 30,
                                             consultas_altorisco_max = 45,
                                             acoes_altorisco_min = 25,
                                             acoes_altorisco_max = 45,
                                             servicos, 
                                             cobertura, 
                                             oferta, 
                                             foco_clinico){
  
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
    acoes_educacionais = runif(n_sim, 
                               min = acoes_min, 
                               max = acoes_max),
    consultas = runif(n_sim, 
                      min = consultas_min, 
                      max = consultas_max),
    absenteismo = runif(n_sim, 
                        min = absenteismo_min, 
                        max = absenteismo_max),
    alto_risco = runif(n_sim, 
                       min = alto_risco_min, 
                       max = alto_risco_max),
    acoes_altorisco = runif(n_sim, 
                            min = acoes_altorisco_min, 
                            max = acoes_altorisco_max),
    consultas_altorisco = runif(n_sim, 
                               min = consultas_altorisco_min,
                               max = consultas_altorisco_max),
    indireta = runif(n_sim, 
                     min = indireta_min, max = indireta_max),
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
      absenteismo = parametros_simulacao$absenteismo[i],
      indireta = parametros_simulacao$indireta[i],
      todos = parametros_simulacao$todos[i],
      alto_risco = parametros_simulacao$alto_risco[i],
      acoes_altorisco = parametros_simulacao$acoes_altorisco[i],
      consultas_altorisco = parametros_simulacao$consultas_altorisco[i],
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
        absenteismo = parametros_simulacao$absenteismo[i],
        indireta = parametros_simulacao$indireta[i],
        todos = parametros_simulacao$todos[i],
        alto_risco = parametros_simulacao$alto_risco[i],
        acoes_altorisco = parametros_simulacao$acoes_altorisco[i],
        consultas_altorisco = parametros_simulacao$consultas_altorisco[i]
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
                                consultas, 
                                absenteismo, 
                                indireta, 
                                todos,
                                alto_risco,
                                acoes_altorisco,
                                consultas_altorisco, 
                                percentual_cobertura),
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
  acoes_min = 20, 
  acoes_max = 40,
  consultas_min = 20, 
  consultas_max = 40,
  absenteismo_min = 8, 
  absenteismo_max = 24,
  indireta_min = 0.40, 
  indireta_max = 0.50,
  prob_todos_0 = 0.5, 
  prob_todos_1 = 0.5,
  alto_risco_min = 0.10,
  alto_risco_max = 0.20,
  consultas_altorisco_min = 30,
  consultas_altorisco_max = 45,
  acoes_altorisco_min = 25,
  acoes_altorisco_max = 45,
  servicos = servicos,
  cobertura = cobertura,
  oferta = oferta,
  foco_clinico = foco_clinico
)

# Funções auxiliares para análise de resultados desagregados

# Identificar regiões com cobertura crítica (abaixo de um limiar)
identificar_regioes_criticas <- 
  
  function(resultado_mc, 
           limiar_percentual = 50) {
  regioes_criticas <- resultado_mc$resumo_por_regiao |>
    filter(media_percentual < limiar_percentual) |>
    arrange(media_percentual)
  
  return(regioes_criticas)
}

# Analisar sensibilidade para uma região específica
analisar_sensibilidade_regiao <- 
  
  function(resultado_mc, 
           cod_regsaude_alvo) {
  
  # Filtrar dados para a região específica
  dados_regiao <- 
    resultado_mc$resultados_por_regiao |>
    filter(cod_regsaude == cod_regsaude_alvo)
  
  # Calcular correlações para esta região
  correlacoes <- cor(dados_regiao |>
                       select(acoes_educacionais, 
                              consultas, 
                              absenteismo, 
                              indireta, 
                              todos,
                              alto_risco,
                              acoes_altorisco,
                              consultas_altorisco, 
                              perc),
                     use = "pairwise.complete.obs")
  
  # Criar gráficos de dispersão para visualizar relações
  parametros <- c("acoes_educacionais", 
                  "consultas", 
                  "absenteismo", 
                  "indireta", 
                  "todos",
                  "alto_risco",
                  "acoes_altorisco",
                  "consultas_altorisco", 
                  "perc")
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


# sensibilidade geral -----------------------------------------------------

analisar_sensibilidade_geral <- function(resultado_mc) {
  # Usar todos os dados, sem filtrar por região
  dados_completos <- resultado_mc$resultados_por_regiao
  
  # Calcular correlações para todos os dados
  correlacoes <- cor(dados_completos |>
                       select(acoes_educacionais, 
                              consultas, 
                              absenteismo, 
                              indireta, 
                              todos,
                              alto_risco,
                              acoes_altorisco,
                              consultas_altorisco, 
                              perc),
                     use = "pairwise.complete.obs")
  
  # Criar gráficos de dispersão para visualizar relações
  parametros <- c("acoes_educacionais", 
                  "consultas", 
                  "absenteismo", 
                  "indireta", 
                  "todos",
                  "alto_risco",
                  "acoes_altorisco",
                  "consultas_altorisco")
  plots <- list()
  
  for (param in parametros) {
    p <- ggplot(dados_completos, 
                aes_string(x = param, 
                           y = "perc")) +
      geom_point(alpha = 0.1) +  # Reduzir alpha porque haverá muitos pontos
      geom_smooth(method = "lm") +
      labs(title = paste("Impacto de", param, "no percentual de balanceamento geral"),
           y = "Percentual de balanceamento (%)",
           subtitle = paste("Correlação:", 
                            round(correlacoes["perc", param], 3))) +
      theme_minimal()
    
    plots[[param]] <- p
  }
  
  # Criar um heatmap das correlações
  cor_matrix <- correlacoes[1:8, 9, drop = FALSE]  # Pegar apenas correlações com perc
  cor_df <- data.frame(
    Parametro = rownames(cor_matrix),
    Correlacao = cor_matrix[,1]
  ) |>
    arrange(desc(abs(Correlacao)))
  
  plot_cor <- ggplot(cor_df, aes(x = reorder(Parametro, abs(Correlacao)), 
                                 y = "Correlação com % balanceamento", 
                                 fill = Correlacao)) +
    geom_tile() +
    geom_text(aes(label = round(Correlacao, 3)), color = "white") +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    theme_minimal() +
    labs(title = "Correlações entre parâmetros e percentual de balanceamento",
         x = "Parâmetro",
         y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Adicionar um gráfico resumo das correlações em barras
  plot_bar <- ggplot(cor_df, aes(x = reorder(Parametro, abs(Correlacao)), 
                                 y = Correlacao)) +
    geom_col(aes(fill = Correlacao > 0)) +
    scale_fill_manual(values = c("red", "blue"), 
                      labels = c("Negativa", "Positiva"), 
                      name = "Correlação") +
    labs(title = "Magnitude das correlações com percentual de balanceamento",
         x = "Parâmetro",
         y = "Coeficiente de correlação") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  
  return(list(
    correlacoes = correlacoes,
    plots = plots,
    heatmap = plot_cor,
    barplot = plot_bar,
    dados = dados_completos
  ))
}





# 1. Comparar distribuições de cobertura entre regiões
comparar_regioes <- 
                    function(resultado_mc, 
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


# 2. sensibilidade geral -----------------------------------------------------

sensibilidade_geral <- 
  analisar_sensibilidade_geral(resultado_mc = resultado_mc)


# 3. comparando regioes ------------------------------------------------------

estado <- cobertura |> 
         filter(uf_sigla == "MA") |> 
         distinct(cod_regsaud)
 
estado <- estado$cod_regsaud
 
estado <- as.character(estado)
 
teste <- comparar_regioes(resultado_mc, 
                           codigos_regsaude = estado)
 
 
dados <- teste$dados

teste

# 4. analise de sensibilidade por regiao -------------------------------------

dados_regiao <- analisar_sensibilidade_regiao(resultado_mc,
                               cod_regsaude_alvo = "13005")

dados_regiao <- dados_regiao$dados 


dados_regiao <- dados_regiao |> 
  mutate(regiao = case_when(uf_sigla %in% c("MG", "SP", "RJ", 
                                            "SP") ~ "Sudeste", 
                            uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
                            uf_sigla %in% c("AC", "AM", "AP", 
                                            "PA", "RO", "RR", "TO") ~ "Norte",
                            uf_sigla %in% c("AL", "BA", "CE", 
                                            "MA", "PB", "PE", 
                                            "PI", "RN", "SE") ~ "Nordeste",
                            uf_sigla %in% c("DF", "GO", 
                                            "MT","MS") ~ "Centro-Oeste")) 

dados_regiao |> 
  ggplot(aes(x = perc, fill = regiao)) + 
  geom_histogram(
    color = "black") + 
  geom_vline(xintercept = median(dados_regiao$perc), 
             color = "red", 
             linetype = "dashed") +
  xlim(0,150) + 
  xlab("Resultado relativo (%)") + 
  ylab("Frequência") +
  theme_minimal() + 
  ggtitle(paste0("Distribuição de resultados da região de saúde - ", 
                 dados_regiao$regiao_saude, " - ", 
                 dados_regiao$uf_sigla)) + 
  theme(legend.position = "none")
 


# Oferta x demanda --------------------------------------------------------

plotar_comparacao_oferta_necessidade <- function(resultado_mc, cod_regsaude_alvo) {

    dados_regiao <- analisar_sensibilidade_regiao(resultado_mc, 
                                                cod_regsaude_alvo = cod_regsaude_alvo)
  
  dados_regiao <- dados_regiao$dados
  
  dados_comparacao <- data.frame(
    categoria = c("Oferta", "Necessidade"),
    valor = c(median(dados_regiao$oferta_media), 
              median(dados_regiao$necessidade_media))
  )
  
  regiao_saude <- unique(dados_regiao$regiao_saude)
  uf_sigla <- unique(dados_regiao$uf_sigla)
  
  grafico <- ggplot(dados_comparacao, aes(x = categoria, y = valor, fill = categoria)) +
    geom_col(width = 0.7) +
    geom_label(aes(label = round(valor, 1)), 
               position = position_dodge(width = 0.5),
               fill = "white",
               vjust = 0.40, 
               size = 4) +
    scale_fill_manual(values = c("steelblue", "coral")) +
    labs(title = paste0("Comparação entre oferta e necessidade - ", 
                        regiao_saude, " - ", 
                        uf_sigla),
         x = "",
         y = "Total de profissionais") +
    theme_minimal() +
    theme(legend.position = "none") + 
    coord_flip()
  
  return(grafico)
}


# 5. Função analise de sensibilidade regional --------------------------------

plotar_distribuicao_regiao <- function(resultado_mc, cod_regsaude_alvo) {
  # Obter os dados da região de saúde
  dados_regiao <- analisar_sensibilidade_regiao(resultado_mc, 
                                                cod_regsaude_alvo = cod_regsaude_alvo)
  
  # Extrair os dados
  dados_regiao <- dados_regiao$dados
  
  # Definir a região geográfica
  dados_regiao <- dados_regiao |> 
    mutate(regiao = case_when(
      uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
      uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
      uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
      uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      TRUE ~ "Não classificado"  # Para lidar com valores não especificados
    ))
  
  # Obter região e UF (assumindo que são os mesmos para todos os dados)
  regiao_saude <- unique(dados_regiao$regiao_saude)
  uf <- unique(dados_regiao$uf_sigla)
  regiao_geografica <- unique(dados_regiao$regiao)
  
  # Definir cores por região
  cores_regioes <- c(
    "Norte" = "#1F78B4",      # Azul
    "Nordeste" = "#33A02C",   # Verde
    "Centro-Oeste" = "#E31A1C", # Vermelho
    "Sudeste" = "#FF7F00",    # Laranja
    "Sul" = "#6A3D9A"         # Roxo
  )
  
  # Criar o gráfico
  grafico <- dados_regiao |> 
    ggplot(aes(x = perc)) + 
    geom_histogram(aes(fill = regiao), color = "black") + 
    geom_vline(xintercept = median(dados_regiao$perc), 
               color = "red", linetype = "dashed") +
    xlim(0,150) + 
    scale_fill_manual(values = cores_regioes) +
    xlab("Resultado relativo (%)") + 
    ylab("Frequência") +
    theme_minimal() + 
    ggtitle(paste0("Distribuição de resultados da região de saúde - ", 
                   regiao_saude, " - ", uf)) +
    theme(legend.position = "none")
  
  # Retornar o gráfico
  return(grafico)
}

plotar_distribuicao_regiao(resultado_mc = resultado_mc,
                           cod_regsaude = "13002")

estado <- unique(resultado_resumo_por_regiao$cod_regsaude)

for (cod_regsaude in estado) {
  
  grafico <- plotar_distribuicao_regiao(resultado_mc, 
                                        cod_regsaude)

  ggsave(plot = grafico,
         filename = paste0("~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/histograma/grafico_regiao_", cod_regsaude, ".jpeg"), 
         width = 8, 
         height = 6)
  
  grafico2 <- plotar_comparacao_oferta_necessidade(resultado_mc,
                                                   cod_regsaude)
  
  ggsave(plot = grafico2,
         filename = paste0("~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/grafico_colunas/oferta_demanda_", cod_regsaude, ".jpeg"), 
         width = 8, 
         height = 6)
}


# # criando o modelo --------------------------------------------------------
 
dados_modelo <- resultado_mc[["resultados_por_regiao"]]
 
dados_modelo <- dados_modelo |> 
                     mutate(indireta = 100 * indireta) |> 
                     mutate(alto_risco = 100 * alto_risco) |> 
                     mutate(regiao = case_when(uf_sigla %in% c("MG", "SP", "RJ", 
                                                               "SP") ~ "Sudeste", 
                                               uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
                                               uf_sigla %in% c("AC", "AM", "AP", 
                                                               "PA", "RO", "RR", "TO") ~ "Norte",
                                               uf_sigla %in% c("AL", "BA", "CE", 
                                                               "MA", "PB", "PE", 
                                                               "PI", "RN", "SE") ~ "Nordeste",
                                               uf_sigla %in% c("DF", "GO", 
                                                               "MT", "MS") ~ "Centro-Oeste",))
 
 
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
              output = "html")     


# Exportando resultados ---------------------------------------------------
# 
# resultado_resumo_por_regiao <- resultado_mc[["resumo_por_regiao"]]
# 
# write.csv(resultado_resumo_por_regiao,
#            "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resumo_regiao.csv")
# 
# parametros_usados <- resultado_mc[["parametros"]]
# 
# write.csv(parametros_usados,
#           "~/GitHub/materno_infantil/02_script/07_output_montecarlo/parametros_usados.csv")
# 
# resultados_regioes <- resultado_mc[["resultados_por_regiao"]]
# 
# write.csv(resultados_regioes,
#           "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_regioes.csv")
# 
# arrow::write_parquet(resultados_regioes,
#                      "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_regioes.parquet")



 