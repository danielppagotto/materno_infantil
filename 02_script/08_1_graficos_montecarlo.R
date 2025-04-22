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

resultados_regioes <- 
  read_csv("02_script/07_output_montecarlo/resultados_regioes.csv")

resumo_regiao <- 
  read_csv("02_script/07_output_montecarlo/resumo_regiao.csv")


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
    xlim(0,100) + 
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
# 


 