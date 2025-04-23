# App.R - Aplicativo Shiny para visualização de simulações de Monte Carlo
# para planejamento da força de trabalho em saúde materno-infantil

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(sf)
library(forcats)
library(scales)
library(ggspatial)
library(DT)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(vroom)

# UI do aplicativo
ui <- dashboardPage(
  dashboardHeader(title = "Simulação FTS"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", 
               tabName = "dashboard", 
               icon = icon("dashboard")),
      menuItem("Sobre", 
               tabName = "about", 
               icon = icon("info-circle"))
    ),
    
    uiOutput("acoes_educacionais_selector"),
    uiOutput("consultas_selector"),
    uiOutput("ttd_selector"),
    uiOutput("indireta_selector"),
    uiOutput("todos_selector"),
    
    actionButton("simulate", "Executar Simulação", 
                 icon = icon("play"), 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                # Coluna da esquerda para o mapa (ocupando 50% da largura)
                column(width = 6,
                       box(title = "Mapa de Cobertura por Região de saúde", 
                           status = "primary", 
                           solidHeader = TRUE,
                           width = NULL, # Ocupa 100% da largura da coluna
                           height = 800, # Altura suficiente para ocupar toda a lateral
                           plotOutput("mapa", height = "750px"))
                ),
                
                # Coluna da direita para boxplot e tabela (ocupando 50% da largura)
                column(width = 6,
                       # Boxplot na metade superior
                       box(title = "Distribuição do Percentual de Cobertura por UF", 
                           status = "primary", 
                           solidHeader = TRUE,
                           width = NULL, # Ocupa 100% da largura da coluna
                           plotOutput("boxplot", height = "375px")),
                       
                       # Tabela na metade inferior
                       box(title = "Resultados da Simulação", 
                           status = "primary", 
                           solidHeader = TRUE,
                           width = NULL, # Ocupa 100% da largura da coluna
                           DTOutput("tabela", height = "375px"))
                )
              )
      ),
      
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12,
                    h2("Sobre este aplicativo"),
                    p("Este aplicativo apresenta os resultados de 10.000 simulações de Monte Carlo para o planejamento da força de trabalho em saúde materno-infantil."),
                    p("As simulações consideram os seguintes parâmetros com suas respectivas variações:"),
                    tags$ul(
                      tags$li("Tempo de atividades educacionais: 22 a 37,5 minutos"),
                      tags$li("Tempo de consultas: 20 a 40 minutos"),
                      tags$li("Tempo Total Disponível: 113 a 153 horas mensais"),
                      tags$li("Percentual de atividades indiretas: 30% a 50%"),
                      tags$li("População: Todos os profissionais ou apenas SUS dependentes")
                    ),
                    p("O resultado (variável de desfecho) é o percentual de balanceamento entre oferta e demanda de serviços de saúde materno-infantil."),
                    p("Utilize os controles no painel lateral para ajustar os parâmetros e observar como eles afetam os resultados da simulação.")
                )
              )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Função para carregar os dados de simulação uma vez
  dados_simulacao <- reactive({
    # Carregar dados
    df <- vroom::vroom("~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_regioes.csv")
    return(df)
  })
  
  # Carregar shapefile das regiões de saúde
  observe({
    # Carregar os dados geoespaciais na inicialização
    regioes_sf <- geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
                               what = "sp")
    
    # Armazenar para uso nos gráficos
    session$userData$regioes_sf <- regioes_sf
  })
  
  # Controles dinâmicos baseados nos dados
  output$acoes_educacionais_selector <- renderUI({
    df <- dados_simulacao()
    valores_acoes <- sort(unique(df$acoes_educacionais))
    
    sliderInput("acoes_educacionais", 
                "Tempo de Atividades Educacionais (minutos):", 
                min = min(valores_acoes),
                max = max(valores_acoes),
                value = median(valores_acoes),
                step = 0.5)
  })
  
  output$consultas_selector <- renderUI({
    df <- dados_simulacao()
    valores_consultas <- sort(unique(df$consultas))
    
    sliderInput("consultas", 
                "Tempo de Consultas (minutos):", 
                min = min(valores_consultas),
                max = max(valores_consultas),
                value = median(valores_consultas),
                step = 1)
  })
  
  output$ttd_selector <- renderUI({
    df <- dados_simulacao()
    valores_ttd <- sort(unique(df$ttd))
    
    sliderInput("ttd", 
                "Tempo Total Disponível (horas mensais):", 
                min = min(valores_ttd),
                max = max(valores_ttd),
                value = median(valores_ttd),
                step = 1)
  })
  
  output$indireta_selector <- renderUI({
    df <- dados_simulacao()
    valores_indireta <- sort(unique(df$indireta))
    
    sliderInput("indireta", 
                "Percentual de Atividades Indiretas (%):", 
                min = min(valores_indireta),
                max = max(valores_indireta),
                value = median(valores_indireta),
                step = 1)
  })
  
  output$todos_selector <- renderUI({
    df <- dados_simulacao()
    valores_todos <- sort(unique(df$todos))
    
    # Para variável dicotômica, usar radioButtons
    radioButtons("todos", 
                 "População:", 
                 choices = setNames(
                   valores_todos, 
                   ifelse(valores_todos == 1, "Todos os profissionais", "Apenas SUS dependentes")
                 ),
                 selected = 1)
  })
  
  # Função que processa os dados com base nos parâmetros selecionados
  simular_dados <- reactive({
    # Simulação é acionada pelo botão
    input$simulate
    
    # Verificar se todos os inputs estão disponíveis
    req(input$acoes_educacionais, input$consultas, input$ttd, input$indireta, input$todos)
    
    # Obter todos os dados
    resultados_completos <- dados_simulacao()
    
    # Filtrar pelos valores selecionados
    resultados <- resultados_completos %>%
      filter(acoes_educacionais == input$acoes_educacionais,
             consultas == input$consultas,
             ttd == input$ttd,
             indireta == input$indireta,
             todos == input$todos)
    
    # Se não houver resultados exatos, use a simulação mais próxima
    if(nrow(resultados) == 0) {
      # Encontrar os registros mais próximos para cada parâmetro
      resultados <- resultados_completos %>%
        mutate(
          diff_acoes = abs(acoes_educacionais - input$acoes_educacionais),
          diff_consultas = abs(consultas - input$consultas),
          diff_ttd = abs(ttd - input$ttd),
          diff_indireta = abs(indireta - input$indireta),
          diff_total = diff_acoes + diff_consultas + diff_ttd + diff_indireta + (todos != input$todos) * 100
        ) %>%
        arrange(diff_total) %>%
        head(1000)  # Pegar os 1000 mais próximos
    }
    
    # Calcular estatísticas por UF
    stats_by_uf <- resultados %>%
      group_by(uf_sigla, regiao_saude) %>%
      summarise(
        media_percentual = mean(perc),
        mediana_percentual = median(perc),
        min_percentual = min(perc),
        max_percentual = max(perc),
        p25 = quantile(perc, 0.25),
        p75 = quantile(perc, 0.75),
        abaixo_50 = mean(perc < 50) * 100,
        entre_50_100 = mean(perc >= 50 & perc <= 100) * 100,
        acima_100 = mean(perc > 100) * 100,
        .groups = 'drop'
      )
    
    # Criando uma média ajustada para o mapa
    stats_by_uf$media_percentual_ajustado <- 
      pmax(0, pmin(100, stats_by_uf$media_percentual))
    
    return(list(
      resultados = resultados,
      stats_by_uf = stats_by_uf
    ))
  })
  
  # Renderizar mapa
  output$mapa <- renderPlot({
    # Requisitar que os inputs e dados geoespaciais estejam disponíveis
    req(input$acoes_educacionais, 
        input$consultas, 
        input$ttd, 
        input$indireta, 
        input$todos)
    req(session$userData$regioes_sf)
    
    # Obter dados processados
    dados <- simular_dados()
    stats_by_uf <- dados$stats_by_uf
    
    # Juntar dados estatísticos com shapefile
    baseline <- stats_by_uf %>% 
      left_join(session$userData$regioes_sf, 
                by = c("regiao_saude" = "reg_id")) %>% 
      distinct()
    
    # Garantir que o objeto é um sf
    baseline <- st_as_sf(baseline)
    
    baseline <- baseline |> 
                mutate(regiao = 
                         case_when(uf_sigla %in% c("AM",
                                                  "RO",
                                                  "RR",
                                                  "TO",
                                                  "PA",
                                                  "AC",
                                                  "AP") ~ "Norte", 
                                  uf_sigla %in% c("BA",
                                                  "PE",
                                                  "PI",
                                                  "SE",
                                                  "MA",
                                                  "RN",
                                                  "AL",
                                                  "CE",
                                                  "PB") ~ "Nordeste",
                                  uf_sigla %in% c("GO",
                                                  "DF",
                                                  "MS",
                                                  "MT") ~ "Centro Oeste",
                                  uf_sigla %in% c("RS",
                                                  "PR",
                                                  "SC") ~ "Sul", 
                                  uf_sigla %in% c("MG",
                                                  "SP",
                                                  "RJ",
                                                  "ES") ~ "Sudeste"), .after = uf_sigla)


    # Gerar o mapa
    ggplot() +
      geom_sf(data = baseline, 
              aes(fill = media_percentual_ajustado, 
                  geometry = geometry), 
              color = "#f5f5f5") +
      scale_fill_gradientn(colors = c("#D92B3A", 
                                      "#d4e302",
                                      "#02592e"), 
                           values = rescale(c(0, 50, 100)), 
                           limits = c(0, 100),
                           breaks = c(0, 25, 50, 75, 100)) + 
      theme_minimal() +
      labs(fill = "Percentual de Balanceamento",
           title = "Distribuição Geográfica do Percentual de Balanceamento entre Oferta e Demanda") +
      annotation_north_arrow(location = "tr",  
                             which_north = "true",
                             style = north_arrow_fancy_orienteering()) +
      annotation_scale(location = "bl", width_hint = 0.3) +
      theme(
        legend.justification = "center",
        legend.box = "horizontal",
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1), 
        plot.margin = margin(10, 10, 10, 10))
  })
  
  # Renderizar gráfico de boxplot
  output$boxplot <- renderPlot({
    # Requisitar que os inputs estejam disponíveis
    req(input$acoes_educacionais, input$consultas, input$ttd, input$indireta, input$todos)
    
    # Obter dados processados
    dados <- simular_dados()
    resultados <- dados$resultados |> 
      mutate(regiao = case_when(uf_sigla %in% c("AM",
                                                "RO",
                                                "RR",
                                                "TO",
                                                "PA",
                                                "AC",
                                                "AP") ~ "Norte", 
                                uf_sigla %in% c("BA",
                                                "PE",
                                                "PI",
                                                "SE",
                                                "MA",
                                                "RN",
                                                "AL",
                                                "CE",
                                                "PB") ~ "Nordeste",
                                uf_sigla %in% c("GO",
                                                "DF",
                                                "MS",
                                                "MT") ~ "Centro Oeste",
                                uf_sigla %in% c("RS",
                                                "PR",
                                                "SC") ~ "Sul", 
                                uf_sigla %in% c("MG",
                                                "SP",
                                                "RJ",
                                                "ES") ~ "Sudeste"), .after = uf_sigla)
    
    
    # Gerar boxplot
    resultados %>% 
      ggplot(aes(x = perc,
                 y = fct_reorder(uf_sigla, perc, median),
                 fill = regiao)) + 
      geom_boxplot() + 
      theme_minimal() + 
      labs(title = "Distribuição do Percentual de Balanceamento por UF",
           x = "Percentual de Balanceamento", 
           y = "UF", 
           fill = "Região") +
      xlim(0, 200) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom"
      )
  })
  
  # Renderizar tabela de resultados
  output$tabela <- renderDT({
    # Requisitar que os inputs estejam disponíveis
    req(input$acoes_educacionais, input$consultas, input$ttd, input$indireta, input$todos)
    
    # Obter dados processados
    dados <- simular_dados()
    stats_by_uf <- dados$stats_by_uf
    
    # Formatar tabela
    tabela <- stats_by_uf %>%
      select(uf_sigla, regiao_saude, mediana_percentual, 
             abaixo_50, entre_50_100, acima_100) %>%
      rename(
        "UF" = uf_sigla,
        "Região" = regiao_saude,
        "Mediana (%)" = mediana_percentual,
        "Abaixo de 50% (%)" = abaixo_50,
        "Entre 50% e 100% (%)" = entre_50_100,
        "Acima de 100% (%)" = acima_100
      )
    
    # Renderizar datatable
    datatable(tabela, 
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                columnDefs = list(list(
                  className = 'dt-center', 
                  targets = "_all"
                ))
              ),
              rownames = FALSE) %>%
      formatRound(columns = c("Mediana (%)", "Abaixo de 50% (%)", 
                              "Entre 50% e 100% (%)", "Acima de 100% (%)"), 
                  digits = 1)
  })
}

# Execução do App
shinyApp(ui = ui, server = server)