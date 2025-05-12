# Simulação de Monte Carlo otimizada para a função oferta_vs_demanda
library(tidyverse)
library(future)
library(furrr)
library(tictoc)
library(sandwich)
library(lmtest)
library(modelsummary)
library(patchwork)

options(scipen = 999)

# Carregando dados que vão alimentar a funcao -----------------------------

servicos <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script/04_servicos/servicos23.csv") |> 
  select(-`...1`) 

oferta_medicos <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_23.csv") |> 
  select(-`...1`) |> 
  janitor::clean_names() |> 
  mutate(mes = as.numeric(mes)) |> 
  select(-uf_sigla) |> 
  filter(categoria_profissional == "Médicos")

oferta_enfermeiros_bruto <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_23.csv") |> 
  select(-`...1`) |> 
  janitor::clean_names() |> 
  mutate(mes = as.numeric(mes)) |> 
  select(-uf_sigla) |> 
  filter(categoria_profissional == "Enfermeiros")

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) 

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc)


executar_simulacao_desagregado <- function(acoes_educacionais, 
                                           consultas, 
                                           absenteismo, 
                                           indireta, 
                                           todos,
                                           servicos_data, 
                                           cobertura_data, 
                                           oferta_data_med,
                                           oferta_data_enf,
                                           foco_clinico_data,
                                           alto_risco,
                                           acoes_altorisco,
                                           consultas_altorisco,
                                           participacao_medico) {
  # Usamos os dataframes passados como argumentos para evitar problemas com variáveis globais
  servicos <- servicos_data
  cobertura <- cobertura_data
  oferta_med <- oferta_data_med
  oferta_enf <- oferta_data_enf
  foco_clinico <- foco_clinico_data
  
  servicos2023 <- 
    servicos |> 
    mutate(ano_proc_rea = 
             year(mes_proc_rea)) |> 
    filter(ano_proc_rea == 2023) |> 
    filter(nivel_atencao == "APS" & 
             publico != "Gestantes de Alto Risco") |>
    filter(tipo_procedimento == "Ações Educacionais" |
             tipo_procedimento == "Consultas ou Visitas") |> 
    filter(procedimento != "Avaliação odontológica") |> 
    filter(procedimento != "Visita domiciliar") 
  
  cobertura$cod_regsaud <- 
    as.numeric(cobertura$cod_regsaud)
  
  # Juntar dados de serviços com cobertura
  
  servicos23_tratado <- 
    servicos2023 |> 
    left_join(cobertura, 
              by = c("cod_regsaude"=
                       "cod_regsaud")) |> 
    janitor::clean_names() |> 
    mutate(cobertura = cobertura/100) |> 
    rename(cobertura_ans = cobertura) 
  
  sus = 1
  
  if(sus == todos){
    servicos23_tratado <- servicos23_tratado |> 
      
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
    servicos23_tratado <- 
      servicos23_tratado |>
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
    servicos23_tratado |> 
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
                         qtd_proc_rh * acoes_educacionais/60,
                       tipo_procedimento == 
                         "Consultas ou Visitas" ~ 
                         qtd_proc_rh * consultas/60)) |> 
    mutate(total_horas_ar = 
             case_when(tipo_procedimento == 
                         "Ações Educacionais" ~ 
                         qtd_proc_ar * acoes_altorisco/60,
                       tipo_procedimento == 
                         "Consultas ou Visitas" ~ 
                         qtd_proc_ar * consultas_altorisco/60)) |> 
    mutate(nec_prof = (total_horas_rh + total_horas_ar)/ttd) |>
    mutate(nec_cat = nec_prof * participacao_medico) 
  
  necessidade_tratada <- 
    necessidade |> 
    group_by(uf_sigla, cod_regsaude, regiao_saude,  
             mes_proc_rea) |> 
    summarise(nec_prof = sum(nec_prof),
              nec_cat = sum(nec_cat),
              nec_ch = sum(total_horas_rh + total_horas_ar)) |> 
    mutate(mes = month(mes_proc_rea),
           ano = year(mes_proc_rea), 
           .after = mes_proc_rea)
  
  # Cálculo para enfermeiros 
  oferta_enfermeiros <- 
    oferta_enf |> 
    left_join(foco_clinico, 
              by = c("cod_regsaud"="cod_regsaud")) |> 
    mutate(oferta_enf = fte40 * (1 - indireta) * perc_fc)
  
  nec_vs_oferta_enfermagem <- 
    necessidade_tratada |> 
    left_join(oferta_enfermeiros, 
              by = c("cod_regsaude"=
                       "cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    mutate(ra_enf = oferta_enf - (nec_prof - nec_cat)) |> 
    mutate(rr_enf = oferta_enf/(nec_prof - nec_cat)) |> 
    mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                      rr_enf >= 0.50 & 
                                        rr_enf <= 1 ~ "C50",
                                      rr_enf < 0.50 ~ "C")) |> 
    ungroup() |> 
    select(cod_regsaude, uf_sigla, mes, ano, oferta_enf, rr_enf, equilibrio_enf)
  
  nec_vs_oferta_enfermagem_anual <- 
    necessidade_tratada |> 
    left_join(oferta_enfermeiros, 
              by = c("cod_regsaude"=
                       "cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    group_by(uf_sigla, cod_regsaude, ano) |> 
    summarise(nec_enf = mean(nec_prof - nec_cat),
              oferta_enf = mean(oferta_enf)) |>
    mutate(rr_enf = 100 * oferta_enf/nec_enf)
  
  
  # Juntar necessidades com oferta para médicos
  oferta_vs_demanda_med <- 
    necessidade_tratada |> 
    left_join(oferta_med, 
              by = c("cod_regsaude"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico, 
              by = c("cod_regsaude"="cod_regsaud")) |> 
    mutate(oferta_direta =  fte40 * (1 - indireta),
           oferta_med = oferta_direta * perc_fc) |> 
    left_join(nec_vs_oferta_enfermagem,
              by = c("cod_regsaude"="cod_regsaude",
                     "mes"="mes",
                     "ano"="ano",
                     "uf_sigla"="uf_sigla")) |>
    mutate(nec_medicos = 
             case_when(equilibrio_enf == "NC" ~ nec_cat,
                       equilibrio_enf == "C50" ~ nec_cat + (nec_prof - nec_cat) * (1 - rr_enf),
                       equilibrio_enf == "C" ~ nec_cat + (nec_prof - nec_cat) * (1 - rr_enf))) |> 
    mutate(ra_med = oferta_med - nec_medicos,
           rr_med = oferta_med/nec_medicos) 
  
  oferta_vs_demanda_med_anual <- 
    oferta_vs_demanda_med |> 
    group_by(uf_sigla, cod_regsaude, regiao_saude) |> 
    summarise(nec_med = mean(nec_medicos),
              oferta_med = mean(oferta_med)) |> 
    mutate(rr_med = oferta_med/nec_med) |> 
    mutate(ra_med = (oferta_med - nec_med)) 
  
  med_enf_oferta_demanda <- 
    oferta_vs_demanda_med_anual |> 
    left_join(nec_vs_oferta_enfermagem_anual,
              by = c("cod_regsaude","uf_sigla")) |> 
    mutate(classificacao = case_when(
      rr_med >= 1 & 
        rr_enf >= 1 ~ "Ambos superávit",
      rr_med >= 1 &
        rr_enf < 1 ~ "Superávit em médicos e déficit de enfermeiros",  
      rr_enf >= 1 &
        rr_med < 1 ~ "Superávit em enfermeiros e déficit de médicos",        
      rr_med < 1 & 
        rr_enf < 1 ~ "Ambos déficit"))  
  
  
  # Retorna os resultados em uma lista
  resultado <- list(
    por_regiao = med_enf_oferta_demanda,
    necessidade_media_enf = mean(med_enf_oferta_demanda$nec_enf, 
                                 na.rm = TRUE),
    oferta_media_enf = mean(med_enf_oferta_demanda$oferta_enf, 
                            na.rm = TRUE),
    necessidade_media_med = mean(med_enf_oferta_demanda$nec_med, 
                                 na.rm = TRUE),
    oferta_media_med = mean(med_enf_oferta_demanda$oferta_med, 
                            na.rm = TRUE),
    rr_enf_medio = mean(med_enf_oferta_demanda$rr_enf, 
                        na.rm = TRUE),
    rr_med_medio = mean(med_enf_oferta_demanda$rr_med, 
                        na.rm = TRUE)
  )
  
  return(resultado)
}


# Função principal para executar a simulação de Monte Carlo

executar_monte_carlo_desagregado <- function(n_sim = 10000, 
                                             acoes_min = 20, 
                                             acoes_max = 40,
                                             consultas_min = 20, 
                                             consultas_max = 40,
                                             absenteismo_min = 8, 
                                             absenteismo_max = 24,
                                             indireta_min = 0.50, 
                                             indireta_max = 0.60,
                                             prob_todos_0 = 0.5, 
                                             prob_todos_1 = 0.5,
                                             alto_risco_min = 0.10,
                                             alto_risco_max = 0.20,
                                             consultas_altorisco_min = 30,
                                             consultas_altorisco_max = 45,
                                             acoes_altorisco_min = 25,
                                             acoes_altorisco_max = 45,
                                             participacao_medico_min = 0.4,
                                             participacao_medico_max = 0.6,
                                             servicos, 
                                             cobertura, 
                                             oferta_med, 
                                             oferta_enf,
                                             foco_clinico){

  
  # Informar início
  cat("Iniciando simulação de Monte Carlo com", n_sim, 
      "iterações\n")
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
    participacao_medico = runif(n_sim,
                                min = participacao_medico_min,
                                max = participacao_medico_max),
    consultas_altorisco = runif(n_sim, 
                                min = consultas_altorisco_min,
                                max = consultas_altorisco_max),
    indireta = runif(n_sim, 
                     min = indireta_min, 
                     max = indireta_max),
    todos = sample(c(0, 1), size = n_sim, 
                   replace = TRUE, 
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
      participacao_medico = parametros_simulacao$participacao_medico[i],
      acoes_altorisco = parametros_simulacao$acoes_altorisco[i],
      consultas_altorisco = parametros_simulacao$consultas_altorisco[i],
      servicos_data = servicos,
      cobertura_data = cobertura,
      oferta_data_med = oferta_med,
      oferta_data_enf = oferta_enf,
      foco_clinico_data = foco_clinico
    )
  }, .options = furrr_options(seed = TRUE))
  
  # Finalizar medição de tempo
  toc()
  
  # Extrair resultados agregados
  resultados_df <- tibble(
    simulacao = 1:n_sim,
    necessidade_media_enf = map_dbl(resultados, ~.x$necessidade_media_enf),
    oferta_media_enf = map_dbl(resultados, ~.x$oferta_media_enf),
    necessidade_media_med = map_dbl(resultados, ~.x$necessidade_media_med),
    oferta_media_med = map_dbl(resultados, ~.x$oferta_media_med),
    rr_enf_medio = map_dbl(resultados, ~.x$rr_enf_medio),
    rr_med_medio = map_dbl(resultados, ~.x$rr_med_medio)
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
        consultas_altorisco = parametros_simulacao$consultas_altorisco[i],
        participacao_medico = parametros_simulacao$participacao_medico[i]
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
      media_necessidade_enf = mean(necessidade_media_enf, na.rm = TRUE),
      media_oferta_enf = mean(oferta_media_enf, na.rm = TRUE),
      media_rr_enf = mean(rr_enf_medio, na.rm = TRUE),
      media_necessidade_med = mean(necessidade_media_med, na.rm = TRUE),
      media_oferta_med = mean(oferta_media_med, na.rm = TRUE),
      media_rr_med = mean(rr_med_medio, na.rm = TRUE),
      
      mediana_necessidade_enf = median(necessidade_media_enf, na.rm = TRUE),
      mediana_oferta_enf = median(oferta_media_enf, na.rm = TRUE),
      mediana_rr_enf = median(rr_enf_medio, na.rm = TRUE),
      mediana_necessidade_med = median(necessidade_media_med, na.rm = TRUE),
      mediana_oferta_med = median(oferta_media_med, na.rm = TRUE),
      mediana_rr_med = median(rr_med_medio, na.rm = TRUE)
    )
  
  # Resumo por região de saúde
  resumo_por_regiao <- 
    resultados_regioes_completo |>
    group_by(uf_sigla, cod_regsaude, regiao_saude) |>
    summarise(
      media_necessidade_enf = mean(nec_enf, na.rm = TRUE),
      media_oferta_enf = mean(oferta_enf, na.rm = TRUE),
      media_rr_enf = mean(rr_enf, na.rm = TRUE),
      media_necessidade_med = mean(nec_med, na.rm = TRUE),
      media_oferta_med = mean(oferta_med, na.rm = TRUE),
      media_rr_med = mean(rr_med, na.rm = TRUE),
      
      mediana_necessidade_enf = median(nec_enf, na.rm = TRUE),
      mediana_oferta_enf = median(oferta_enf, na.rm = TRUE),
      mediana_rr_enf = median(rr_enf, na.rm = TRUE),
      mediana_necessidade_med = median(nec_med, na.rm = TRUE),
      mediana_oferta_med = median(oferta_med, na.rm = TRUE),
      mediana_rr_med = median(rr_med, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # Resultados por valor de 'todos'
  resultados_por_todos <- resultados_completos |>
    group_by(todos) |>
    summarise(
      media_rr_enf = mean(rr_enf_medio, na.rm = TRUE),
      media_rr_med = mean(rr_med_medio, na.rm = TRUE),
      mediana_rr_enf = median(rr_enf_medio, na.rm = TRUE),
      mediana_rr_med = median(rr_med_medio, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Resultados por região e valor de 'todos'
  resultados_regiao_todos <- resultados_regioes_completo |>
    group_by(uf_sigla, cod_regsaude, regiao_saude, todos) |>
    summarise(
      media_rr_enf = mean(rr_enf, na.rm = TRUE),
      media_rr_med = mean(rr_med, na.rm = TRUE),
      mediana_rr_enf = median(rr_enf, na.rm = TRUE),
      mediana_rr_med = median(rr_med, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    parametros = parametros_simulacao,
    resultados = resultados_completos,
    resultados_por_regiao = resultados_regioes_completo,
    resumo = resumo_simulacao,
    resumo_por_regiao = resumo_por_regiao,
    resultados_por_todos = resultados_por_todos,
    resultados_regiao_todos = resultados_regiao_todos
  ))
  
}


resultado_mc <- executar_monte_carlo_desagregado(
  n_sim = 10000, 
  acoes_min = 20, 
  acoes_max = 40,
  consultas_min = 20, 
  consultas_max = 40,
  absenteismo_min = 8, 
  absenteismo_max = 24,
  indireta_min = 0.50, 
  indireta_max = 0.60,
  prob_todos_0 = 0.5, 
  prob_todos_1 = 0.5,
  alto_risco_min = 0.10,
  alto_risco_max = 0.20,
  consultas_altorisco_min = 30,
  consultas_altorisco_max = 45,
  acoes_altorisco_min = 25,
  acoes_altorisco_max = 45,
  participacao_medico_min = 0.4,
  participacao_medico_max = 0.6,
  servicos = servicos,
  cobertura = cobertura,
  oferta_med = oferta_medicos,
  oferta_enf = oferta_enfermeiros_bruto,
  foco_clinico = foco_clinico
)


resultados_regioes <- resultado_mc[["resultados_por_regiao"]]|>   
  mutate(alto_risco = alto_risco * 100,
         indireta = indireta * 100, 
         rr_med = rr_med * 100, 
         participacao_medico = participacao_medico * 100) |> 
  mutate(regiao = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  )) 

# write.csv(resultados_regioes,
#            "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_23_08_05.csv")
# 
# resumo_regiao <- resultado_mc[["resumo_por_regiao"]]
# 
# write.csv(resumo_regiao,
#            "~/GitHub/materno_infantil/02_script/07_output_montecarlo/resumo_resultados23_08_05.csv")
# 
# parametros <- resultado_mc[["parametros"]]
# 
# write.csv(parametros, 
#            "~/GitHub/materno_infantil/02_script/07_output_montecarlo/parametros23_08_05.csv")
# 

# Explorando resultados da simulação --------------------------------------

resultados_mc <- read_csv("~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_23_08_05.csv") 
                        

modelo <- lm("rr_med ~ consultas + absenteismo + indireta + todos + alto_risco + 
                       acoes_altorisco + consultas_altorisco + participacao_medico +
                       acoes_educacionais + regiao", resultados_mc)


summary(modelo)


# explorando resultados  --------------------------------------------------

mediana <- 
  resultados_mc |> 
  filter(cod_regsaude == 29022) |> 
  group_by(cod_regsaude) |> 
  summarise(mediana_med = median(rr_med),
            mediana_enf = median(rr_enf), 
            quart1_med = quantile(rr_med, 0.25),
            quart3_med = quantile(rr_med, 0.75),
            quart1_enf = quantile(rr_enf, 0.25),
            quart3_enf = quantile(rr_enf, 0.75))

mediana_med <- mediana$mediana_med
mediana_enf <- mediana$mediana_enf
  
med <- resultados_mc |> 
  filter(cod_regsaude == 29022) |> 
  ggplot(aes(x = rr_med)) + 
  geom_histogram(fill = "darkblue",
                 col = "black") + 
  geom_vline(xintercept = mediana_med,
             linetype = "dashed",
             col = "red") +
  theme_bw() + xlab("Percentual (%)") + 
  ylab("Frequência")  + xlim(0, 200) + 
  ggtitle("Distribuição de Resultado Relativo - Médicos",
          "Região de Saúde: Santo Antônio de Jesus - BA")

enf <- resultados_mc |> 
  filter(cod_regsaude == 29022) |> 
  ggplot(aes(x = rr_enf)) + 
  geom_histogram(fill = "darkgreen",
                 col = "black") + 
  geom_vline(xintercept = mediana_enf,
             linetype = "dashed",
             col = "red") +
  theme_bw() + xlab("Percentual (%)") + 
  ylab("Frequência")  + xlim(0, 200) + 
  ggtitle("Distribuição de Resultado Relativo - Enfermeiros",
          "Região de Saúde: Santo Antônio de Jesus - BA")

a <- med + enf

ggsave(plot = a, 
       filename = "~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/regiao12001.jpeg",
       dpi = 500, 
       width = 10, 
       height = 5)


# Automatizando uma função ------------------------------------------------

regioes <- unique(resultados_mc$cod_regsaude)



distribuicao_resultados <- 
  
  function(reg){
    
    info_regiao <- 
      resultados_mc |> 
      filter(cod_regsaude == reg) |> 
      select(regiao_saude, uf_sigla) |> 
      distinct()
    
    regiao_saude <- info_regiao$regiao_saude
    uf_sigla <- info_regiao$uf_sigla
    
    mediana <- 
      resultados_mc |> 
      filter(cod_regsaude == reg) |> 
      group_by(cod_regsaude) |> 
      summarise(mediana_med = median(rr_med),
                mediana_enf = median(rr_enf))
    
    mediana_med <- mediana$mediana_med
    mediana_enf <- mediana$mediana_enf
    
    med <- 
      resultados_mc |> 
        filter(cod_regsaude == reg) |> 
        ggplot(aes(x = rr_med)) + 
        geom_histogram(fill = "darkblue",
                       col = "black") + 
        geom_vline(xintercept = mediana_med,
                   linetype = "dashed",
                   col = "red") +
        xlim(0, 200) + 
        theme_bw() + xlab("Percentual (%)") + 
        ylab("Frequência")  +
        ggtitle("Distribuição de Resultado Relativo - Médicos",
                paste0("Região de Saúde: ", regiao_saude," - ", uf_sigla))
    
    enf <- 
      resultados_mc |> 
        filter(cod_regsaude == reg) |>  
        ggplot(aes(x = rr_enf)) + 
        geom_histogram(fill = "darkgreen",
                       col = "black") + 
        geom_vline(xintercept = mediana_enf,
                   linetype = "dashed",
                   col = "red") +
        xlim(0, 200) + 
        theme_bw() + xlab("Percentual (%)") + 
        ylab("Frequência")  + 
        ggtitle("Distribuição de Resultado Relativo - Enfermeiros",
              paste0("Região de Saúde: ", regiao_saude," - ", uf_sigla))
    
    b <- med + enf
    
    ggsave(plot = b, 
           filename = paste0("~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/histograma/grafico_regiao_",
                             reg,".jpeg"),
           dpi = 500, 
           width = 12, 
           height = 5)
  }



for(reg in regioes){
  
  distribuicao_resultados(reg = reg)
  
  cat("Processando região:", reg, "\n")
  
  
}

