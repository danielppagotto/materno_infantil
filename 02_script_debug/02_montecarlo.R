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
  read_csv("02_script_debug/01_dfs/servicos23_tratado.csv") |> 
  select(-`...1`)

oferta_med <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_23.csv") |> 
  select(-`...1`) |> 
  janitor::clean_names() |> 
  mutate(mes = as.numeric(mes)) |> 
  select(-uf_sigla) |> 
  filter(categoria_profissional == "Médicos")

oferta_enf <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_23.csv") |> 
  select(-`...1`) |> 
  janitor::clean_names() |> 
  mutate(mes = as.numeric(mes)) |> 
  select(-uf_sigla) |> 
  filter(categoria_profissional == "Enfermeiros")

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc)

# Função para executar a simulação de Monte Carlo com a nova função oferta_vs_demanda


executar_simulacao_desagregado <- function(acoes_hab, 
                                           prenatal_hab, 
                                           indireta_enf,
                                           indireta_med, 
                                           todos, 
                                           absenteismo, 
                                           prenatal_alto, 
                                           alto_risco, 
                                           acoes_alto, 
                                           ferias, 
                                           feriados, 
                                           imunizacao, 
                                           coleta_exames,
                                           visita, 
                                           consulta_puerperal, 
                                           consulta_cd, 
                                           enf_coleta_exames, 
                                           enf_coleta_cito, 
                                           enf_prenatal,
                                           enf_imunizacao, 
                                           enf_puerperal, 
                                           enf_visita, 
                                           enf_cd, 
                                           enf_acoes,
                                           servicos_data,  
                                           oferta_data_med, 
                                           oferta_data_enf, 
                                           foco_clinico_data) {
  
  # Usamos os dataframes passados como argumentos para evitar problemas com variáveis globais
  servicos <- servicos_data
  oferta_med <- oferta_data_med
  oferta_enf <- oferta_data_enf
  foco_clinico <- foco_clinico_data
  
  servicos23_tratado <- servicos |> 
    mutate(ano_proc_rea = year(mes_proc_rea)) |> 
    filter(ano_proc_rea == 2023) |> 
    filter(nivel_atencao == "APS")
  
  sus <- 1
  
  if(sus == todos){
    
    servicos23_procedimentos <- 
      
      servicos23_tratado |> 
      
      mutate(qtd_proc_rh = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (1-alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (1-alto_risco),
                 procedimento == "Visita domiciliar" ~ 
                   qtd_proc * (1-alto_risco),
                 TRUE ~ qtd_proc
               )) |> 
      mutate(qtd_proc_ar = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (alto_risco),
                 procedimento == "Visita domiciliar" ~ 
                   2 * qtd_proc * alto_risco, 
                 TRUE ~ 0
               ) 
      )
  } else {
    
    servicos23_procedimentos <- 
      
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
                 procedimento == "Visita domiciliar" ~
                   qtd_proc * (1-alto_risco),
                 TRUE ~ qtd_proc
               )) |> 
      mutate(qtd_proc_ar = 
               case_when(
                 procedimento == "Consulta pré-natal" ~ 
                   qtd_proc * (alto_risco),
                 procedimento == "Ações Educacionais" ~
                   qtd_proc * (alto_risco),
                 procedimento == "Visita domiciliar" ~ 
                   2 * qtd_proc * alto_risco,
                 TRUE ~ 0
               )) 
  }
  
  servicos23_procedimentos <- 
    servicos23_procedimentos |> 
    select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaud, 
           regiao_saude, qtd_nascidos, cobertura_ans, codigo_sigtap,
           procedimento, tipo_procedimento, mes_programado,
           publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
           qtd_proc_ar)
  
  # Traduzir número de horas em número de profissionais necessários
  
  ttd <- 160 - ferias - feriados - absenteismo
  
  necessidade <-
    servicos23_procedimentos |> 
    mutate(total_horas_rh = 
             case_when(codigo_sigtap == "0301010110" ~ 
                         qtd_proc_rh * prenatal_hab/60, 
                       codigo_sigtap == "0301010129" ~ 
                         qtd_proc_rh * consulta_puerperal/60,
                       codigo_sigtap == "1234" ~ 
                         qtd_proc_rh * imunizacao/60, 
                       codigo_sigtap == "0101010010" ~ 
                         qtd_proc_rh * acoes_hab/60,
                       codigo_sigtap == "0301010137" ~
                         qtd_proc_rh * visita/60,
                       codigo_sigtap == "0202" ~ 
                         qtd_proc_rh * coleta_exames/60,
                       codigo_sigtap == "0203" ~ 
                         qtd_proc_rh * coleta_exames/60,
                       codigo_sigtap == "0301010080" ~ 
                         qtd_proc_rh * consulta_cd/60)) |> 
    mutate(total_horas_ar = 
             case_when(codigo_sigtap == "0301010110" ~ 
                         qtd_proc_ar * prenatal_alto/60, 
                       codigo_sigtap == "0301010129" ~ 
                         qtd_proc_ar * consulta_puerperal/60,
                       codigo_sigtap == "1234" ~ 
                         qtd_proc_ar * imunizacao/60, 
                       codigo_sigtap == "0101010010" ~ 
                         qtd_proc_ar * acoes_alto/60,
                       codigo_sigtap == "0301010137" ~
                         qtd_proc_ar * visita/60,
                       codigo_sigtap == "0202" ~ 
                         qtd_proc_ar * coleta_exames/60,
                       codigo_sigtap == "0203" ~ 
                         qtd_proc_ar * coleta_exames/60,
                       codigo_sigtap == "0301010080" ~ 
                         qtd_proc_ar * consulta_cd/60)) |> 
    mutate(nec_prof = (total_horas_rh + total_horas_ar)) |> 
    mutate(necessidade_media_enf = case_when(codigo_sigtap == "0301010110" ~
                                 nec_prof * enf_prenatal,
                               codigo_sigtap == "0301010129" ~ 
                                 nec_prof * enf_puerperal, 
                               codigo_sigtap == "1234" ~ 
                                 nec_prof * enf_imunizacao, 
                               codigo_sigtap == "0101010010" ~ 
                                 nec_prof * enf_acoes,
                               codigo_sigtap == "0301010137" ~
                                 nec_prof * enf_visita, 
                               codigo_sigtap == "0202" ~ 
                                 nec_prof * enf_coleta_exames,
                               codigo_sigtap == "0203" ~
                                 nec_prof * enf_coleta_cito,
                               codigo_sigtap == "0301010080" ~
                                 nec_prof * enf_cd)) |> 
    mutate(nec_med = nec_prof - necessidade_media_enf)
  
  nec_prof <- 
    necessidade |>
    group_by(ano_proc_rea,
             mes_proc_rea, 
             cod_regsaud,
             regiao_saude,
             uf_sigla) |> 
    summarise(nec_med = sum(nec_med),
              necessidade_media_enf = sum(necessidade_media_enf)) |> 
    mutate(nec_med_prof = nec_med/ttd,
           necessidade_media_enf_prof = necessidade_media_enf/ttd) |> 
    rename(ano = ano_proc_rea) |> 
    mutate(mes = month(mes_proc_rea))|> 
    mutate(cod_regsaud = as.numeric(cod_regsaud)) 
  
  
  # Trechinho para tratar a questão da priorização
  
  necessidade_media_enf <- nec_prof |> 
    ungroup() |> 
    select(ano, 
           mes,
           uf_sigla,
           cod_regsaud,
           necessidade_media_enf_prof) 
  
  
  oferta_enfermeiros <- 
    oferta_enf |> 
    left_join(foco_clinico, 
              by = c("cod_regsaud"="cod_regsaud")) |> 
    mutate(oferta_enf = fte40 * (1 - indireta_enf) * perc_fc)
  
  nec_vs_oferta_enfermagem <- 
    necessidade_media_enf |> 
    left_join(oferta_enfermeiros, 
              by = c("cod_regsaud"=
                       "cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    mutate(ra_enf = oferta_enf - necessidade_media_enf_prof) |> 
    mutate(rr_enf = oferta_enf/necessidade_media_enf_prof) |> 
    mutate(equilibrio_enf = case_when(rr_enf > 1 ~ "NC",
                                      rr_enf >= 0.50 & 
                                        rr_enf <= 1 ~ "C50",
                                      rr_enf < 0.50 ~ "C")) |> 
    ungroup()
  
  nec_vs_oferta_enfermagem <- 
    nec_vs_oferta_enfermagem |> 
    select(cod_regsaud, uf_sigla, mes, 
           ano, necessidade_media_enf_prof, 
           oferta_enf, ra_enf, rr_enf, equilibrio_enf)
  
  
  oferta_vs_demanda_enf_anual <- 
    nec_vs_oferta_enfermagem |> 
    group_by(uf_sigla, cod_regsaud) |> 
    summarise(necessidade_media_enf = mean(necessidade_media_enf_prof),
              oferta_media_enf = mean(oferta_enf)) |> 
    mutate(rr_enf = 100 * (oferta_media_enf/necessidade_media_enf)) |> 
    mutate(ra_enf = (oferta_media_enf - necessidade_media_enf)) 
  
  # voltando para a oferta 
  # vamos adicionar a logica de que - caso de faltar profissional de 
  # enfermagem, a carga de trabalho deverá ser jogada para o médico
  # fizemos desta forma, pois a enfermagem tem maiores % nos procedimentos
  # em média
  # Juntar necessidades com oferta
  
  oferta_vs_demanda_med <- 
    nec_prof |> 
    left_join(oferta_med, 
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano")) |> 
    left_join(foco_clinico, 
              by = c("cod_regsaud"="cod_regsaud")) |> 
    mutate(oferta_med = fte40 * (1 - indireta_med) * perc_fc) |> 
    left_join(nec_vs_oferta_enfermagem,
              by = c("cod_regsaud"="cod_regsaud",
                     "mes"="mes",
                     "ano"="ano",
                     "uf_sigla"="uf_sigla")) |>
    mutate(nec_med_realoc = 
             case_when(equilibrio_enf == "NC" ~ nec_med_prof,
                       equilibrio_enf == "C50" |
                         equilibrio_enf == "C" ~ nec_med_prof + (-1 * ra_enf))
    ) |> 
    mutate(ra_med = oferta_med - nec_med_realoc,
           rr_med = 100 * (oferta_med/nec_med_realoc)) 
  
  oferta_vs_demanda_med_anual <- 
    oferta_vs_demanda_med |> 
    group_by(uf_sigla, cod_regsaud, regiao_saude) |> 
    summarise(necessidade_media_med = mean(nec_med_realoc),
              oferta_media_med = mean(oferta_med)) |> 
    mutate(rr_med = 100 * (oferta_media_med/necessidade_media_med)) |> 
    mutate(ra_med = (oferta_media_med - necessidade_media_med)) 
  
  med_enf_oferta_demanda <- 
    oferta_vs_demanda_med_anual |> 
    left_join(oferta_vs_demanda_enf_anual,
              by = c("cod_regsaud","uf_sigla")) |> 
    mutate(classificacao = case_when(
      rr_med >= 100 & 
        rr_enf >= 100 ~ "Ambos superávit",
      rr_med >= 100 &
        rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
      rr_enf >= 100 &
        rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
      rr_med <= 100 & 
        rr_enf <= 100 ~ "Ambos déficit")) |> 
    mutate(Região = case_when(
      uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
      uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
      uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
      uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      TRUE ~ "Não classificado"
    ))
  
  
  
  # Retorna os resultados em uma lista
  resultado <- list(
    por_regiao = med_enf_oferta_demanda,
    necessidade_media_enf = mean(med_enf_oferta_demanda$necessidade_media_enf, 
                                 na.rm = TRUE),
    oferta_media_enf = mean(med_enf_oferta_demanda$oferta_media_enf, 
                            na.rm = TRUE),
    necessidade_media_med = mean(med_enf_oferta_demanda$necessidade_media_med, 
                                 na.rm = TRUE),
    oferta_media_med = mean(med_enf_oferta_demanda$oferta_media_med, 
                            na.rm = TRUE),
    rr_enf_medio = mean(med_enf_oferta_demanda$rr_enf, 
                        na.rm = TRUE),
    ra_enf_medio = mean(med_enf_oferta_demanda$ra_enf,
                        na.rm = TRUE),
    rr_med_medio = mean(med_enf_oferta_demanda$rr_med, 
                        na.rm = TRUE),
    ra_med_medio = mean(med_enf_oferta_demanda$ra_med,
                        na.rm = TRUE)
  )
  
  return(resultado)
}


# Função principal para executar a simulação de Monte Carlo

executar_monte_carlo_desagregado <- function(n_sim = 10000, 
                                             acoes_hab_min = 25,
                                             acoes_hab_max = 35,
                                             prenatal_hab_min = 20,
                                             prenatal_hab_max = 40,
                                             indireta_enf_min = 0.40,
                                             indireta_enf_max = 0.60,
                                             indireta_med_min = 0.50,
                                             indireta_med_max = 0.65,
                                             prob_todos_0 = 0.5, 
                                             prob_todos_1 = 0.5, 
                                             absenteismo_min = 8,
                                             absenteismo_max = 24,
                                             prenatal_alto_min = 25,
                                             prenatal_alto_max = 40,
                                             alto_risco_min = 0.10,
                                             alto_risco_max = 0.20,
                                             acoes_alto_min = 30,
                                             acoes_alto_max = 40,
                                             ferias_min = 4,
                                             ferias_max = 8,
                                             feriados_min = 4,
                                             feriados_max = 8,
                                             imunizacao_min = 10, 
                                             imunizacao_max = 20,
                                             coleta_exames_min = 10,
                                             coleta_exames_max = 20,
                                             visita_min = 45,
                                             visita_max = 60,
                                             consulta_puerperal_min = 20, 
                                             consulta_puerperal_max = 30,
                                             consulta_cd_min = 20,
                                             consulta_cd_max = 30, 
                                             enf_coleta_exames_min = 0.70,
                                             enf_coleta_exames_max = 0.80,
                                             enf_coleta_cito_min = 0.20,
                                             enf_coleta_cito_max = 0.40,
                                             enf_prenatal_min = 0.45,
                                             enf_prenatal_max = 0.55,
                                             enf_imunizacao_min = 0.70,
                                             enf_imunizacao_max = 0.80,
                                             enf_puerperal_min = 0.45,
                                             enf_puerperal_max = 0.55,
                                             enf_visita_min = 0.45,
                                             enf_visita_max = 0.55,
                                             enf_cd_min = 0.45,
                                             enf_cd_max = 0.55,
                                             enf_acoes_min = 0.45,
                                             enf_acoes_max = 0.55,
                                             servicos, 
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
                               min = acoes_hab_min, 
                               max = acoes_hab_max),
    consultas = runif(n_sim, 
                      min = prenatal_hab_min, 
                      max = prenatal_hab_max),
    absenteismo = runif(n_sim, 
                        min = absenteismo_min, 
                        max = absenteismo_max),
    alto_risco = runif(n_sim, 
                       min = alto_risco_min, 
                       max = alto_risco_max),
    acoes_altorisco = runif(n_sim, 
                            min = acoes_alto_min, 
                            max = acoes_alto_max),
    consultas_altorisco = runif(n_sim, 
                                min = prenatal_alto_min,
                                max = prenatal_alto_max),
    indireta_enf = runif(n_sim, 
                     min = indireta_enf_min, 
                     max = indireta_enf_max),
    indireta_med = runif(n_sim,
                         min = indireta_med_min,
                         max = indireta_med_max),
    ferias = runif(n_sim,
                   min = ferias_min,
                   max = ferias_max),
    feriados = runif(n_sim,
                     min = feriados_min,
                     max = feriados_max), 
    coleta_ex = runif(n_sim,
                      min = coleta_exames_min,
                      max = coleta_exames_max),
    visita = runif(n_sim,
                   min = visita_min,
                   max = visita_max),
    
    consulta_puerp = runif(n_sim,
                           min = consulta_puerperal_min,
                           max = consulta_puerperal_max), 
    consulta_cd = runif(n_sim,
                        min = consulta_cd_min,
                        max = consulta_cd_max), 
    enf_acoes = runif(n_sim,
                      min = enf_acoes_min,
                      max = enf_acoes_max),
    enf_coleta = runif(n_sim,
                       min = enf_coleta_exames_min,
                       max = enf_coleta_exames_max),
    enf_coleta_cito = runif(n_sim, 
                            min = enf_coleta_cito_min,
                            max = enf_coleta_cito_max), 
    enf_prenatal = runif(n_sim,
                         min = enf_prenatal_min,
                         max = enf_prenatal_max),
    enf_imunizacao = runif(n_sim,
                           min = enf_imunizacao_min,
                           max = enf_imunizacao_max),
    enf_puerperal = runif(n_sim,
                          min = enf_puerperal_min,
                          max = enf_puerperal_max),
    enf_visita = runif(n_sim,
                       min = enf_visita_min,
                       max = enf_visita_max),
    enf_cd = runif(n_sim,
                   min = enf_cd_min,
                   max = enf_cd_max),
    todos = sample(c(0, 1), size = n_sim, 
                   replace = TRUE, 
                   prob = c(prob_todos_0, prob_todos_1))
  )
  

  
  
  
  # Iniciar medição de tempo
  tic("Execução da simulação Monte Carlo desagregada em paralelo")
  
  # Executar simulação em paralelo
  resultados <- future_map(1:n_sim, function(i) {
    executar_simulacao_desagregado(
      # Parâmetros básicos
      acoes_hab = parametros_simulacao$acoes_educacionais[i],
      prenatal_hab = parametros_simulacao$consultas[i],
      indireta_enf = parametros_simulacao$indireta_enf[i],
      indireta_med = parametros_simulacao$indireta_med[i],
      todos = parametros_simulacao$todos[i],
      absenteismo = parametros_simulacao$absenteismo[i],
      alto_risco = parametros_simulacao$alto_risco[i],
      
      # Parâmetros para alto risco
      prenatal_alto = parametros_simulacao$consultas_altorisco[i],
      acoes_alto = parametros_simulacao$acoes_altorisco[i],
      
      # Parâmetros de ausências
      ferias = parametros_simulacao$ferias[i],
      feriados = parametros_simulacao$feriados[i],
      
      # Parâmetros para tempos de procedimentos médicos
      imunizacao = parametros_simulacao$enf_imunizacao[i], # Usando parâmetro próximo
      coleta_exames = parametros_simulacao$coleta_ex[i],
      visita = parametros_simulacao$visita[i],
      consulta_puerperal = parametros_simulacao$consulta_puerp[i],
      consulta_cd = parametros_simulacao$consulta_cd[i],
      
      # Parâmetros de participação de enfermeiros
      enf_coleta_exames = parametros_simulacao$enf_coleta[i],
      enf_coleta_cito = parametros_simulacao$enf_coleta_cito[i],
      enf_prenatal = parametros_simulacao$enf_prenatal[i],
      enf_imunizacao = parametros_simulacao$enf_imunizacao[i],
      enf_puerperal = parametros_simulacao$enf_puerperal[i],
      enf_visita = parametros_simulacao$enf_visita[i],
      enf_cd = parametros_simulacao$enf_cd[i],
      enf_acoes = parametros_simulacao$enf_acoes[i],
      servicos_data = servicos,
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
    ra_enf_medio = map_dbl(resultados, ~.x$ra_enf_medio),
    rr_med_medio = map_dbl(resultados, ~.x$rr_med_medio),
    ra_med_medio = map_dbl(resultados, ~.x$ra_med_medio)
  )
  
  # Lista de resultados por região para cada simulação
  resultados_por_regiao <- map(1:n_sim, function(i) {
    reg_result <- resultados[[i]]$por_regiao
    # Adicionar o número da simulação e os parâmetros utilizados
    reg_result |>
      mutate(
        simulacao = i,
        # Parâmetros básicos
        acoes_hab = parametros_simulacao$acoes_educacionais[i],
        prenatal_hab = parametros_simulacao$consultas[i],
        absenteismo = parametros_simulacao$absenteismo[i],
        alto_risco = parametros_simulacao$alto_risco[i],
        
        # Parâmetros para indireta
        indireta_enf = parametros_simulacao$indireta_enf[i],
        indireta_med = parametros_simulacao$indireta_med[i],
        
        # Parâmetros para alto risco
        acoes_alto = parametros_simulacao$acoes_altorisco[i],
        prenatal_alto = parametros_simulacao$consultas_altorisco[i],
        
        # Parâmetros de ausências adicionais
        ferias = parametros_simulacao$ferias[i],
        feriados = parametros_simulacao$feriados[i],
        
        # Parâmetros para tempos de procedimentos
        coleta_exames = parametros_simulacao$coleta_ex[i],
        visita = parametros_simulacao$visita[i],
        consulta_puerperal = parametros_simulacao$consulta_puerp[i],
        consulta_cd = parametros_simulacao$consulta_cd[i],
        
        # Parâmetros de participação de enfermeiros
        enf_coleta_exames = parametros_simulacao$enf_coleta[i],
        enf_coleta_cito = parametros_simulacao$enf_coleta_cito[i],
        enf_prenatal = parametros_simulacao$enf_prenatal[i],
        enf_imunizacao = parametros_simulacao$enf_imunizacao[i],
        enf_puerperal = parametros_simulacao$enf_puerperal[i],
        enf_visita = parametros_simulacao$enf_visita[i],
        enf_cd = parametros_simulacao$enf_cd[i],
        enf_acoes = parametros_simulacao$enf_acoes[i],
        
        # Parâmetro todos
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
    group_by(uf_sigla, cod_regsaud, regiao_saude) |>
    summarise(
      media_necessidade_enf = mean(necessidade_media_enf, na.rm = TRUE),
      media_oferta_enf = mean(oferta_media_enf, na.rm = TRUE),
      media_rr_enf = mean(rr_enf, na.rm = TRUE),
      media_necessidade_med = mean(necessidade_media_med, na.rm = TRUE),
      media_oferta_med = mean(oferta_media_med, na.rm = TRUE),
      media_rr_med = mean(rr_med, na.rm = TRUE),
      
      mediana_necessidade_enf = median(necessidade_media_enf, na.rm = TRUE),
      mediana_oferta_enf = median(oferta_media_enf, na.rm = TRUE),
      mediana_rr_enf = median(rr_enf, na.rm = TRUE),
      mediana_necessidade_med = median(necessidade_media_med, na.rm = TRUE),
      mediana_oferta_med = median(oferta_media_med, na.rm = TRUE),
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
    group_by(uf_sigla, cod_regsaud, regiao_saude, todos) |>
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
  acoes_hab_min = 20, 
  acoes_hab_max = 40,
  prenatal_hab_min = 20, 
  prenatal_hab_max = 40,
  absenteismo_min = 8, 
  absenteismo_max = 24,
  indireta_enf_min = 0.50, 
  indireta_enf_max = 0.60,
  indireta_med_min = 0.50,
  indireta_med_max = 0.65,
  prob_todos_0 = 0.5, 
  prob_todos_1 = 0.5,
  alto_risco_min = 0.10,
  alto_risco_max = 0.20,
  prenatal_alto_min = 30,
  prenatal_alto_max = 45,
  acoes_alto_min = 25,
  acoes_alto_max = 45,
  ferias_min = 4,
  ferias_max = 8,
  feriados_min = 4,
  feriados_max = 8,
  imunizacao_min = 10,
  imunizacao_max = 20,
  coleta_exames_min = 10,
  coleta_exames_max = 20,
  visita_min = 45,
  visita_max = 60,
  consulta_puerperal_min = 20,
  consulta_puerperal_max = 30,
  consulta_cd_min = 20,
  consulta_cd_max = 30,
  enf_coleta_exames_min = 0.70,
  enf_coleta_exames_max = 0.80,
  enf_coleta_cito_min = 0.20,
  enf_coleta_cito_max = 0.40,
  enf_prenatal_min = 0.45,
  enf_prenatal_max = 0.55,
  enf_imunizacao_min = 0.70,
  enf_imunizacao_max = 0.80,
  enf_puerperal_min = 0.45,
  enf_puerperal_max = 0.55,
  enf_visita_min = 0.45,
  enf_visita_max = 0.55,
  enf_cd_min = 0.45,
  enf_cd_max = 0.55,
  enf_acoes_min = 0.45,
  enf_acoes_max = 0.55,
  servicos = servicos,
  oferta_med = oferta_med,
  oferta_enf = oferta_enf,
  foco_clinico = foco_clinico
)




# resultados_regioes <- resultado_mc[["resultados_por_regiao"]]|>   
#   mutate(alto_risco = alto_risco * 100,
#          indireta = indireta * 100, 
#          rr_med = rr_med * 100, 
#          participacao_medico = participacao_medico * 100) |> 
#   mutate(regiao = case_when(
#     uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
#     uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
#     uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
#     uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
#     uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
#     TRUE ~ "Não classificado"
#   )) 
# 
