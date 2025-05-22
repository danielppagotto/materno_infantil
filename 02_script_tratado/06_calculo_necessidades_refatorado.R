
library(tidyverse)


# Função para 2025 a 2030 -------------------------------------------------

servicos_tratado <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script_tratado/01_dfs/servicos2430_tratado.csv") |> 
  select(-`...1`)

cobertura_projetada <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/10_cenarios_ans/cenarios_ans_projetado.csv") |> 
  select(-`...1`) |> 
  mutate(cobertura = cobertura/100) |> 
  mutate(cod_regsaud = as.character(cod_regsaud)) |> 
  mutate(cenario = str_replace(cenario, "cenario ", "")) |> 
  filter(ano > 2023) |> 
  pivot_wider(
    id_cols = c(uf_sigla, cod_regsaud, regiao_saude, ano),
    names_from = cenario,
    values_from = cobertura
  ) |> 
  select(-uf_sigla, -regiao_saude)

foco_clinico_projetado <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv")

oferta_enfermeiros <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_enfermeiros_combinados.csv") |> 
  rename(cenario_reducao10 = cenario_aumento,
         cenario_reducao20 = cenario_reducao)

oferta_medicos <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_medicos_combinados.csv") |> 
  mutate(ano = year(ds),
         mes = month(ds)) |> 
  rename(cenario_reducao10 = cenario_aumento,
         cenario_reducao20 = cenario_reducao) |> 
  select(-regiao_saude)


# Calculando a quantidade de procedimentos com SUS dependente --------


calcular_procedimentos <- function(servicos_tratado, 
                                   tipo_ans, 
                                   alto_risco, 
                                   todos) {
  sus <- 1
  
  if(sus == todos) {
    servicos_procedimentos <- 
      servicos_tratado |> 
      mutate(valor_ans = .data[[tipo_ans]]) |>
      mutate(qtd_proc_rh = case_when(
        procedimento == "Consulta pré-natal" ~ qtd_proc * (1-alto_risco),
        procedimento == "Ações Educacionais" ~ qtd_proc * (1-alto_risco),
        procedimento == "Visita domiciliar" ~ qtd_proc * (1-alto_risco),
        TRUE ~ qtd_proc
      )) |> 
      mutate(qtd_proc_ar = case_when(
        procedimento == "Consulta pré-natal" ~ qtd_proc * (alto_risco),
        procedimento == "Ações Educacionais" ~ qtd_proc * (alto_risco),
        procedimento == "Visita domiciliar" ~ 2 * qtd_proc * alto_risco, 
        TRUE ~ 0
      ))
  } else {
    servicos_procedimentos <- 
      servicos_tratado |>
      mutate(valor_ans = .data[[tipo_ans]]) |>
      mutate(qtd_proc = (qtd_nascidos - (qtd_nascidos * valor_ans)) * parametro) |> 
      mutate(qtd_proc_rh = case_when(
        procedimento == "Consulta pré-natal" ~ qtd_proc * (1-alto_risco),
        procedimento == "Ações Educacionais" ~ qtd_proc * (1-alto_risco),
        procedimento == "Visita domiciliar" ~ qtd_proc * (1-alto_risco),
        TRUE ~ qtd_proc
      )) |> 
      mutate(qtd_proc_ar = case_when(
        procedimento == "Consulta pré-natal" ~ qtd_proc * (alto_risco),
        procedimento == "Ações Educacionais" ~ qtd_proc * (alto_risco),
        procedimento == "Visita domiciliar" ~ 2 * qtd_proc * alto_risco,
        TRUE ~ 0
      ))
  }
  
  return(servicos_procedimentos |> 
           select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaud, 
                  regiao_saude, qtd_nascidos, codigo_sigtap,
                  procedimento, tipo_procedimento, mes_programado,
                  publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
                  qtd_proc_ar))
}


# Calcular necessidades em horas profissionais ----------------------------


calcular_necessidades <- function(procedimentos, prenatal_hab, consulta_puerperal, 
                                  imunizacao, acoes_hab, visita, coleta_exames, 
                                  consulta_cd, prenatal_alto, acoes_alto,
                                  enf_prenatal, enf_puerperal, enf_imunizacao, 
                                  enf_acoes, enf_visita, enf_coleta_exames, 
                                  enf_coleta_cito, enf_cd, ist_enf, ist_med) {
  
  necessidade <-
    procedimentos |> 
    mutate(total_horas_rh = case_when(
      codigo_sigtap == "0301010110" ~ qtd_proc_rh * prenatal_hab/60, 
      codigo_sigtap == "0301010129" ~ qtd_proc_rh * consulta_puerperal/60,
      codigo_sigtap == "1234" ~ qtd_proc_rh * imunizacao/60, 
      codigo_sigtap == "0101010010" ~ qtd_proc_rh * acoes_hab/60,
      codigo_sigtap == "0301010137" ~ qtd_proc_rh * visita/60,
      codigo_sigtap == "0202" ~ qtd_proc_rh * coleta_exames/60,
      codigo_sigtap == "0203" ~ qtd_proc_rh * coleta_exames/60,
      codigo_sigtap == "0301010080" ~ qtd_proc_rh * consulta_cd/60
    )) |> 
    mutate(total_horas_ar = case_when(
      codigo_sigtap == "0301010110" ~ qtd_proc_ar * prenatal_alto/60, 
      codigo_sigtap == "0301010129" ~ qtd_proc_ar * consulta_puerperal/60,
      codigo_sigtap == "1234" ~ qtd_proc_ar * imunizacao/60, 
      codigo_sigtap == "0101010010" ~ qtd_proc_ar * acoes_alto/60,
      codigo_sigtap == "0301010137" ~ qtd_proc_ar * visita/60,
      codigo_sigtap == "0202" ~ qtd_proc_ar * coleta_exames/60,
      codigo_sigtap == "0203" ~ qtd_proc_ar * coleta_exames/60,
      codigo_sigtap == "0301010080" ~ qtd_proc_ar * consulta_cd/60
    )) |> 
    mutate(nec_prof = (total_horas_rh + total_horas_ar)) |> 
    mutate(nec_enf = case_when(
      codigo_sigtap == "0301010110" ~ nec_prof * enf_prenatal,
      codigo_sigtap == "0301010129" ~ nec_prof * enf_puerperal, 
      codigo_sigtap == "1234" ~ nec_prof * enf_imunizacao, 
      codigo_sigtap == "0101010010" ~ nec_prof * enf_acoes,
      codigo_sigtap == "0301010137" ~ nec_prof * enf_visita, 
      codigo_sigtap == "0202" ~ nec_prof * enf_coleta_exames,
      codigo_sigtap == "0203" ~ nec_prof * enf_coleta_cito,
      codigo_sigtap == "0301010080" ~ nec_prof * enf_cd
    )) |> 
    mutate(nec_enf = nec_enf * (1 + ist_enf),
           nec_med = nec_prof - nec_enf,
           nec_med = nec_med * (1 + ist_med))
  
  return(necessidade)
}


# Transformando horas de prof em número de prof ---------------------------

agregar_necessidades <- function(necessidades, ferias, feriados, absenteismo) {
  ttd <- 160 - ferias - feriados - absenteismo
  
  nec_prof_agregado <- 
    necessidades |>
    group_by(ano_proc_rea, mes_proc_rea, cod_regsaud, regiao_saude, uf_sigla) |> 
    summarise(nec_med = sum(nec_med), nec_enf = sum(nec_enf)) |> 
    mutate(nec_med_prof = nec_med/ttd, nec_enf_prof = nec_enf/ttd) |> 
    rename(ano = ano_proc_rea) |> 
    mutate(mes = month(mes_proc_rea)) |> 
    mutate(cod_regsaud = as.numeric(cod_regsaud))
  
  return(nec_prof_agregado)
}

# Oferta vs Demanda de enfermagem  ----------------------------------------

calcular_oferta_demanda_enfermagem <- function(nec_prof_agregado, 
                                               projecao_enfermeiros, 
                                               foco_clinico, 
                                               cenario_oferta, 
                                               indireta_enf) {

  nec_enf <- nec_prof_agregado |> 
    ungroup() |> 
    select(ano, mes, uf_sigla, cod_regsaud, nec_enf_prof)
  
  oferta_enfermeiros <- 
    projecao_enfermeiros |>
    mutate(ano = year(ds), mes = month(ds)) |> 
    left_join(foco_clinico, by = c("cod_regsaud", "ano")) |>
    mutate(valor_oferta_enf = .data[[cenario_oferta]]) |>
    mutate(oferta_enf = valor_oferta_enf * (1 - indireta_enf) * perc_fc)
  
  nec_vs_oferta_enfermagem <- 
    nec_enf |> 
    left_join(oferta_enfermeiros, by = c("cod_regsaud", "mes", "ano", "uf_sigla" = "uf")) |> 
    mutate(ra_enf = oferta_enf - nec_enf_prof) |> 
    mutate(rr_enf = oferta_enf/nec_enf_prof) |> 
    mutate(equilibrio_enf = case_when(
      rr_enf > 1 ~ "NC",
      rr_enf >= 0.50 & rr_enf <= 1 ~ "C50",
      rr_enf < 0.50 ~ "C"
    )) |> 
    ungroup() |> 
    filter(ds > "2025-01-01") |>
    select(cod_regsaud, regiao_saude, uf_sigla, mes, ano, 
           nec_enf_prof, oferta_enf, ra_enf, rr_enf, equilibrio_enf)
  
  return(nec_vs_oferta_enfermagem)
}


# Oferta vs demanda médica ------------------------------------------------

calcular_oferta_demanda_medica <- function(nec_prof_agregado, 
                                           oferta_medicos_data, 
                                           foco_clinico, 
                                           nec_vs_oferta_enfermagem,
                                           cenario_oferta, 
                                           indireta_med) {
  
  oferta_vs_demanda_med <- 
    nec_prof_agregado |> 
    mutate(mes_proc_rea = as.Date(mes_proc_rea)) |> 
    filter(mes_proc_rea > "2025-01-01") |> 
    ungroup() |> 
    left_join(oferta_medicos_data, by = c("cod_regsaud", "mes", "ano")) |> 
    left_join(foco_clinico, by = c("cod_regsaud", "ano")) |> 
    mutate(valor_oferta_med = .data[[cenario_oferta]]) |>
    mutate(oferta_med = valor_oferta_med * (1 - indireta_med) * perc_fc) |> 
    left_join(nec_vs_oferta_enfermagem, 
              by = c("cod_regsaud", "mes", "ano", "uf_sigla")) |>
    mutate(nec_med_realoc = case_when(
      equilibrio_enf == "NC" ~ nec_med_prof,
      equilibrio_enf == "C50" | equilibrio_enf == "C" ~ nec_med_prof + (-1 * ra_enf)
    )) |> 
    mutate(ra_med = oferta_med - nec_med_realoc,
           rr_med = 100 * (oferta_med/nec_med_realoc))
  
  return(oferta_vs_demanda_med)
}


# Agregando resultados anualmente  ----------------------------------------


# 6. Função para sintetizar resultados anuais
sintetizar_resultados_anuais <- function(oferta_vs_demanda_med, 
                                         oferta_vs_demanda_enf_anual) {
  
  oferta_vs_demanda_med_anual <- 
    oferta_vs_demanda_med |> 
    group_by(ano, uf_sigla, cod_regsaud) |> 
    summarise(necessidade_media_med = mean(nec_med_realoc),
              oferta_media_med = mean(oferta_med)) |> 
    mutate(rr_med = 100 * (oferta_media_med/necessidade_media_med)) |> 
    mutate(ra_med = (oferta_media_med - necessidade_media_med))
  
  med_enf_oferta_demanda <- 
    oferta_vs_demanda_med_anual |> 
    left_join(oferta_vs_demanda_enf_anual, by = c("cod_regsaud", "uf_sigla", "ano")) |> 
    mutate(classificacao = case_when(
      rr_med >= 100 & rr_enf >= 100 ~ "Ambos superávit",
      rr_med >= 100 & rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
      rr_enf >= 100 & rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
      rr_med <= 100 & rr_enf <= 100 ~ "Ambos déficit"
    )) |> 
    mutate(Região = case_when(
      uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
      uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
      uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
      uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      TRUE ~ "Não classificado"
    ))
  
  return(med_enf_oferta_demanda)
}


# Função que controla tudo  -----------------------------------------------

oferta_vs_demanda <- function(acoes_hab, 
                              prenatal_hab, 
                              indireta_enf,
                              indireta_med, 
                              todos, 
                              absenteismo, 
                              prenatal_alto, 
                              alto_risco, 
                              acoes_alto, 
                              ferias, feriados, 
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
                              ist_enf, 
                              ist_med, 
                              tipo_ans, 
                              cenario_oferta) {
  
  servicos2430_tratado <- 
    vroom::vroom("~/GitHub/materno_infantil/02_script_tratado/01_dfs/servicos2430_tratado.csv") |> 
    select(-`...1`)
  
  projecao_enfermeiros_combinados <- 
    vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_enfermeiros_combinados.csv") |> 
    rename(cenario_reducao10 = cenario_aumento,
           cenario_reducao20 = cenario_reducao)
  
  oferta_medicos <- 
    vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_medicos_combinados.csv") |> 
    mutate(ano = year(ds),
           mes = month(ds)) |> 
    rename(cenario_reducao10 = cenario_aumento,
           cenario_reducao20 = cenario_reducao) |> 
    select(-regiao_saude)
  
  foco_clinico_projetado <- 
    vroom::vroom("~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv")
  
  
  # Fluxo de processamento
  servicos_proc <- calcular_procedimentos(
    servicos2430_tratado, tipo_ans, alto_risco, todos)
  
  necessidades <- calcular_necessidades(
    servicos_proc, prenatal_hab, consulta_puerperal, 
    imunizacao, acoes_hab, visita, coleta_exames, 
    consulta_cd, prenatal_alto, acoes_alto,
    enf_prenatal, enf_puerperal, enf_imunizacao, 
    enf_acoes, enf_visita, enf_coleta_exames, 
    enf_coleta_cito, enf_cd, ist_enf, ist_med)
  
  nec_agregadas <- agregar_necessidades(
    necessidades, ferias, feriados, absenteismo)
  
  oferta_vs_demanda_enf <- calcular_oferta_demanda_enfermagem(
    nec_agregadas, projecao_enfermeiros_combinados, 
    foco_clinico_projetado, cenario_oferta, indireta_enf)
  
  oferta_vs_demanda_enf_anual <- 
    oferta_vs_demanda_enf |> 
    group_by(ano, uf_sigla, cod_regsaud, regiao_saude) |> 
    summarise(necessidade_media = mean(nec_enf_prof),
              oferta_media = mean(oferta_enf)) |> 
    mutate(rr_enf = 100 * (oferta_media/necessidade_media)) |> 
    mutate(ra_enf = (oferta_media - necessidade_media))
  
  oferta_vs_demanda_med <- calcular_oferta_demanda_medica(
    nec_agregadas, oferta_medicos, foco_clinico_projetado, 
    oferta_vs_demanda_enf, cenario_oferta, indireta_med)
  
  resultados <- sintetizar_resultados_anuais(
    oferta_vs_demanda_med, oferta_vs_demanda_enf_anual)
  
  return(resultados)
}


# cenario 1 ---------------------------------------------------------------


c1 <- oferta_vs_demanda(acoes_hab = 45, 
                        prenatal_hab = 30, 
                        indireta_enf = 0.50,
                        indireta_med = 0.50, 
                        todos = 1, 
                        absenteismo = 16, 
                        prenatal_alto = 35, 
                        alto_risco = 15, 
                        acoes_alto = 50, 
                        ferias = 16, 
                        feriados = 4, 
                        imunizacao = 15, 
                        coleta_exames = 15,
                        visita = 60, 
                        consulta_puerperal = 25, 
                        consulta_cd = 25, 
                        enf_coleta_exames = 0.75, 
                        enf_coleta_cito = 0.25, 
                        enf_prenatal = 0.50,
                        enf_imunizacao = 0.75, 
                        enf_puerperal = 0.50, 
                        enf_visita = 0.50, 
                        enf_cd = 0.50, 
                        enf_acoes = 0.50,
                        ist_enf = 0.10,
                        ist_med = 0.10,
                        tipo_ans = "diminuicao", 
                        cenario_oferta = "cenario_reducao20") 


