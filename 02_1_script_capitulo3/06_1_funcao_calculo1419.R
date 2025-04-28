
library(RODBC)
library(tidyverse)

# Carregando dados

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) 

cobertura$cod_regsaud <- 
  as.numeric(cobertura$cod_regsaud)

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico.csv") |> 
  select(cod_regsaud, perc_fc)

servicos14_19 <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script/04_servicos/servicos_14_19.csv") |> 
  select(-`...1`) 

oferta_tratado1419 <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/oferta_aps_14_19.csv") |> 
  select(-`...1`) 




query_prof <- 
  'SELECT * FROM "@daniel"."Profissionais_APS_tese"'

oferta_aps <- sqlQuery(channel, 
                       query_prof, 
                       as.is = TRUE) 

oferta_aps <- oferta_aps |> 
  filter(categoria_profissional == "Médicos") |> 
  mutate(ANO = as.numeric(ANO)) |>    
  mutate(MES = as.numeric(MES)) |> 
  mutate(FTE40 = as.numeric(FTE40)) |> 
  filter(ANO >= 2014 & ANO <= 2019) |> 
  janitor::clean_names()

write.csv(oferta_aps, 
          "~/GitHub/materno_infantil/01_dados/oferta_aps_1419.csv")



todos <- 0
ttd <- 133
acoes_educacionais <- 40
consultas <- 30

oferta_vs_demanda1419 <- 
  
  function(acoes_educacionais,
           consultas, ttd, 
           indireta, todos){
    
    servicos14_19_tratado <- 
      servicos14_19 |> 
      left_join(cobertura, 
                by = c("cod_regsaude"=
                         "cod_regsaud")) |> 
      janitor::clean_names() |> 
      mutate(cobertura = cobertura/100) |> 
      rename(cobertura_ans = cobertura) 
    
    
    sus <- 1
    
    
    if(sus == todos){
      servicos14_19_tratado <- servicos14_19_tratado |> 
        mutate(qtd_proc = qtd_proc)
    } else {
      servicos14_19_tratado <- servicos14_19_tratado |>
        mutate(qtd_proc = (qtd_nascidos - 
                             (qtd_nascidos * cobertura_ans)) * 
                 parametro)
    }
    
    servicos14_19_tratado <- 
      servicos14_19_tratado |> 
      select(ano_proc_rea, uf_sigla, cod_regsaude, 
             regiao_saude, qtd_nascidos, cobertura_ans, 
             procedimento, tipo_procedimento, publico, 
             nivel_atencao, parametro, mes_proc_rea, 
             qtd_proc)
    
    # Traduzir número de horas em número de profissionais necessários
    
    necessidade14_19 <- 
      servicos14_19_tratado |> 
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
                nec_ch = sum(total_horas)) |> 
      mutate(mes = month(mes_proc_rea),
             ano = year(mes_proc_rea), 
             .after = mes_proc_rea)
    
    oferta_vs_demanda14_19 <- 
      necessidade14_19 |> 
      left_join(oferta_tratado1419, 
                by = c("cod_regsaude"="cod_regsaud",
                       "mes"="mes",
                       "ano"="ano")) |> 
      left_join(foco_clinico, 
                by = c("cod_regsaude"="cod_regsaud")) |> 
      mutate(oferta_direta =  fte40 * (1 - indireta),
             oferta_linha = oferta_direta * perc_fc) |> 
      mutate(total_abs = oferta_linha - nec_prof,
             total_perc = 100 * (oferta_linha/nec_prof)) |> 
      group_by(ano, uf_sigla, cod_regsaude, regiao_saude) |> 
      summarise(necessidade_media = mean(nec_prof),
                oferta_media = mean(oferta_linha)) |> 
      mutate(perc = 100 * (oferta_media/necessidade_media)) |> 
      mutate(total = (oferta_media - necessidade_media)) 
    
  }

teste <- oferta_vs_demanda1419(acoes_educacionais = 40,
                               consultas = 30, 
                               ttd = 133, 
                               indireta = 0.40, 
                               todos = 0)