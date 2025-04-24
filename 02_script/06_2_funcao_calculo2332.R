
library(RODBC)
library(tidyverse)


# Demanda -----------------------------------------------------------------

servicos23_32 <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script/04_servicos/servicos23_32.csv") |> 
  select(-`...1`)

# Carregando dados

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) 

cobertura$cod_regsaud <- 
  as.numeric(cobertura$cod_regsaud)

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv") |> 
  select(-`...1`)

# Serviços ----------------------------------------------------------------

# este bloco é usado para selecionar as observações dos melhores modelos de projeção 

melhor_modelo <- read.csv("~/GitHub/materno_infantil/02_script/03_outputs_projecoes/prophet/metricas_previsoes_prophet/melhor_modelo.csv") |> 
  mutate(cod_regsaud = 
           as.character(cod_regsaud)) |> 
  mutate(reg_concat = 
                      paste0(cod_regsaud, 
                             .model_desc))

vetor <- melhor_modelo$reg_concat

servicos23 <- servicos23_32 |> 
                filter(mes_proc_rea < "2024-01-01") |> 
                mutate(reg_concat = "dados reais")

cobertura <- vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
                select(-`...1`) |> 
             mutate(cod_regsaud = 
                      as.character(cod_regsaud)) |> 
             janitor::clean_names()

# Abaixo filtramos os modelos com melhores resultados 

servicos_demais <- servicos23_32 |> 
                    filter(mes_proc_rea >= "2024-01-01") |> 
                    mutate(cod_regsaude = 
                             as.character(cod_regsaude)) |> 
                    mutate(reg_concat = 
                             paste0(cod_regsaude,
                                    .model_desc)) |> 
                    filter(reg_concat %in% vetor)

servicos <- rbind(servicos23, 
                  servicos_demais) |> 
            mutate(ano_proc_rea = 
                     year(mes_proc_rea)) |> 
            filter(nivel_atencao == "APS" & 
                     publico != "Gestantes de Alto Risco") |>
            filter(tipo_procedimento == "Ações Educacionais" |
                     tipo_procedimento == "Consultas ou Visitas") |> 
            filter(procedimento != "Avaliação odontológica") |> 
            filter(procedimento != "Visita domiciliar") |> 
            left_join(cobertura, 
                      by = c("cod_regsaude"="cod_regsaud"))

# write.csv(servicos, 
#            "~/GitHub/materno_infantil/02_script/04_servicos/servicos23_32_tratado.csv")


# Oferta ------------------------------------------------------------------

oferta_enf <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script/09_output_projecao_oferta/dfs_projecoes/projecao_enfermeiros_combinados.csv") |> 
  rename(cen_base_enf = cenario_base,
         cen_aumento_enf = cenario_aumento,
         cen_reducao_enf = cenario_reducao)

oferta_med <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script/09_output_projecao_oferta/dfs_projecoes/projecao_medicos_combinados.csv") |> 
  rename(cen_base_med = cenario_base,
         cen_aumento_med = cenario_aumento,
         cen_reducao_med = cenario_reducao) |> 
  select(-uf, -regiao_saude, 
         - regiao, - arquivo_origem)

oferta <- 
  oferta_enf |> 
  left_join(oferta_med, 
            by = c("ds"="ds",
                   "cod_regsaud"="cod_regsaud")) |> 
  mutate(ano = year(ds),
         mes = month(ds))


# Função ------------------------------------------------------------------



todos <- 0
ttd <- 133
acoes_educacionais <- 40
consultas <- 30

oferta_vs_demanda <- 
  
  function(acoes_educacionais, consultas, ttd, 
           indireta, todos, mix, dinamica_enf,
           dinamica_med){
    
    sus <- 1
    
    oferta$cenario_enf <- dinamica_enf
    oferta$cenario_med <- dinamica_med
    
    if(sus == todos){
      servicos_tratado <- servicos |> 
        mutate(qtd_proc = qtd_proc)
    } else {
      servicos_tratado <- servicos |>
        mutate(qtd_proc = (qtd_nascidos - 
                             (qtd_nascidos * cobertura)) * 
                          parametro)
    }
    
    servicos_tratado <- 
      servicos |> 
      select(ano_proc_rea, uf_sigla, cod_regsaude, 
             regiao_saude, qtd_nascidos, cobertura, 
             procedimento, tipo_procedimento, publico, 
             nivel_atencao, parametro, mes_proc_rea, 
             qtd_proc)
    
    # Traduzir número de horas em número de profissionais necessários
    
    necessidade <- 
      servicos_tratado |> 
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
             .after = mes_proc_rea) |> 
      mutate(nec_medicos = mix * nec_prof,
             nec_enfermeiros = (1 - mix) * nec_prof)
    
    oferta_vs_demanda<- 
      necessidade |> 
      left_join(oferta, 
                by = c("cod_regsaude"="cod_regsaud",
                       "mes"="mes",
                       "ano"="ano")) |> 
      left_join(foco_clinico, 
                by = c("cod_regsaude"="cod_regsaud",
                       "ano"="ano")) |> 
      mutate(fte40 = case_when(
                        cenario == "1" ~   
      
      ))
      ### aqui vai entrar o tipo de cenário
      
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