
library(tidyverse)

# Carregando dados

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/10_cenarios_ans/cenarios_ans_projetado.csv") |> 
  select(-`...1`) |> 
  mutate(cod_regsaud = 
           as.character(cod_regsaud)) |> 
  janitor::clean_names() |> 
  filter(ano > 2023) |> 
  select(ano, cod_regsaud, cobertura, cenario) 
  
cobertura$cod_regsaud <- 
  as.numeric(cobertura$cod_regsaud)

servicos2332 <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/06_servicos/servicos24_30_tratado.csv") |> 
  select(-`...1`) 

servicos_pecanha <- servicos |> 
                        filter(cod_regsaude == "31080")

# a função pode assumir os seguintes valores: 
# todos, aumento, constante, diminuicao

cenario_ans <- function(cenario_sus_dependente){
  
  if(cenario_sus_dependente == "todos" ){
    
    cobertura_ajustado <- cobertura |> 
                            filter(cenario == "aumento") |> 
                            mutate(cenario = "Sem dedução") |> 
                            mutate(cobertura = 0)
  } else {
  
  cobertura_ajustado <- 
    cobertura |> 
      filter(cenario == cenario_sus_dependente)
  }
  
  return(cobertura_ajustado)
}

ans <- cenario_ans(cenario_sus_dependente = "aumento") |> 
          rename(cenario_ans = cenario)

# Oferta ------------------------------------------------------------------

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv") |> 
  select(-`...1`)

oferta_aps <- read_csv("~/GitHub/materno_infantil/01_dados/oferta_aps07_25.csv") |> 
                  select(-`...1`) |> 
                  mutate(competen = as.character(competen)) |>
                  mutate(ds = ymd(paste0(substr(competen, 1, 4), "-", 
                                               substr(competen, 5, 6), "-01"))) |> 
                  mutate(cenario = "reais") |> 
                  filter(ds >= "2024-01-01" & 
                         ds <= "2025-01-01") |> 
                  rename(uf = uf_sigla,
                         regiao_saude = regiao_saude_pad,
                         oferta = fte40,
                         categoria = categoria_profissional,
                         CBO = cbo_recod) |> 
                  select(ds, cod_regsaud, uf,
                         regiao_saude, regiao, cenario, 
                         oferta, categoria, CBO)
  
previsao_oferta_enf <-  
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_enfermeiros_combinados.csv") |> 
  pivot_longer(
    cols = c(cenario_base, cenario_aumento, cenario_reducao),
    names_to = "cenario",
    values_to = "oferta"
  ) |> 
  mutate(categoria = "Enfermeiros") |> 
  mutate(CBO = "2235") |> 
  select(-arquivo_origem) |> 
  filter(ds >= "2025-01-01")
  
previsao_oferta_med <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/projecao_medicos_combinados.csv") |> 
  pivot_longer(
    cols = c(cenario_base, cenario_aumento, cenario_reducao),
    names_to = "cenario",
    values_to = "oferta"
  ) |> 
  mutate(categoria = "Médicos") |> 
  mutate(CBO = "2231") |> 
  select(-arquivo_origem) |> 
  filter(ds >= "2025-01-01")

oferta <- rbind(oferta_aps,
                previsao_oferta_enf,
                previsao_oferta_med) |> 
          mutate(ano = year(ds)) |> 
          left_join(foco_clinico, 
                      by = c("cod_regsaud"="cod_regsaud",
                             "ano"="ano")) |> 
          filter(ds < "2031-01-01")

oferta_anual <- 
  oferta |> 
  group_by(ano, uf, cod_regsaud, 
           regiao_saude, cenario, 
           categoria, CBO) |> 
  summarise(oferta = mean(oferta))


oferta_anual |> 
  filter(cod_regsaud == "16003") |>
  rename(Cenário = cenario) |> 
  ggplot(aes(x = ano, y = oferta, 
             col = Cenário)) + 
  geom_smooth(method="loess",
              se = FALSE) + 
  facet_wrap(~categoria) + 
  theme_minimal() +
  xlab("Ano") + 
  ylim(0, 200)

cenario_oferta <- 
  
  function(oferta_cenario){
  
    oferta_ajustada <- 
      oferta |> 
      filter(cenario == oferta_cenario |
             cenario == "reais")
  
  return(oferta_ajustada)
}


oferta_resultante <- 
        cenario_oferta(oferta_cenario = "cenario_base") |> 
        mutate(ano = year(ds),
               mes = month(ds)) |> 
        select(-regiao_saude, -uf,
               -direcao, -variacao_percentual) 

# Executando o passo a passo sem função -----------------------------------

alto_risco <- 0.15 #novo parametro para a funcao

servicos_tratado <- 
  servicos |> 
  left_join(ans, 
            by = c("cod_regsaude"="cod_regsaud",
                   "ano_proc_rea"="ano")) |> 
  mutate(qtd_cobertura = 
           qtd_nasc - (qtd_nasc * (cobertura/100))) |> 
  mutate(qtd_proc = parametro * qtd_cobertura) |> 
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
             ))  |> 
  select(ano_proc_rea, mes_proc_rea, uf_sigla, 
         cod_regsaude, regiao_saude, qtd_nasc, 
         cobertura, cenario_ans, codigo_sigtap,
         procedimento, tipo_procedimento, mes_programado,
         publico, nivel_atencao, parametro, 
         qtd_proc, qtd_proc_rh, qtd_proc_ar)


# Necessidade de profissionais --------------------------------------------

acoes_educacionais <- 45
acoes_altorisco <- 60
consultas <- 30
consultas_altorisco <- 45
ferias <- 16
feriados <- 4
absenteismo <- 16 #novo parametro da funcao

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
  mutate(nec_med = (((total_horas_rh * 0.50) + (total_horas_ar * 0.50))/ttd)) |> 
  mutate(nec_enf = (((total_horas_rh * 0.50) + (total_horas_ar * 0.50))/ttd))

# parametros que precisam ser ajustados (0.50 e 0.60)

necessidade_tratada <- 
  necessidade |> 
  group_by(uf_sigla, cod_regsaude, regiao_saude,  
           mes_proc_rea) |> 
  summarise(nec_med = sum(nec_med),
            nec_enf = sum(nec_enf)) |> 
  mutate(mes = month(mes_proc_rea),
         ano = year(mes_proc_rea), 
         .after = mes_proc_rea) |> 
  filter(ano >= 2024 & ano < 2031)

# Visualizando para uma região de saúde

necessidade_tratada |> 
  filter(cod_regsaude == "12003") |>  
  ggplot(aes(x = mes_proc_rea, 
             y = nec_enf,
             fill = regiao_saude)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() + 
  xlab("Mês de realização de procedimento") + 
  ylab("Necessidade de enfermeiros") + 
  ylim(0, 25) +
  theme(legend.position = "none") 

# necessidade dividido por categorias

oferta_enf <- oferta_resultante |> 
  filter(categoria == "Enfermeiros")

oferta_med <- oferta_resultante |> 
  filter(categoria == "Médicos")

comparar_enf <- 
  necessidade_tratada |> 
    left_join(oferta_enf, 
            by = c("mes"="mes",
                   "ano"="ano",
                   "cod_regsaude"="cod_regsaud",
                   "mes_proc_rea"="ds")) |> 
  mutate(fte40 = oferta * 0.50 * perc_fc) |> 
  mutate(ra = fte40 - nec_enf,
         rr = fte40/nec_enf)

comparar_med <- 
  necessidade_tratada |> 
  left_join(oferta_med, 
            by = c("mes"="mes",
                   "ano"="ano",
                   "cod_regsaude"="cod_regsaud",
                   "mes_proc_rea"="ds")) |> 
  mutate(fte40 = oferta * 0.50 * perc_fc) |> 
  mutate(ra = fte40 - nec_med,
         rr = fte40/nec_med)







comparacao <- 
  necessidade_tratada |> 
  mutate(cod_regsaude = as.numeric(cod_regsaude)) |> 
  left_join(oferta, 
            by = c("ano"="ano",
                   "mes"="mes",
                   "cod_regsaude"="cod_regsaud")) |> 
  left_join(foco_clinico, 
            by = c("ano"="ano",
                   "cod_regsaude"="cod_regsaud")) |> 
  mutate(oferta_base_enf = cen_base_enf * 0.50 * perc_fc,
         oferta_aumento_enf = cen_aumento_enf * 0.50 * perc_fc,
         oferta_reducao_enf = cen_reducao_enf * 0.50 * perc_fc,
         oferta_base_med = cen_base_med * 0.50 * perc_fc,
         oferta_aumento_med = cen_aumento_med * 0.50 * perc_fc,
         oferta_reducao_med = cen_reducao_med * 0.50 * perc_fc) |> 
  mutate(perc_base_enf = oferta_base_enf/nec_enf,
         perc_base_med = oferta_base_med/nec_med,
         perc_aumento_enf = oferta_aumento_enf/nec_enf,
         perc_aumento_med = oferta_aumento_med/nec_med,
         perc_reducao_enf = oferta_reducao_enf/nec_enf,
         perc_reducao_med = oferta_reducao_med/nec_med)
  

# Consulta puerperal - https://www.scielo.br/j/ean/a/kkjnfNwzL8fCRKnVKNmXBvq/#:~:text=A%20realiza%C3%A7%C3%A3o%20do%20retorno%20puerperal,ser%20realizada%20a%20busca%20ativa.
# Consulta pré-natal - https://www.gov.br/saude/pt-br/assuntos/saude-de-a-a-z/g/gravidez/pre-natal#:~:text=A%20gestante%20dever%C3%A1%20procurar%20a,m%C3%A3e%20e%20para%20o%20beb%C3%AA. 

# Montando a Função ------------------------------------------------------------------


