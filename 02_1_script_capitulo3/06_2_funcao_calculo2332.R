
library(RODBC)
library(tidyverse)


# Demanda -----------------------------------------------------------------

dremio_host <- Sys.getenv("endereco")
dremio_port <- Sys.getenv("port")
dremio_uid <- Sys.getenv("uid")
dremio_pwd <- Sys.getenv("datalake")


channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;
                                     HOST=%s;
                                     PORT=%s;
                                     UID=%s;
                                     PWD=%s;
                                     AUTHENTICATIONTYPE=Basic Authentication;
                                     CONNECTIONTYPE=Direct", 
                                     dremio_host, 
                                     dremio_port, 
                                     dremio_uid, 
                                     dremio_pwd))

query <- 'SELECT * FROM "@daniel"."servicos_NV_24_30"'

servicos2432 <- sqlQuery(channel, 
                     query, 
                     as.is = TRUE) 


servicos24_32_tratado <- 
  servicos2432 |> 
  rename(mes_proc_rea = mês_procedimento_realizado,
         qtd_proc = quantidade, 
         qtd_nasc = qtd,
         cod_regsaude = cod_regsaud,
         publico = Público,
         mes_programado = mes) |> 
  mutate(mes_programado = as.numeric(mes_programado)) |> 
  mutate(ano_proc_rea = 
           year(mes_proc_rea)) |> 
  filter(nivel_atencao == "APS" & 
           publico != "Gestantes de Alto Risco") |>
  filter(tipo_procedimento == "Ações Educacionais" |
           tipo_procedimento == "Consultas ou Visitas") |> 
  filter(mes_programado < 35) |> 
  filter(procedimento != "Avaliação odontológica") |> 
  filter(procedimento != "Visita domiciliar") |> 
  mutate(cod_regsaude = as.numeric(cod_regsaude)) |> 
  filter(ano_proc_rea > 2023) |> 
  mutate(mes_proc_rea = as.Date(mes_proc_rea))

# verificando visualmente

servicos24_32_tratado |> 
  filter(cod_regsaude == "11001") |> 
  filter(procedimento == "Consulta pré-natal") |> 
  group_by(mes_proc_rea) |> 
  summarise(qtd_proc = sum(qtd_proc)) |> 
  ungroup() |> 
  ggplot(aes(x = mes_proc_rea, 
             y = qtd_proc)) +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5)) + 
  ylim(0, 3000) +
  theme_bw() +
  xlab("Mês/ano") + 
  ylab("Quantidade de procedimentos")




# Carregando dados

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) |> 
  mutate(cod_regsaud = 
           as.character(cod_regsaud)) |> 
  janitor::clean_names() |> 
  select(uf_sigla, cod_regsaud, 
         regiao_saude, cobertura)

cobertura$cod_regsaud <- 
  as.numeric(cobertura$cod_regsaud)

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/01_dados/foco_clinico_projetado.csv") |> 
  select(-`...1`)

# Serviços ----------------------------------------------------------------

# este bloco é usado para selecionar as observações dos melhores modelos de projeção 


write.csv(servicos, 
            "~/GitHub/materno_infantil/02_1_script_capitulo3/06_servicos/servicos19_32_tratado.csv")



# Oferta ------------------------------------------------------------------

oferta_aps <- read_csv("~/GitHub/materno_infantil/01_dados/oferta_aps07_25.csv") |> 
                  select(-`...1`) |> 
                  mutate(competen = as.character(competen)) |>
                  mutate(ds = ymd(paste0(substr(competen, 1, 4), "-", 
                                               substr(competen, 5, 6), "-01"))) |> 
                  mutate(cenario = "reais") |> 
                  filter(ds >= "2019-01-01" & 
                         ds < "2025-02-01") |> 
                  rename(uf = uf_sigla,
                         regiao_saude = regiao_saude_pad,
                         oferta = fte40,
                         categoria = categoria_profissional,
                         CBO = cbo_recod) |> 
                  select(ds, cod_regsaud, uf,
                         regiao_saude, regiao, cenario, 
                         oferta, categoria, CBO)
  
previsao_oferta_enf <-  
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/dfs_projecoes/projecao_enfermeiros_combinados.csv") |> 
  pivot_longer(
    cols = c(cenario_base, cenario_aumento, cenario_reducao),
    names_to = "cenario",
    values_to = "oferta"
  ) |> 
  mutate(categoria = "Enfermeiros") |> 
  mutate(CBO = "2235") |> 
  select(-arquivo_origem)
  
previsao_oferta_med <- 
  vroom::vroom("~/GitHub/materno_infantil/02_1_script_capitulo3/09_output_projecao_oferta/dfs_projecoes/projecao_medicos_combinados.csv") |> 
  pivot_longer(
    cols = c(cenario_base, cenario_aumento, cenario_reducao),
    names_to = "cenario",
    values_to = "oferta"
  ) |> 
  mutate(categoria = "Médicos") |> 
  mutate(CBO = "2231") |> 
  select(-arquivo_origem)

oferta <- rbind(oferta_aps,
                previsao_oferta_enf,
                previsao_oferta_med)


oferta |> 
  filter(cod_regsaud == "16003") |> 
  ggplot(aes(x = ds, y = oferta, 
             col = cenario)) + 
  geom_smooth(method="loess", span=0.3) + 
  facet_wrap(~categoria)

# Executando o passo a passo sem função -----------------------------------

sus = 1
todos = 0
alto_risco = 0.15 #novo parametro para a funcao

if(sus == todos){
  
  servicos_tratado <- 
    
    servicos |> 
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
  
  servicos_tratado <- 
    
    servicos |>
    mutate(qtd_proc = ((qtd_nascidos - 
                      (qtd_nascidos * (cobertura/100)))) * 
             parametro_recod) |> 
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
  servicos_tratado |> 
  select(ano_proc_rea, mes_proc_rea, uf_sigla, cod_regsaude, 
         regiao_saude, qtd_nascidos, cobertura, sigtap_recod,
         procedimento, tipo_procedimento, mes_programado,
         publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
         qtd_proc_ar)



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
  mutate(nec_med = (((total_horas_rh * 0.50) + (total_horas_ar * 0.60))/ttd)) |> 
  mutate(nec_enf = (((total_horas_rh * 0.50) + (total_horas_ar * 0.40))/ttd))

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
  filter(ano >= 2019 & ano < 2030)

# Visualizando para uma região de saúde

necessidade_tratada |> 
  filter(cod_regsaude == "12003") |>  
  ggplot(aes(x = mes_proc_rea, 
             y = nec_enf,
             fill = regiao_saude)) + 
  geom_col() + theme_minimal() + 
  xlab("Mês de realização de procedimento") + 
  ylab("Necessidade de enfermeiros")

# necessidade dividido por categorias

comparacao <- necessidade_tratada |> 
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


