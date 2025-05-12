
library(tidyverse)
library(RODBC)
library(patchwork)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(scales)
library(ggspatial) 
library(sf)
library(readxl)
library(leaflet)

# Leitura dos dados

servicos <- 
  read_csv("~/GitHub/materno_infantil/04_analises_UF_cap3/02_servicos/servicos_UF_1733.csv") |> 
  select(-`...1`) |> 
  rename(mes_proc_rea = mês_procedimento_realizado,
         publico = Público,
         qtd_proc = quantidade,
         qtd_nascidos = qtd,
         mes_programado = mes)

oferta <- 
  read_csv("~/GitHub/materno_infantil/04_analises_UF_cap3/04_projecao_oferta_uf/dfs_projecoes/oferta_proj_UF.csv") |> 
  select(-`...1`) |> 
  mutate(categoria = case_when(
            grepl("Enfermeiros", origem) ~ "Enfermeiro",
            grepl("Médicos", origem) ~ "Médico"
  )) |> 
  select(-origem) |> 
  pivot_longer(
    cols = starts_with("cenario"),
    names_to = "tipo_cenario",
    values_to = "fte40"
  ) |> 
  mutate(tipo_cenario = gsub("cenario_", "", 
                             tipo_cenario)) |> 
  mutate(cbo_recod = if_else(categoria == "Enfermeiro",
                             "2235","225"))


oferta_atual <- 
  read_csv("~/GitHub/materno_infantil/04_analises_UF_cap3/04_projecao_oferta_uf/oferta_aps_uf_2019_2025.csv") |> 
  rename(ds = data_formatada,
         categoria = categoria_profissional) |> 
  mutate(tipo_cenario = "dados reais") |> 
  select(-`...1`) |> 
  select(ds, uf_sigla, regiao, categoria, tipo_cenario,
         fte40, cbo_recod)

oferta <- rbind(oferta, 
                oferta_atual)  |> 
          filter(ds < "2031-01-01")


cobertura <- 
  read_csv("~/GitHub/materno_infantil/01_dados/ans_uf.csv") |> 
  janitor::clean_names() |> 
  mutate(cobertura = benef/pop)

foco_clinico <- 
  read_csv("~/GitHub/materno_infantil/04_analises_UF_cap3/05_projecao_FC_outputs/fc_UF_projetado.csv") |>  
  select(UF, perc_fc, ano)

fc_2019_2024 <- read_excel("~/GitHub/materno_infantil/01_dados/fc_2019_2024.xlsx") |> 
                  rename(UF = uf_sigla, 
                         ano = Ano) |> 
                  select(UF, perc_fc, ano) |> 
                  filter(ano != 2024)

foco_clinico <- rbind(foco_clinico,
                      fc_2019_2024)

# Função ------------------------------------------------------------------

acoes_educacionais <- 30
consultas <- 30
ttd <- 133
indireta <- 0.40
todos <- 0
ferias <- 16
feriados <- 4
absenteismo <- 16 #novo parametro da funcao
consultas_altorisco <- 45
alto_risco <- 0.15
acoes_altorisco <- 50
perc_med <- 50

# oferta_vs_demanda <- 
#   
#   function(acoes_educacionais,
#            consultas, 
#            alto_risco,
#            indireta, 
#            todos, 
#            acoes_altorisco, 
#            consultas_altorisco,
#            absenteismo){

# se todos = 1, pegar todos 
# se todos = 0, pegar apenas SUS dependente

sus <- 1
ferias <- 16
feriados <- 4

# Filtrar procedimentos
servicos23_32 <- 
  servicos |> 
  mutate(ano_proc_rea = 
           year(mes_proc_rea)) |> 
  filter(ano_proc_rea >= 2019 & ano_proc_rea <= 2030) |> 
  filter(nivel_atencao == "APS" & 
         publico != "Gestantes de Alto Risco") |>
  filter(tipo_procedimento == "Ações Educacionais" |
         tipo_procedimento == "Consultas ou Visitas") |> 
  filter(procedimento != "Avaliação odontológica") |> 
  filter(procedimento != "Visita domiciliar") 

# Juntar dados de serviços com cobertura

servicos23_32_tratado <- 
  servicos23_32 |> 
  left_join(cobertura, 
            by = c("uf"=
                   "uf_sigla")) |> 
  janitor::clean_names() |> 
  rename(cobertura_ans = cobertura) 

todos = 0

if(sus == todos){
  servicos23_32_tratado <- servicos23_32_tratado |> 
    
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
  servicos23_32_tratado <- 
    servicos23_32_tratado |>
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
  servicos23_32_tratado |> 
  select(ano_proc_rea, mes_proc_rea, uf, 
         qtd_nascidos, cobertura_ans, codigo_sigtap,
         procedimento, tipo_procedimento, mes_programado,
         publico, nivel_atencao, parametro, qtd_proc, qtd_proc_rh, 
         qtd_proc_ar)

# Traduzir número de horas em número de profissionais necessários

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
  mutate(nec_prof = (total_horas_rh + total_horas_ar)/ttd) 

necessidade_tratada <- 
  necessidade |> 
  group_by(uf,  
           mes_proc_rea) |> 
  summarise(nec_prof = sum(nec_prof),
            nec_ch = sum(total_horas_rh + total_horas_ar)) |> 
  mutate(mes = month(mes_proc_rea),
         ano = year(mes_proc_rea), 
         .after = mes_proc_rea) |> 
  mutate(necessidade_medico = nec_prof * perc_med/100, 
         necessidade_enf = nec_prof * (1 - perc_med/100)) |> 
  pivot_longer(
    cols = starts_with("necessidade_"),
    names_to = "categoria",
    values_to = "necessidade"
  ) |> 
  mutate(categoria = gsub("necessidade_", "", 
                             categoria)) |> 
  mutate(cbo_recod = if_else(categoria == "enf",
                             "2235","225")) |> 
  mutate(categoria = if_else(categoria == "enf",
                             "Enfermeiro","Médico")) |> 
  select(uf, mes_proc_rea, cbo_recod, necessidade)
  

# tratando oferta 

oferta_tratada <- 
  oferta |> 
  mutate(oferta_direta =  fte40 * (1 - indireta)) |> 
  mutate(ano = year(ds)) |> 
  left_join(foco_clinico, by = c("uf_sigla"="UF",
                                 "ano"="ano")) |> 
  mutate(oferta_linha = oferta_direta * perc_fc) 
  


# Juntar necessidades com oferta_linha
# Juntar necessidades com oferta


oferta_vs_demanda <- 
  oferta_tratada |> 
    left_join(necessidade_tratada, 
              by = c("uf_sigla"="uf",
                     "cbo_recod"="cbo_recod",
                     "ds"="mes_proc_rea")) |> 
    mutate(rr = oferta_linha/necessidade) |> 
    mutate(categoria = case_when(categoria == "Enfermeiros" ~ "Enfermeiro",
                                 categoria == "Médicos" ~ "Médico",
                                 TRUE ~ categoria))


oferta_vs_demanda_anual <- 
  oferta_vs_demanda |> 
    group_by(regiao, uf_sigla, ano,
             tipo_cenario, categoria) |> 
    summarize(oferta = mean(oferta_linha),
              necessidade = mean(necessidade)) |> 
    mutate(rr = oferta/necessidade)


#}


oferta_vs_demanda |> 
  filter(regiao == "Região Norte") |> 
  filter(categoria == "Enfermeiro") |> 
  ggplot(aes(x = ds, y = rr, 
             col = tipo_cenario)) + 
  geom_line() + facet_wrap(~uf_sigla) 


oferta_vs_demanda |> 
  filter(regiao == "Região Nordeste") |> 
  filter(categoria == "Enfermeiro") |>  
  filter(tipo_cenario == "base" | 
         tipo_cenario == "dados reais") |> 
  ggplot() + 
  geom_smooth(aes(x = ds, 
                y = oferta_linha),
            col = "red", method = "loess") +
  geom_smooth(aes(x = ds,
                y = necessidade),
                col = "blue", method = "loess") +
  facet_wrap(~uf_sigla,
             scales = "free_y") +
  ylim(0, 2000)


oferta_vs_demanda_anual |> 
  filter(regiao == "Região Norte") |> 
  filter(categoria == "Enfermeiro") |>  
  filter(tipo_cenario == "base" | 
           tipo_cenario == "dados reais") |> 
  ggplot() + 
  scale_x_continuous(breaks = seq(2019, 
                                  2030, 
                                  1)) +
  geom_line(aes(x = ano, 
                y = oferta),
            col = "red") +
  geom_line(aes(x = ano,
                y = necessidade),
            col = "blue") +
  facet_wrap(~uf_sigla,
             scales = "free_y")  



# Testando função ---------------------------------------------------------

# teste <- oferta_vs_demanda(acoes_educacionais = 30,
#                            alto_risco = 0.15,
#                            acoes_altorisco = 45,
#                            consultas = 30, 
#                            absenteismo = 8, 
#                            indireta = 0.50,
#                            todos = 1,
#                            consultas_altorisco = 40)
# 
# teste2 <- oferta_vs_demanda(acoes_educacionais = 30,
#                             alto_risco = 0.50,
#                             acoes_altorisco = 45,
#                             consultas = 30, 
#                             absenteismo = 8, 
#                             indireta = 0.50,
#                             todos = 1,
#                             consultas_altorisco = 40)


