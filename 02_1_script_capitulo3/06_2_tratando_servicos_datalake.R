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


# write.csv(servicos24_32_tratado,
#           "~/GitHub/materno_infantil/02_1_script_capitulo3/06_servicos/servicos24_30_tratado.csv")

