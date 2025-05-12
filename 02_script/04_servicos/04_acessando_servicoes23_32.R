library(tidyverse)
library(RODBC)

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

query_servico <- 
  'SELECT * FROM "@daniel"."procedimentos_materno_infantil_19_32"'


servicos <- sqlQuery(channel, 
                     query_servico, 
                     as.is = TRUE)

# write.csv(servicos, 
#            "~/GitHub/materno_infantil/02_script/04_servicos/servicos19_32.csv")


query_servico23 <- 
  'SELECT * FROM "@daniel"."procedimentos_materno_infantil_2023"'


servicos_23 <- sqlQuery(channel, 
                     query_servico23, 
                     as.is = TRUE)


servicos23_tratado <- 
  servicos_23 |> 
  filter(.model_desc == "Dados Históricos"|
         .model_desc == "PROPHET")

prenatal <- servicos23_tratado |> 
  filter(cod_regsaude == "11001") |> 
  filter(procedimento == "Consulta puerperal")

prenatal |> 
  ggplot(aes(x = mes_proc_rea, 
             y = qtd_proc, 
             fill = .model_desc)) + 
  geom_col(position = "dodge") +
  theme_minimal()

servicos_actual <- servicos_23 |> 
  filter(.model_desc == "ACTUAL" |
         .model_desc == "Dados Históricos"|
         .model_desc == "PROPHET") 

servicos_historico <- servicos_23 |> 
  filter(.model_desc == "Dados Históricos")

write.csv(servicos_actual, 
          "~/GitHub/materno_infantil/02_script/04_servicos/servicos23.csv")


