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

#write.csv(servicos, 
#           "~/GitHub/materno_infantil/02_script/04_servicos/servicos19_32.csv")
