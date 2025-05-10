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


query <- 'SELECT * 
          FROM "@daniel"."Profissionais_APS_tese"
          WHERE ANO = 2023'


oferta_aps <- sqlQuery(channel, 
                       query, 
                       as.is = TRUE) 

# write.csv(oferta_aps,
#           "~/GitHub/materno_infantil/01_dados/oferta_23.csv")
