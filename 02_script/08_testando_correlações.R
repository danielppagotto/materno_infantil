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

query <- 
  'SELECT * FROM "@daniel"."taxa_mortalidade_materna"'

query_pib <- 
  'SELECT * FROM "@daniel"."pib_regioes_saude"'

pbf_query <- 
  'SELECT * FROM "@daniel"."PBF_regiao_saude"'

taxa_mortalidade <- sqlQuery(channel, 
                     query, 
                     as.is = TRUE)

pbf <- sqlQuery(channel, pbf_query, 
                as.is = TRUE) |> 
       mutate(valor_familia = valor_repasse/qtd_familias)

pib <- sqlQuery(channel, 
                query_pib, 
                as.is = TRUE) |>  
  filter(ano == "2019") |> 
  select(regiao, cod_regsaud, ppc)

taxa_mortalidade$total_nv <- 
  as.numeric(taxa_mortalidade$total_nv)

taxa_mortalidade$total_obitos_mat <- 
  as.numeric(taxa_mortalidade$total_obitos_mat)

taxa_mortalidade <- 
  taxa_mortalidade |> 
  mutate(mortalidade_materna = 
           (total_obitos_mat/total_nv) * 1000)


resumo_regiao <- 
  read_csv("02_script/07_output_montecarlo/resumo_regiao.csv") |> 
  select(cod_regsaude, media_percentual) |> 
  mutate(cod_regsaude = as.character(cod_regsaude))

base_modelo <- resumo_regiao |> 
  left_join(taxa_mortalidade, 
            by = c("cod_regsaude"="cod_regsaud")) |>
  mutate(mortalidade_materna = 
           if_else(is.na(mortalidade_materna), 
                   0, 
                   mortalidade_materna)) |> 
  left_join(pib, 
            by = c("cod_regsaude"="cod_regsaud")) |> 
  mutate(ppc_log = log(ppc)) |>
  left_join(pbf, 
            by = c("cod_regsaude"="cod_regsaud"))
  

modelo <- lm("mortalidade_materna ~ 
             media_percentual + ppc_log + valor_familia",
             data = base_modelo)

summary(modelo)

hist(modelo$residuals)

base_modelo |> 
  select(media_percentual, mortalidade_materna, ppc_log, 
         valor_familia) |> 
  GGally::ggpairs()

base_modelo |> 
  ggplot(aes(x = mortalidade_materna,
             y = media_percentual, 
             col = regiao)) + geom_point() +
  facet_wrap(~regiao) + theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE)

