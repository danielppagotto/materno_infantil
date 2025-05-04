
library(tidyverse)
library(vroom)
library(readxl)
library(RODBC)

resultados_regioes <- 
  read_csv("~/GitHub/materno_infantil/02_script/08_output_gráficos/cenarios_selecionados.csv") |> 
  mutate(regiao = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  ))



# baixando os dados -------------------------------------------------------

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

query <- 'SELECT * FROM "@daniel"."rendimentos medicos 2019"'


rendimentos <- sqlQuery(channel, 
                        query, 
                         as.is = TRUE) |> 
  mutate(uf_recod = case_when(
    UF == "Acre" ~ "AC",
    UF == "Alagoas" ~ "AL",
    UF == "Amapá" ~ "AP",
    UF == "Amazonas" ~ "AM",
    UF == "Bahia" ~ "BA",
    UF == "Ceará" ~ "CE",
    UF == "Distrito Federal" ~ "DF",
    UF == "Espírito Santo" ~ "ES",
    UF == "Goiás" ~ "GO",
    UF == "Maranhão" ~ "MA",
    UF == "Mato Grosso" ~ "MT",
    UF == "Mato Grosso do Sul" ~ "MS",
    UF == "Minas Gerais" ~ "MG",
    UF == "Pará" ~ "PA",
    UF == "Paraíba" ~ "PB",
    UF == "Paraná" ~ "PR",
    UF == "Pernambuco" ~ "PE",
    UF == "Piauí" ~ "PI",
    UF == "Rio de Janeiro" ~ "RJ",
    UF == "Rio Grande do Norte" ~ "RN",
    UF == "Rio Grande do Sul" ~ "RS",
    UF == "Rondônia" ~ "RO",
    UF == "Roraima" ~ "RR",
    UF == "Santa Catarina" ~ "SC",
    UF == "São Paulo" ~ "SP",
    UF == "Sergipe" ~ "SE",
    UF == "Tocantins" ~ "TO",
    TRUE ~ NA_character_  # Se quiser deixar NA caso não encontre correspondência
  )) |> 
  mutate(regiao = case_when(
    uf_recod %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_recod %in% c("PR", "SC", "RS") ~ "Sul",
    uf_recod %in% c("AC", "AM", "AP", "PA", 
                    "RO", "RR", "TO") ~ "Norte",
    uf_recod %in% c("AL", "BA", "CE", 
                    "MA", "PB", "PE", "PI", 
                    "RN", "SE") ~ "Nordeste",
    uf_recod %in% c("DF", "GO", 
                    "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  ))

writexl::write_xlsx(rendimentos,
                    "~/GitHub/materno_infantil/02_script/11_rendimentos/rendimentos_uf.xlsx")

# Juntando bases ----------------------------------------------------------

rendimento_medio_regiao <- rendimentos |> 
                              group_by(regiao) |> 
                              summarise(renda = mean(rendimento))


resultado_tratado <- 
  resultados_regioes |> 
  left_join(rendimentos, 
            by = c("uf_sigla"="uf_recod")) |> 
  mutate(lacuna = (total * rendimento * 1.40)) |> 
  group_by(uf_sigla, cenario) |> 
  summarise(resultado_absoluto = sum(total),
            valor = sum(lacuna)) |> 
  mutate(resultado_absoluto = round(resultado_absoluto),
         valor = round(valor)) |> 
  pivot_wider(
    names_from = cenario,
    values_from = c(resultado_absoluto, valor)
  ) 

writexl::write_xlsx(resultado_tratado, 
                    "~/GitHub/materno_infantil/02_script/11_rendimentos/rendimentos_necessarios.xlsx")

