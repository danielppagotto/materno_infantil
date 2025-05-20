

library(tidyverse)
library(vroom)
library(readxl)
library(RODBC)

rendimentos <- 
  read_excel("~/GitHub/materno_infantil/02_script/11_rendimentos/rendimentos_uf.xlsx") |> 
  mutate(prof = if_else(prof == "Enfermeiro",
                        "Enfermeiros",
                        "Médicos"))

resultados_regioes <- 
  read_csv("~/GitHub/materno_infantil/02_script_tratado/resultados_absolutos_uf.csv")


resultados_enf <- 
  resultados_regioes |> 
  select(uf_sigla, 
         ra_enf, 
         cenario) |> 
  left_join(rendimentos |>  filter(prof == "Enfermeiros"), 
              by = c("uf_sigla"="uf_recod")) |> 
  mutate(lacuna = (ra_enf * rendimento * 1.40)) |> 
  mutate(lacuna = if_else(lacuna > 0, 0, lacuna)) |> 
  mutate(lacuna = lacuna/1000000) |> 
  mutate(lacuna = lacuna *( -1)) |> 
  pivot_wider(
    names_from = cenario,
    values_from = c(ra_enf, lacuna)
  ) |> 
  select(-UF) |> 
  rename(UF = uf_sigla,
         Ano = ano,
         Rendimento = rendimento,
         RA1 = `ra_enf_Cenário 1`,
         gap1 = `lacuna_Cenário 1`,
         RA2 = `ra_enf_Cenário 2`,
         gap2 = `lacuna_Cenário 2`,
         RA3 = `ra_enf_Cenário 3`,
         gap3 = `lacuna_Cenário 3`,
         RA4 = `ra_enf_Cenário 4`,
         gap4 = `lacuna_Cenário 4`) |> 
  mutate(categoria = "Enfermagem") |> 
  select(categoria, UF, Ano, Rendimento, RA1, gap1,
         RA2, gap2, RA3, gap3, RA4, gap4) 
  
  
resultados_med <- 
    resultados_regioes |> 
    select(uf_sigla, 
           ra_med, 
           cenario) |> 
    left_join(rendimentos |>  filter(prof == "Médicos"), 
              by = c("uf_sigla"="uf_recod")) |> 
    mutate(lacuna = (ra_med * rendimento * 1.40)) |> 
    mutate(lacuna = if_else(lacuna > 0, 0, lacuna)) |> 
    mutate(lacuna = lacuna/1000000) |> 
    mutate(lacuna = lacuna *( -1)) |> 
    pivot_wider(
      names_from = cenario,
      values_from = c(ra_med, lacuna)
    ) |> 
    select(-UF) |> 
    rename(UF = uf_sigla,
           Ano = ano,
           Rendimento = rendimento,
           RA1 = `ra_med_Cenário 1`,
           gap1 = `lacuna_Cenário 1`,
           RA2 = `ra_med_Cenário 2`,
           gap2 = `lacuna_Cenário 2`,
           RA3 = `ra_med_Cenário 3`,
           gap3 = `lacuna_Cenário 3`,
           RA4 = `ra_med_Cenário 4`,
           gap4 = `lacuna_Cenário 4`) |> 
    mutate(categoria = "Médicos") |> 
  select(categoria, UF, Ano, Rendimento, RA1, gap1,
         RA2, gap2, RA3, gap3, RA4, gap4) 


lacunas <- rbind(resultados_enf,
                 resultados_med)

writexl::write_xlsx(lacunas, 
                    "~/GitHub/materno_infantil/02_script_tratado/04_orcamentos/rendimentos_necessarios.xlsx")
