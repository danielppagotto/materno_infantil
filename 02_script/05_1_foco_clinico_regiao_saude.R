library(readxl)

percentual_proced_2019 <- 
  read_excel("~/GitHub/materno_infantil/01_dados/percentual_proced_2019.xlsx") |> 
  mutate(Ibge = as.numeric(Ibge))

territorial <- read_csv("~/GitHub/materno_infantil/01_dados/territorial.csv")

fc <- territorial |> 
  left_join(percentual_proced_2019, 
            by = c("cod_municipio"="Ibge")) |> 
  group_by(uf_sigla, cod_regsaud, regiao_saude) |> 
  summarise(total = sum(Atend_Individual_Todos_proced,
                        na.rm = TRUE),
            total_materno = sum(Ated_Individual,
                                na.rm = TRUE)) |> 
  mutate(perc_fc = total_materno/total)

write.csv(fc, 
          "~/GitHub/materno_infantil/01_dados/foco_clinico.csv")
