
library(tidyverse)
library(vroom)
library(patchwork)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(scales)
library(ggspatial) 
library(sf)
library(readxl)
library(leaflet)

resultados_regioes <- 
  read_csv("~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_23_08_05.csv") |> 
  mutate(regiao = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado"
  ))


# mapa resumo --------------------------------------------------------------------

spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 

estados_br <- read_state(year = 2020,
                         showProgress = FALSE)

spdf_fortified <- 
  sf::st_as_sf(spdf)

# Definir limites de longitude e latitude para focar no Brasil

limite_long <- c(-75,-28)  

limite_lat <- c(-33, 4)     

# funcao 

gerar_mapa <- 
  
  function(df, var_perc, posicao, titulo){
    
    var_sym <- sym(var_perc)
    
    ggplot() +
      geom_sf(data = df, 
              aes(fill = !!var_sym, 
                  geometry = geometry), 
              color = "#f5f5f5") +
      geom_sf(data = estados_br, 
              fill = NA, 
              color = "#4c4d4a", 
              size = 0.1) +
      theme_minimal() +
      scale_fill_gradientn(colors = 
                             c("#FF2400","#FF7F00",  
                               "#c1c700","#7ac142",  
                               "#2c7719"),  
                           values = rescale(c(0, 50, 
                                              100)), 
                           limits = c(0, 100),
                           breaks = c(0, 50, 
                                      100)) +
      labs(fill = "RR(%)") +
      annotation_north_arrow(
        location = "tr", 
        which_north = "true",
        style = north_arrow_fancy_orienteering()) +
      annotation_scale(location = "bl", 
                       width_hint = 0.3) +
      ggtitle(titulo) +
      theme(
        legend.justification = "center",
        legend.box = "horizontal",
        legend.position = posicao,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 14),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1), 
        plot.margin = margin(10, 10, 10, 10))
  }


# cenarios ----------------------------------------------------------------

# cenario ruim 
# absenteísmo alto, todos os usuarios, baixa produtividade, baixo percentual 
# de atividades diretas

cenario1 <- 
  resultados_regioes |> 
  filter(simulacao == 3898) |> 
  mutate(perc = if_else(rr_med > 100, 100, rr_med)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()

mapa1 <- gerar_mapa(cenario1, 
           "perc",'none',"Cenário 1")

cenario1_agrupado <- cenario1 |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 1") 

cenario1_agrupado_uf <- 
  cenario1 |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(cenario = "Cenário 1") |> 
  mutate(categoria = "Médicos")


g1 <- cenario1_agrupado |> 
  ggplot(aes(x = fct_reorder(regiao, perc_medio), 
             y = perc_medio, fill = regiao)) + 
  geom_col() +
  geom_label(aes(label = perc_medio), 
                 fill = "white") +
  theme_minimal() + xlab("Região") + 
  ylab("Resultado relativo (%)") +
  theme(legend.position = "none")

# cenario 2 ---------------------------------------------------------------
# vamos deduzir os SUS dependente, mas com as demais condições mantidas

cenario2 <- resultados_regioes |> 
              filter(simulacao == 6298) |> 
              mutate(perc = if_else(rr_med > 100, 100, rr_med)) |> 
              left_join(spdf_fortified,
                        by = c("cod_regsaude"="reg_id")) |> 
              distinct()

mapa2 <- gerar_mapa(cenario2, 
           "perc", "none", "Cenário 2")

cenario2_agrupado <- cenario2 |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(cenario = "Cenário 2") |> 
  mutate(categoria = "Médicos")

cenario2_agrupado_uf <- 
  cenario2 |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(cenario = "Cenário 2") |> 
  mutate(categoria = "Médicos")


g2 <- cenario2_agrupado |> 
  ggplot(aes(x = fct_reorder(regiao, rr_med), 
             y = perc_medio, fill = regiao)) + 
  geom_col() +
  geom_label(aes(label = perc_medio), 
                 fill = "white") +
  theme_minimal() + xlab("Região") + 
  ylab("Resultado relativo (%)") +
  theme(legend.position = "none")


# cenario 3 ---------------------------------------------------------------
# vamos deduzir os SUS dependente e agora reduzindo o absenteísmo e o indireto

cenario3 <- 
  resultados_regioes |> 
  filter(simulacao == 6577) |> 
  mutate(perc = if_else(rr_med > 100, 100, rr_med)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()


mapa3 <- gerar_mapa(cenario3, 
                    "perc", "none",
                    "Cenário 3")

cenario3_agrupado <- cenario3 |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |>
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 3") |> 
  mutate(categoria = "Médicos")


cenario3_agrupado_uf <- cenario3 |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |>
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 3") |> 
  mutate(categoria = "Médicos")


g3 <- cenario3_agrupado |> 
  ggplot(aes(x = fct_reorder(regiao, perc_medio), 
             y = perc_medio, fill = regiao)) + 
  geom_col() +
  geom_label(aes(label = perc_medio), 
             fill = "white") +
  theme_minimal() + xlab("Região") + 
  ylab("Resultado relativo (%)") +
  theme(legend.position = "none")


# cenário 4 - melhora na produtividade  -----------------------------------

cenario4 <- 
  resultados_regioes |> 
  filter(simulacao == 5838) |> 
  mutate(perc = if_else(rr_med > 100, 100, rr_med)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()

mapa4 <- gerar_mapa(cenario4, 
                    "perc","right","Cenário 4")

cenario4_agrupado <- cenario4 |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(cenario = "Cenário 4") |> 
  mutate(categoria = "Médicos")

cenario4_agrupado_uf <- cenario4 |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_med),
            oferta = sum(oferta_med)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(cenario = "Cenário 4") |> 
  mutate(categoria = "Médicos")


g4 <- cenario4_agrupado |> 
  ggplot(aes(x = fct_reorder(regiao, perc_medio), 
             y = perc_medio, fill = regiao)) + 
  geom_col() +
  geom_label(aes(label = perc_medio), 
             fill = "white") +
  theme_minimal() + xlab("Região") + 
  ylab("Resultado relativo (%)") +
  theme(legend.position = "none")


# plotando gráficos -------------------------------------------------------

cenarios_agrupados_prep <- 
  cenarios_agrupados |> 
  rename(Região = regiao) |> 
  mutate(perc_medio_arredondado = round(perc_medio, 2))

# Defina uma variável global para o dodge
dodge_width <- 0.9
posicao <- position_dodge(width = dodge_width)

# Crie o gráfico
graficos_med <- 
  ggplot(cenarios_agrupados_prep, 
       aes(x = cenario, 
           y = perc_medio, 
           fill = Região, group = Região)) + 
  geom_col(position = posicao) + 
  geom_text(aes(label = perc_medio_arredondado, 
                y = perc_medio + 4),
            position = posicao,
            size = 3) +
  theme_minimal() +
  ylim(0, 250) + 
  ylab("Resultado relativo (%)") + 
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")


figura_artigo <- (mapa1 | 
                  mapa2 | 
                  mapa3 | 
                  mapa4) / graficos_med


ggsave(figura_artigo,
      filename = "~/GitHub/materno_infantil/02_script/08_output_gráficos/figura_artigo_med.jpeg",
      dpi = 1000, width = 16, height = 8)


# Vamos fazer agora para a enfermagem -------------------------------------

cenario1_enf <- 
  resultados_regioes |> 
  filter(simulacao == 3898) |> 
  mutate(perc = if_else(rr_enf > 100, 100, rr_enf)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()

mapa1_enf <- gerar_mapa(cenario1_enf, 
                    "perc",'none',"Cenário 1")

cenario1_agrupado_enf_uf <- 
  cenario1_enf |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 1") |> 
  mutate(categoria = "Enfermeiros")

cenario1_agrupado_enf <- cenario1_enf |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 1") 


g1_enf <- cenario1_agrupado_enf |> 
  ggplot(aes(x = fct_reorder(regiao, perc_medio), 
             y = perc_medio, fill = regiao)) + 
  geom_col() +
  geom_label(aes(label = perc_medio), 
             fill = "white") +
  theme_minimal() + xlab("Região") + 
  ylab("Resultado relativo (%)") +
  theme(legend.position = "none")

# cenario 2 ---------------------------------------------------------------
# vamos deduzir os SUS dependente, mas com as demais condições mantidas

cenario2_enf <- resultados_regioes |> 
  filter(simulacao == 6298) |> 
  mutate(perc = if_else(rr_enf > 100, 100, rr_enf)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()

mapa2_enf <- gerar_mapa(cenario2_enf, 
                    "perc", "none", "Cenário 2")

cenario2_agrupado_enf <- cenario2_enf |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(cenario = "Cenário 2") |> 
  mutate(categoria = "Enfermeiros")

cenario2_agrupado_enf_uf <- 
  cenario2_enf |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 2") |> 
  mutate(categoria = "Enfermeiros")


g2_enf <- cenario2_agrupado_enf |> 
  ggplot(aes(x = fct_reorder(regiao, perc_medio), 
             y = perc_medio, fill = regiao)) + 
  geom_col() +
  geom_label(aes(label = perc_medio), 
             fill = "white") +
  theme_minimal() + xlab("Região") + 
  ylab("Resultado relativo (%)") +
  theme(legend.position = "none")


# cenario 3 ---------------------------------------------------------------
# vamos deduzir os SUS dependente e agora reduzindo o absenteísmo e o indireto

cenario3_enf <- 
  resultados_regioes |> 
  filter(simulacao == 6577) |> 
  mutate(perc = if_else(rr_enf > 100, 100, rr_enf)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()


mapa3_enf <- gerar_mapa(cenario3_enf, 
                    "perc", "none",
                    "Cenário 3")

cenario3_agrupado_enf <- cenario3_enf |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |>
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 3") |> 
  mutate(categoria = "Enfermeiros")

cenario3_agrupado_enf_uf <- 
  cenario3_enf |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 3") |> 
  mutate(categoria = "Enfermeiros")


g3_enf <- cenario3_agrupado_enf |> 
  ggplot(aes(x = fct_reorder(regiao, perc_medio), 
             y = perc_medio, fill = regiao)) + 
  geom_col() +
  geom_label(aes(label = perc_medio), 
             fill = "white") +
  theme_minimal() + xlab("Região") + 
  ylab("Resultado relativo (%)") +
  theme(legend.position = "none")


# cenário 4 - melhora na produtividade  -----------------------------------

cenario4_enf <- 
  resultados_regioes |> 
  filter(simulacao == 5838) |> 
  mutate(perc = if_else(rr_enf > 100, 100, rr_enf)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct()

mapa4_enf <- gerar_mapa(cenario4_enf, 
                    "perc","right","Cenário 4")

cenario4_agrupado_enf <- cenario4_enf |> 
  group_by(regiao) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(cenario = "Cenário 4") 

cenario4_agrupado_enf_uf <- 
  cenario4_enf |> 
  group_by(uf_sigla) |> 
  summarise(necessidade = sum(nec_enf),
            oferta = sum(oferta_enf)) |>
  mutate(perc = (oferta/necessidade) * 100) |> 
  mutate(absoluto = oferta - necessidade) |> 
  mutate(perc_medio = round(perc, 2)) |> 
  mutate(cenario = "Cenário 4") |> 
  mutate(categoria = "Enfermeiros")


cenarios_agrupados_enf <- rbind(cenario1_agrupado_enf,
                            cenario2_agrupado_enf,
                            cenario3_agrupado_enf,
                            cenario4_agrupado_enf)


cenarios_agrupados_prep_enf <- 
  cenarios_agrupados_enf |> 
  rename(Região = regiao) |> 
  mutate(perc_medio_arredondado = round(perc_medio, 2))

# Defina uma variável global para o dodge
dodge_width <- 0.9
posicao <- position_dodge(width = dodge_width)

# Crie o gráfico

graficos_enf <- 
  ggplot(cenarios_agrupados_prep_enf, 
         aes(x = cenario, 
             y = perc_medio, 
             fill = Região, group = Região)) + 
  geom_col(position = posicao) + 
  geom_text(aes(label = perc_medio_arredondado, 
                y = perc_medio + 4),
            position = posicao,
            size = 3) +
  theme_minimal() +
  ylim(0, 250) + 
  ylab("Resultado relativo (%)") + 
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")



figura_artigo_enf <- (mapa1_enf | 
                  mapa2_enf | 
                  mapa3_enf | 
                  mapa4_enf) / graficos_enf

ggsave(figura_artigo_enf,
       filename = "~/GitHub/materno_infantil/02_script/08_output_gráficos/figura_artigo2_2023_enf.jpeg",
       dpi = 1000, width = 16, height = 8)


# salvando cenarios 

cenarios_agrupados_uf <- 
  rbind(cenario1_agrupado_enf_uf,
      cenario2_agrupado_enf_uf,
      cenario3_agrupado_enf_uf,
      cenario4_agrupado_enf_uf,
      cenario1_agrupado_uf,
      cenario2_agrupado_uf,
      cenario3_agrupado_uf,
      cenario4_agrupado_uf)


write.csv(cenarios_agrupados_uf, 
          "~/GitHub/materno_infantil/02_script/08_output_gráficos/cenarios_uf_1105.csv")




############################
# cenarios usando medidas de posição --------------------------------------
# Experimentos apenas
##########################

# quartis <- 
#   resultados_regioes |> 
#     group_by(cod_regsaude, uf_sigla, 
#              regiao_saude) |> 
#     summarise(rr_enf_primeiro = quantile(rr_enf, 0.25),
#               rr_med_primeiro = quantile(rr_med, 0.25),
#               rr_enf_med = quantile(rr_enf, 0.50),
#               rr_med_med = quantile(rr_med, 0.50),
#               rr_enf_terceiro = quantile(rr_enf, 0.75),
#               rr_med_terceiro = quantile(rr_med, 0.75))
# 
# primeiro_quartil_enf <- 
#   resultados_regioes |> 
#     filter(simulacao == 749) |> 
#     mutate(cenario = "Cenário 1")
# 
# mediana_enf <- 
#   resultados_regioes |> 
#   filter(simulacao == 5774) |> 
#   mutate(cenario = "Cenário 2")
# 
# terceiro_quartil_enf <- 
#   resultados_regioes |> 
#   filter(simulacao == 9099) |> 
#   mutate(cenario = "Cenário 3")
# 
# 
# cenario_enf <- rbind(primeiro_quartil_enf,
#                      mediana_enf,
#                      terceiro_quartil_enf) 
#                  
# 
# caract_enf <- cenario_enf |> 
#   group_by(cenario, regiao, regiao_saude, uf_sigla) |> 
#   summarise(rr_enf = median(rr_enf),
#             acoes_educacionais = mean(acoes_educacionais),
#             consultas = mean(consultas),
#             absenteismo = mean(absenteismo),
#             indireta = mean(indireta),
#             alto_risco = mean(alto_risco),
#             participacao_medico = mean(participacao_medico),
#             todos = mean(todos),
#             acoes_altorisco = mean(acoes_altorisco),
#             consultas_altorisco = mean(consultas_altorisco)) 
# 
# # caracteristicas ---------------------------------------------------------
# 
# caract_primeiro_quartil <- 
#   primeiro_quartil |> 
#   group_by(ano) |> 
#   summarise(acoes_educacionais = mean(acoes_educacionais),
#             consultas = mean(consultas),
#             absenteismo = mean(absenteismo),
#             indireta = mean(indireta),
#             alto_risco = mean(alto_risco),
#             participacao_medico = mean(participacao_medico),
#             todos = mean(todos),
#             acoes_altorisco = mean(acoes_altorisco),
#             consultas_altorisco = mean(consultas_altorisco)) |> 
#   mutate(perfil = "primeiro quartil")
# 
# 
# caract_med <- 
#   mediana |> 
#   group_by(ano) |> 
#   summarise(acoes_educacionais = mean(acoes_educacionais),
#             consultas = mean(consultas),
#             absenteismo = mean(absenteismo),
#             indireta = mean(indireta),
#             alto_risco = mean(alto_risco),
#             participacao_medico = mean(participacao_medico),
#             todos = mean(todos),
#             acoes_altorisco = mean(acoes_altorisco),
#             consultas_altorisco = mean(consultas_altorisco)) |> 
#   mutate(perfil = "mediana")
# 
# 
# caract_terceiro_quartil <- 
#   terceiro_quartil |> 
#   group_by(ano) |> 
#   summarise(acoes_educacionais = mean(acoes_educacionais),
#             consultas = mean(consultas),
#             absenteismo = mean(absenteismo),
#             indireta = mean(indireta),
#             alto_risco = mean(alto_risco),
#             participacao_medico = mean(participacao_medico),
#             todos = mean(todos),
#             acoes_altorisco = mean(acoes_altorisco),
#             consultas_altorisco = mean(consultas_altorisco)) |> 
#   mutate(perfil = "terceiro quartil")
# 
# caract <- rbind(caract_med,
#                 caract_primeiro_quartil,
#                 caract_terceiro_quartil)
