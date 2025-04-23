library(tidyverse)
library(arrow)
library(patchwork)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(scales)
library(ggspatial) 
library(sf)


resultados_regioes <- 
  read_csv("02_script/07_output_montecarlo/resultados_regioes.csv")

resultados_regioes |> 
  filter(uf_sigla == "AC") |>
  ggplot(aes(x = perc)) + geom_histogram() + theme_minimal() + 
  geom_vline(aes(xintercept = 100), 
             linetype = "dashed", color = "red", linewidth = 1) + 
  facet_wrap(~regiao_saude, 
             ncol = 1, nrow = 3)

resultados19 <- read_csv("~/GitHub/materno_infantil/02_script/07_output_montecarlo/resumo_regiao1419.csv") |> 
                filter(ano == 2019) |> 
                select(-`...1`)

estados_br <- read_state(year = 2020,
                         showProgress = FALSE)

spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 

spdf_fortified <- 
  sf::st_as_sf(spdf) |> 
  distinct()

# Definir limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude

baseline <- 
  resultados19 |> 
  mutate(cod_regsaude = as.numeric(cod_regsaude)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct() |> 
  mutate(media_percentual_ajustado = if_else(media_percentual > 100, 
                                             100, media_percentual)) |> 
  mutate(regiao = case_when(uf_sigla %in% c("AM",
                                           "RO",
                                           "RR",
                                           "TO",
                                           "PA",
                                           "AC",
                                           "AP") ~ "Norte", 
                            uf_sigla %in% c("BA",
                                           "PE",
                                           "PI",
                                           "SE",
                                           "MA",
                                           "RN",
                                           "AL",
                                           "CE",
                                           "PB") ~ "Nordeste",
                            uf_sigla %in% c("GO",
                                           "DF",
                                           "MS",
                                           "MT") ~ "Centro Oeste",
                            uf_sigla %in% c("RS",
                                           "PR",
                                           "SC") ~ "Sul", 
                            uf_sigla %in% c("MG",
                                           "SP",
                                           "RJ",
                                           "ES") ~ "Sudeste"), .after = uf_sigla)

ggplot() +
  geom_sf(data = baseline, 
          aes(fill = media_percentual_ajustado, 
              geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) + 
  scale_fill_gradientn(colors = c("#D92B3A", 
                                  "#d4e302",
                                  "#02592e"), 
                       values = 
                         rescale(c(0, 50,100)), 
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100)) + 
  theme_minimal() +
  labs(fill = "Tendências no número de nascidos") +
  annotation_north_arrow(location = "tr",  
                         which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme(
    legend.justification = "center",
    legend.box = "horizontal",
    legend.position = "bottom",
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

baseline |> 
  ggplot(aes(x = media_percentual,
             y = fct_reorder(uf_sigla, 
                             media_percentual),
             fill = regiao)) + 
  geom_boxplot() + 
  theme_minimal() + 
  xlab("Percentual") + 
  ylab("UF") + xlim(0, 200)


# cenarios ----------------------------------------------------------------

c1 <- resultados_regioes |> 
          filter(simulacao == 5156)   


c2 <- resultados_regioes |> 
          filter(simulacao == 3026)


c3 <- resultados_regioes |> 
  filter(simulacao == 6407)


c4 <- resultados_regioes |> 
        filter(simulacao == 3866)

c5 <- resultados_regioes |> 
        filter(simulacao == 7405)


# funcao para gerar mapa --------------------------------------------------

gerar_mapa <- function(base){
  
  mapa <- 
    base |> 
    mutate(cod_regsaude = as.numeric(cod_regsaude)) |> 
    left_join(spdf_fortified,
              by = c("cod_regsaude"="reg_id")) |> 
    distinct() |> 
    mutate(media_percentual_ajustado = if_else(perc > 100, 
                                               100, perc)) |> 
    mutate(regiao = case_when(uf_sigla %in% c("AM",
                                              "RO",
                                              "RR",
                                              "TO",
                                              "PA",
                                              "AC",
                                              "AP") ~ "Norte", 
                              uf_sigla %in% c("BA",
                                              "PE",
                                              "PI",
                                              "SE",
                                              "MA",
                                              "RN",
                                              "AL",
                                              "CE",
                                              "PB") ~ "Nordeste",
                              uf_sigla %in% c("GO",
                                              "DF",
                                              "MS",
                                              "MT") ~ "Centro Oeste",
                              uf_sigla %in% c("RS",
                                              "PR",
                                              "SC") ~ "Sul", 
                              uf_sigla %in% c("MG",
                                              "SP",
                                              "RJ",
                                              "ES") ~ "Sudeste"), .after = uf_sigla)
  

  ggplot() +
    geom_sf(data = mapa, 
            aes(fill = media_percentual_ajustado, 
                geometry = geometry), 
            color = "#f5f5f5") +
    geom_sf(data = estados_br, 
            fill = NA, 
            color = "#4c4d4a", 
            size = 0.1) + 
    scale_fill_gradientn(colors = c("#D92B3A", 
                                    "#d4e302",
                                    "#02592e"), 
                         values = 
                           rescale(c(0, 50,100)), 
                         limits = c(0, 100),
                         breaks = c(0, 25, 50, 75, 100)) + 
    theme_minimal() +
    labs(fill = "Tendências no número de nascidos") +
    annotation_north_arrow(location = "tr",  
                           which_north = "true",
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme(
      legend.justification = "center",
      legend.box = "horizontal",
      legend.position = "bottom",
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

gerar_mapa(c1) + ggtitle("Cenário 1")
gerar_mapa(c2) + ggtitle("Cenário 2")
gerar_mapa(c3) + ggtitle("Cenário 3")
gerar_mapa(c4) + ggtitle("Cenário 4")
gerar_mapa(c5) + ggtitle("Cenário 5")

