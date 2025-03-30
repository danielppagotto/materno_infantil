
library(tidyverse)
library(patchwork)
library(geojsonio)
library(geojsonsf)
library(geobr)
library(scales)
library(ggspatial) 
library(sf)
library(readxl)
library(leaflet)


# carregando dados --------------------------------------------------------

tendencias <- read_excel("01_dados/classificacao_tendencias_final.xlsx")

estados_br <- read_state(year = 2020,
                         showProgress = FALSE)

spdf <- 
  geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
               what = "sp") 

spdf_fortified <- 
  sf::st_as_sf(spdf)

# Definir limites de longitude e latitude para focar no Brasil
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude

baseline <- 
  tendencias |> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(spdf_fortified,
              by = c("cod_regsaud"="reg_id")) |> 
  distinct()


# Mapa --------------------------------------------------------------------

a <- ggplot() +
  geom_sf(data = baseline, 
          aes(fill = tendencia, geometry = geometry), 
          color = "#f5f5f5") +
  geom_sf(data = estados_br, 
          fill = NA, 
          color = "#4c4d4a", 
          size = 0.1) +
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

ggsave(a, filename = "~/GitHub/materno_infantil/02_script/02_mapa_tendencias/mapa.jpeg",
       height = 8, width = 8, dpi = 500)
