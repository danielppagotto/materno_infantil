
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

modelos_ensemble <- 
  read_csv("~/GitHub/materno_infantil/02_script_tratado/05_nv/modelos_ensemble.csv") |> 
  filter(competen == "2025-02-01" |
         competen == "2030-02-01")

dados_2025 <- modelos_ensemble %>%
  filter(competen == "2025-02-01") %>%
  select(cod_regsaud, qtd_2025 = qtd)

dados_2030 <- modelos_ensemble %>%
  filter(competen == "2030-02-01") %>%
  select(cod_regsaud, qtd_2030 = qtd)

tendencia_regioes <- dados_2025 %>%
  inner_join(dados_2030, by = "cod_regsaud") %>%
  mutate(
    # Calculando a variação percentual
    variacao_percentual = ((qtd_2030 - qtd_2025) / qtd_2025) * 100,
    
    # Classificando conforme os critérios
    tendencia = case_when(
      variacao_percentual > 5 ~ "Crescimento",
      variacao_percentual < -5 ~ "Queda",
      TRUE ~ "Estacionário"
    )
  )

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
  tendencia_regioes |> 
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
  labs(fill = "Tendências") +
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
    plot.margin = margin(10, 10, 10, 10)) + 
  ggtitle("Tendência de de NV")

a

ggsave(a, filename = "~/GitHub/materno_infantil/02_script_tratado/05_nv/mapa_tendencia.jpeg",
       height = 8, width = 8, dpi = 500)
