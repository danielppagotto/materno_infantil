
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
library(sandwich)
library(lmtest)
library(modelsummary)
library(patchwork)

dados_simulados <- 
  vroom::vroom("~/GitHub/materno_infantil/02_script_tratado/02_output_mc/resultados_lp_500.csv") |> 
  select(-`...1`)


plotar_grafico <- function(reg){

dados <- dados_simulados |> 
            filter(cod_regsaud == reg)



dados$simulacao <- as.factor(dados$simulacao)
max_rr <- max(dados$rr_enf)
regiao <- unique(dados$regiao_saude)
uf <- unique(dados$uf_sigla)
cod <- unique(dados$cod_regsaud)

medianas <- dados |>
  group_by(ano) |>
  summarize(rr_enf_mediana = median(rr_enf))

percentis <- dados |>
  group_by(ano) |>
  summarize(
    p05 = quantile(rr_enf, 0.05),
    p25 = quantile(rr_enf, 0.25),
    p50 = median(rr_enf),
    p75 = quantile(rr_enf, 0.75),
    p95 = quantile(rr_enf, 0.95)
  )

percentis_long <- percentis |>
  pivot_longer(
    cols = c(p05, p25, p50, p75, p95),
    names_to = "percentil",
    values_to = "valor"
  )

cores_percentis <- c(
  "p05" = "darkblue",
  "p25" = "skyblue",
  "p50" = "red",
  "p75" = "skyblue",
  "p95" = "darkblue"
)

tamanhos_percentis <- c(
  "p05" = 0.7,
  "p25" = 0.9,
  "p50" = 1.5,
  "p75" = 0.9,
  "p95" = 0.7
)

grafico <- ggplot() +
  geom_line(data = dados |> 
              filter(simulacao %in% as.factor(sample(1:500, 
                                                     400))), 
            aes(x = ano, y = rr_enf, group = simulacao),
            color = "gray50", alpha = 0.1) +
  
  geom_line(data = percentis_long, 
            aes(x = ano, y = valor, group = percentil, color = percentil, size = percentil)) +
  
  geom_point(data = percentis, 
             aes(x = ano, y = p50), 
             color = "red", size = 3) +
  
  scale_color_manual(values = cores_percentis,
                     labels = c("5%", "25%", "Mediana (50%)", "75%", "95%"),
                     name = "Percentil") +
  scale_size_manual(values = tamanhos_percentis,
                    labels = c("5%", "25%", "Mediana (50%)", "75%", "95%"),
                    name = "Percentil") +
  
  # Configurações
  scale_x_continuous(breaks = 2025:2030) +
  labs(
    title = "Resultados Relativos para Enfermeiros",
    subtitle = paste0("Região de Saúde: ", 
                      regiao," - ", uf),
    x = "Ano",
    y = "RR (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  ylim(0, 1.50 * max_rr) +
  geom_text(data = medianas, 
            aes(x = ano, 
                y = rr_enf_mediana, 
                label = round(rr_enf_mediana, 1)),
            color = "black", fontface = "bold", size = 3,
            nudge_y = 7)
  
ggsave(plot = grafico,
       filename = paste0("~/GitHub/materno_infantil/02_script_tratado/07_figuras_mc_lp/mc_enfermeiros_",
                         cod,".jpeg"),
       dpi = 600, 
       height = 5,
       width = 10)

}

regioes_saude <- unique(dados_simulados$cod_regsaud)

for(reg in regioes_saude){
  
  plotar_grafico(reg)
  
}



# plotando mapas para cenários --------------------------------------------

dados_mapa <- dados_simulados |> 
  group_by(ano, cod_regsaud, regiao_saude, uf_sigla) |> 
  summarise(mediana_enf = median(rr_enf),
            mediana_med = median(rr_med)) |> 
  filter(ano == 2025 |
         ano == 2030) |> 
  mutate(status_enf = case_when(mediana_enf < 95 ~ "Déficit",
                            mediana_enf >= 95 &
                            mediana_enf <= 105 ~ "Equilíbrio",
                            mediana_enf > 105 ~ "Superávit")) |> 
  mutate(status_med = case_when(mediana_med < 95 ~ "Déficit",
                                mediana_med >= 95 &
                                mediana_med <= 105 ~ "Equilíbrio",
                                mediana_med > 105 ~ "Superávit"))



estados_br <- read_state(year = 2020, showProgress = FALSE)
         
spdf <- geojson_read("~/GitHub/saude_bucal/01_dados/shape_file_regioes_saude.json", 
                        what = "sp") 
         
spdf_fortified <- sf::st_as_sf(spdf)
         
limite_long <- c(-75, -28)  # limites de longitude
limite_lat <- c(-33, 4)     # limites de latitude
         
baseline25 <- dados_mapa |> 
              filter(ano == 2025) |> 
              mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
              left_join(spdf_fortified,
                     by = c("cod_regsaud"="reg_id")) |> 
              distinct() 
         
baseline30 <- dados_mapa |> 
  filter(ano == 2030) |> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaud"="reg_id")) |> 
  distinct() 
         
# Mapa --------------------------------------------------------------------
         
gerar_mapa <- 
  
  function(df, var_perc, titulo){
             
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
               annotation_north_arrow(
                 location = "tr", 
                 which_north = "true",
                 style = north_arrow_fancy_orienteering()) +
               annotation_scale(location = "bl", 
                                width_hint = 0.3) +
               theme(
                 legend.justification = "center",
                 legend.box = "horizontal",
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
         
         
enf_mapa <- gerar_mapa(df = baseline25, 
           var_perc = "status_enf",
           titulo = "Enfermeiros em 2025") + ggtitle("Enfermeiros - 2025") +
           theme(legend.position = "none")
         
         
enf_mapa30 <- gerar_mapa(df = baseline30, 
               var_perc = "status_enf",
               titulo = "Enfermeiros em 2030") + ggtitle("Enfermeiros - 2030") +
               labs(fill = "Status")

enf_mapa | enf_mapa30


med_mapa <- gerar_mapa(df = baseline25, 
                       var_perc = "status_med",
                       titulo = "Médicos em 2025") + ggtitle("Médicos - 2025") +
  theme(legend.position = "none")


med_mapa30 <- gerar_mapa(df = baseline30, 
                         var_perc = "status_med",
                         titulo = "Médicos em 2030") + ggtitle("Médicos - 2030") +
  labs(fill = "Status")


grafico_med <- 
  dados_mapa |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado")) |> 
  group_by(ano, Região, status_med) |> 
  count() |> 
  # Calcular percentuais para exibição nos rótulos
  group_by(ano, Região) |>
  mutate(perc = n/sum(n),
         # Formatar como percentual
         perc_label = scales::percent(perc, accuracy = 0.1)) |>
  ungroup() |>
  ggplot(aes(x = Região,
             y = n,
             fill = status_med)) + 
  geom_col(position = "fill") + 
  # Adicionar texto dentro das barras
  geom_text(aes(label = perc_label),
            position = position_fill(vjust = 0.5), # Centraliza verticalmente
            color = "white",                      # Cor do texto (branco geralmente é mais visível)
            fontface = "bold",                    # Texto em negrito para melhor visibilidade
            size = 3.5) +                         # Tamanho do texto (ajuste conforme necessário)
  facet_grid(~ano) +
  theme_minimal() + 
  xlab(" ") + 
  labs(fill = "Status", 
       y = "Proporção") + 
  ggtitle("") +
  # Melhorar a legibilidade do gráfico
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotacionar rótulos do eixo x para melhor visualização
    panel.grid.major.x = element_blank(),                # Remover linhas de grade verticais
    legend.position = "none"                          # Posicionar legenda abaixo
  ) 

a <- (med_mapa | med_mapa30) / grafico_med

ggsave(plot = a, 
       filename = "~/GitHub/materno_infantil/02_script_tratado/07_figuras_mapas_mclp/mapa_figura_med.jpeg",
       dpi = 500, 
       width = 12,
       height = 8)


grafico_enf <- 
  dados_mapa |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado")) |> 
  group_by(ano, Região, status_enf) |> 
  count() |> 
  # Calcular percentuais para exibição nos rótulos
  group_by(ano, Região) |>
  mutate(perc = n/sum(n),
         # Formatar como percentual
         perc_label = scales::percent(perc, accuracy = 0.1)) |>
  ungroup() |>
  ggplot(aes(x = Região,
             y = n,
             fill = status_enf)) + 
  geom_col(position = "fill") + 
  # Adicionar texto dentro das barras
  geom_text(aes(label = perc_label),
            position = position_fill(vjust = 0.5), # Centraliza verticalmente
            color = "white",                      # Cor do texto (branco geralmente é mais visível)
            fontface = "bold",                    # Texto em negrito para melhor visibilidade
            size = 3.5) +                         # Tamanho do texto (ajuste conforme necessário)
  facet_grid(~ano) +
  theme_minimal() + 
  xlab(" ") + 
  labs(fill = "Status", 
       y = "Proporção") + 
  ggtitle("") +
  # Melhorar a legibilidade do gráfico
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotacionar rótulos do eixo x para melhor visualização
    panel.grid.major.x = element_blank(),                # Remover linhas de grade verticais
    legend.position = "none"                          # Posicionar legenda abaixo
  ) 

b <- (enf_mapa | enf_mapa30) / grafico_enf

ggsave(plot = b, 
       filename = "~/GitHub/materno_infantil/02_script_tratado/07_figuras_mapas_mclp/mapa_figura_enf.jpeg",
       dpi = 500, 
       width = 12,
       height = 8)

cor_25 <- cor(baseline25$mediana_enf, baseline25$mediana_med) 
cor_30 <- cor(baseline30$mediana_enf, baseline30$mediana_med) 


# Primeiro, calcular correlações por ano
correlacoes <- dados_mapa %>%
  group_by(ano) %>%
  summarise(correlacao = cor(mediana_enf, mediana_med, use = "complete.obs"))

# Depois criar um data frame para as anotações
anotacoes <- data.frame(
  ano = unique(dados_mapa$ano),
  x = 180,
  y = 20
) %>%
  left_join(correlacoes, by = "ano")

# Agora o gráfico

x <- dados_mapa |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado")) |> 
  ggplot(aes(x = mediana_enf, 
             y = mediana_med)) + 
  geom_point(aes(col = Região)) + 
  geom_vline(xintercept = 100, 
             linetype = "dashed") +
  geom_hline(yintercept = 100, 
             linetype = "dashed") +
  geom_smooth(method = "lm",
              se = FALSE) + 
  facet_grid(~ano) + 
  theme_minimal() + 
  xlim(0, 200) + ylim(0, 200) + 
  xlab("RR (%) Mediana Enfermagem") + 
  ylab("RR (%) Mediana Medicina") +
  geom_text(data = anotacoes, 
            aes(x = x, y = y, 
                label = paste0("r = ", round(correlacao, 2))))
ggsave(plot = x, 
       filename = "~/GitHub/materno_infantil/02_script_tratado/07_figuras_mapas_mclp/correlacao_mc_lp.jpeg",
       dpi = 500, 
       width = 6,
       height = 4)
