
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

resultados_regioes <- 
  read_csv("~/GitHub/materno_infantil/02_script_tratado/02_output_mc/resultados_1000.csv") |> 
  select(-`...1`)

resumo_regiao <- 
  read_csv("~/GitHub/materno_infantil/02_script_tratado/02_output_mc/resumo_resultados_1000.csv") |> 
  select(-`...1`)


# Modelo Sensibilidade ----------------------------------------------------

resultados_regioes <- resultados_regioes |> 
                        mutate(alto_risco = 100 * alto_risco, 
                               indireta_med = 100 * indireta_med,
                               enf_coleta_cito = 100 * enf_coleta_cito,
                               enf_coleta_exames = 100 * enf_coleta_exames,
                               enf_prenatal = 100 * enf_prenatal,
                               enf_imunizacao = 100 * enf_imunizacao,
                               enf_puerperal = 100 * enf_puerperal,
                               enf_visita = 100 * enf_visita, 
                               enf_cd = 100 * enf_cd, 
                               enf_acoes = 100 * enf_acoes)




modelo <- lm("rr_enf ~ acoes_hab + prenatal_hab + absenteismo + alto_risco + indireta_med + 
             acoes_alto + prenatal_alto + coleta_exames + 
             visita + consulta_puerperal + consulta_cd + 
             enf_coleta_exames + enf_coleta_cito + enf_prenatal + 
             enf_imunizacao + enf_puerperal + enf_visita + 
             enf_cd + enf_acoes + todos + regiao", 
             data = resultados_regioes)

summary(modelo)


# Gráficos ----------------------------------------------------

cor.test(resumo_regiao$media_rr_med,
         resumo_regiao$media_rr_enf)

corr_rr <- resumo_regiao  |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado")) |> 
  ggplot(aes(x = mediana_rr_enf, y = mediana_rr_med,
             col = Região)) + 
  geom_point() + 
  geom_hline(yintercept = 100, 
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 100, 
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = 'lm', se = FALSE,
              col = "blue") + 
  facet_wrap(~Região)  + 
  theme_bw() +
  annotate("text", x = 25, y = 200, label = "I", size = 3, fontface = "bold") +
  annotate("text", x = 180, y = 200, label = "II", size = 3, fontface = "bold") +
  annotate("text", x = 25, y = 80, label = "III", size = 3, fontface = "bold") +
  annotate("text", x = 180, y = 80, label = "IV", size = 3, fontface = "bold") +
  xlab("Enfermeiros - RR(%)") + 
  ylab("Médicos - RR(%)") +
  xlim(0,250) + ylim(0,250)

corr_rr

ggsave(plot = corr_rr,
       filename = "~/GitHub/materno_infantil/02_script_tratado/03_graficos/correlacao_rr.png",
       dpi = 800, 
       width = 8,
       height = 5)

dist_uf <- 
  resumo_regiao |> 
  mutate(Classificação = case_when(
    media_rr_med >= 100 & 
    media_rr_enf >= 100 ~ "Ambos superávit",
    media_rr_med >= 100 &
    media_rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
    media_rr_enf >= 100 &
    media_rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
    media_rr_med <= 100 & 
    media_rr_enf <= 100 ~ "Ambos déficit")) |> 
  group_by(uf_sigla, Classificação) |> 
  count()

dist_uf |> 
  ggplot(aes(x = uf_sigla, y = n, 
             fill = Classificação)) + 
  geom_col(position = "fill") +  coord_flip() + 
  xlab("UF") +
  theme_minimal() + 
  theme(legend.position = "bottom") 


# Gráficos distribuição de resultados ---------------------------------------------------

# Gráfico para apenas uma região

mediana <- 
  resultados_regioes |> 
  filter(cod_regsaud == 23003) |> 
  group_by(cod_regsaud) |> 
  summarise(mediana_med = median(rr_med),
            mediana_enf = median(rr_enf), 
            quart1_med = quantile(rr_med, 0.25),
            quart3_med = quantile(rr_med, 0.75),
            quart1_enf = quantile(rr_enf, 0.25),
            quart3_enf = quantile(rr_enf, 0.75))

mediana_med <- mediana$mediana_med
mediana_enf <- mediana$mediana_enf

med <- resultados_regioes |> 
  filter(cod_regsaud == 23003) |> 
  ggplot(aes(x = rr_med)) + 
  geom_histogram(fill = "darkblue",
                 col = "black") + 
  geom_vline(xintercept = mediana_med,
             linetype = "dashed",
             col = "red") +
  theme_bw() + xlab("Percentual (%)") + 
  ylab("Frequência")  + xlim(0, 200) + 
  ggtitle("Distribuição de Resultado Relativo - Médicos",
          "Região de Saúde: 3ª Região Maracanaú - CE")

enf <- resultados_regioes |> 
  filter(cod_regsaud == 23003) |> 
  ggplot(aes(x = rr_enf)) + 
  geom_histogram(fill = "darkgreen",
                 col = "black") + 
  geom_vline(xintercept = mediana_enf,
             linetype = "dashed",
             col = "red") +
  theme_bw() + xlab("Percentual (%)") + 
  ylab("Frequência")  + xlim(0, 200) + 
  ggtitle("Distribuição de Resultado Relativo - Enfermeiros",
          "Região de Saúde: 3ª Região Maracanaú - CE")

a <- med + enf
a

# Função

regioes <- unique(resultados_regioes$cod_regsaud)

distribuicao_resultados <- 
  
  function(reg){
    
    info_regiao <- 
      resultados_regioes |> 
      filter(cod_regsaud == reg) |> 
      select(regiao_saude, uf_sigla) |> 
      distinct()
    
    regiao_saude <- info_regiao$regiao_saude
    uf_sigla <- info_regiao$uf_sigla
    
    mediana <- 
      resultados_regioes |> 
      filter(cod_regsaud == reg) |> 
      group_by(cod_regsaud) |> 
      summarise(mediana_med = median(rr_med),
                mediana_enf = median(rr_enf))
    
    mediana_med <- mediana$mediana_med
    mediana_enf <- mediana$mediana_enf
    
    med <- 
      resultados_regioes |> 
      filter(cod_regsaud == reg) |> 
      ggplot(aes(x = rr_med)) + 
      geom_histogram(fill = "darkblue",
                     col = "black") + 
      geom_vline(xintercept = mediana_med,
                 linetype = "dashed",
                 col = "red") +
      xlim(0, 250) + 
      theme_bw() + xlab("Percentual (%)") + 
      ylab("Frequência")  +
      ggtitle("Distribuição de Resultado Relativo - Médicos",
              paste0("Região de Saúde: ", regiao_saude," - ", uf_sigla))
    
    enf <- 
      resultados_regioes |> 
      filter(cod_regsaud == reg) |>  
      ggplot(aes(x = rr_enf)) + 
      geom_histogram(fill = "darkgreen",
                     col = "black") + 
      geom_vline(xintercept = mediana_enf,
                 linetype = "dashed",
                 col = "red") +
      xlim(0, 250) + 
      theme_bw() + xlab("Percentual (%)") + 
      ylab("Frequência")  + 
      ggtitle("Distribuição de Resultado Relativo - Enfermeiros",
              paste0("Região de Saúde: ", regiao_saude," - ", uf_sigla))
    
    b <- med + enf
    
    ggsave(plot = b, 
           filename = paste0("~/GitHub/materno_infantil/02_script_debug/03_graficos/01_histograma/histograma_regiao_",
                             reg,".jpeg"),
           dpi = 500, 
           width = 12, 
           height = 5)
  }



for(reg in regioes){
  
  distribuicao_resultados(reg = reg)
  
  cat("Processando região:", reg, "\n")
  
  
}


# Gráfico boxplot ---------------------------------------------------------

mediana_br <- median(resumo_regiao$mediana_rr_med)

boxplot_med <- 
  resumo_regiao  |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado")) |> 
  ggplot(aes(x = mediana_rr_med,
             y = fct_reorder(uf_sigla, mediana_rr_med),
             fill = Região)) + 
  geom_boxplot() + 
  xlim(0, 250) +
  geom_vline(xintercept = mediana_br, 
             linetype = "dashed",
             color = "red") + 
  theme_minimal() + 
  xlab("UF") + 
  ylab("Balanceamento (%)") +
  theme(legend.position = "none") + 
  ggtitle("Distribuição de RR por UF","Médicos")

mediana_br_enf <- median(resumo_regiao$mediana_rr_enf)

boxplot_enf <- 
  resumo_regiao  |> 
  mutate(Região = case_when(
    uf_sigla %in% c("MG", "SP", "RJ", "ES") ~ "Sudeste", 
    uf_sigla %in% c("PR", "SC", "RS") ~ "Sul",
    uf_sigla %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO") ~ "Norte",
    uf_sigla %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
    uf_sigla %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
    TRUE ~ "Não classificado")) |> 
  ggplot(aes(x = mediana_rr_enf,
             y = fct_reorder(uf_sigla, mediana_rr_enf),
             fill = Região)) + 
  geom_boxplot() + 
  xlim(0, 250) +
  geom_vline(xintercept = mediana_br_enf, 
             linetype = "dashed",
             color = "red") + 
  theme_minimal() + 
  xlab("UF") + 
  ylab("") + 
  ggtitle("","Enfermeiros")

boxplot <- boxplot_med + boxplot_enf

ggsave(plot = boxplot,
       filename = "~/GitHub/materno_infantil/02_script_tratado/03_graficos/boxplot.png",
       dpi = 800, 
       width = 8,
       height = 5)


# mapas -------------------------------------------------------------------

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
  resumo_regiao |> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaud"="reg_id")) |> 
  distinct() |> 
  mutate(mediana_rr_med_recod = 
           if_else(mediana_rr_med > 150, 150,
                   mediana_rr_med), 
         mediana_rr_enf_recod = 
           if_else(mediana_rr_enf > 150, 150,
                   mediana_rr_enf))
  


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
      scale_fill_gradientn(colors = 
                             c("#FF2400","#FF7F00",  
                               "#c1c700","#7ac142",  
                               "#2c7719"),  
                           values = rescale(c(0, 50,
                                              100, 150)), 
                           limits = c(0, 150),
                           breaks = c(0, 50, 100, 150)) +
      labs(fill = "RR(%)") +
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


mapa_mediana_medicos <- 
  gerar_mapa(df = baseline, 
           var_perc = "mediana_rr_med_recod",
           titulo = "Médicos") + 
  ggtitle("RR(%) por regiões de saúde",
          "Médicos") + 
  theme(legend.position = "none")

mapa_mediana_enf <- 
  gerar_mapa(df = baseline, 
             var_perc = "mediana_rr_enf_recod",
             titulo = "Enfermeiros") + 
  ggtitle("RR(%) por regiões de saúde",
          "Enfermeiros")

mapa_mediana <- mapa_mediana_medicos + mapa_mediana_enf 

ggsave(plot = mapa_mediana,
       filename = "~/GitHub/materno_infantil/02_script_tratado/03_graficos/mapa_mediana.png",
       dpi = 800, 
       width = 10,
       height = 5)

# Separando cenários  -----------------------------------------------------

# cenario 1 - Simulação 721

cenario1 <- 
  resultados_regioes |>
    filter(simulacao == "721") |> 
    mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
    left_join(spdf_fortified,
            by = c("cod_regsaud"="reg_id")) |> 
    distinct() |> 
    mutate(mediana_rr_med_recod = 
             if_else(rr_med > 150, 150,
                     rr_med), 
           mediana_rr_enf_recod = 
             if_else(rr_enf > 150, 150,
                     rr_enf))

dist_uf1 <- 
  cenario1 |> 
  mutate(Classificação = case_when(
    rr_med >= 100 & 
      rr_enf >= 100 ~ "Ambos superávit",
    rr_med >= 100 &
      rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
    rr_enf >= 100 &
      rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
    rr_med <= 100 & 
      rr_enf <= 100 ~ "Ambos déficit")) |> 
  group_by(uf_sigla, Classificação) |> 
  count() |> 
  ggplot(aes(x = uf_sigla, y = n, 
             fill = Classificação)) + 
  geom_col(position = "fill") +  coord_flip() + 
  xlab("UF") +
  ylab(" ") +
  theme_minimal() + 
  theme(legend.position = "none") + 
  ggtitle("Cenário 1") + 
  theme(text = element_text(size = 16))


# mapas 1

cenario1_med <-  gerar_mapa(df = cenario1, 
                            var_perc = "mediana_rr_med_recod",
                            titulo = "Médicos") + 
                  ggtitle("RR(%) por regiões de saúde",
                      "Médicos - Cenário 1") +
                  theme(legend.position = "none")

  
cenario1_enf <- 
  gerar_mapa(df = cenario1, 
             var_perc = "mediana_rr_enf_recod",
             titulo = "Enfermeiros") + 
  ggtitle("RR(%) por regiões de saúde",
          "Enfermeiros - Cenário 1") + 
  theme(legend.position = "none")


# Cenário 2 ---------------------------------------------------------------

cenario2 <- 
  resultados_regioes |>
  filter(simulacao == "53") |> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaud"="reg_id")) |> 
  distinct() |> 
  mutate(mediana_rr_med_recod = 
           if_else(rr_med > 150, 150,
                   rr_med), 
         mediana_rr_enf_recod = 
           if_else(rr_enf > 150, 150,
                   rr_enf))


dist_uf2 <- 
  cenario2 |> 
  mutate(Classificação = case_when(
    rr_med >= 100 & 
      rr_enf >= 100 ~ "Ambos superávit",
    rr_med >= 100 &
      rr_enf < 100 ~ "Superávit em médicos e\ndeficit de enfermeiros",  
    rr_enf >= 100 &
      rr_med < 100 ~ "Superávit em enfermeiros e\ndeficit de médicos",        
    rr_med <= 100 & 
      rr_enf <= 100 ~ "Ambos déficit")) |> 
  group_by(uf_sigla, Classificação) |> 
  count() |> 
  ggplot(aes(x = uf_sigla, y = n, 
             fill = Classificação)) + 
  geom_col(position = "fill") +  coord_flip() + 
  xlab(" ") +
  ylab(" ") +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  ggtitle("Cenário 2") + 
  theme(text = element_text(size = 16))

cenario2_med <-  gerar_mapa(df = cenario2, 
                            var_perc = "mediana_rr_med_recod",
                            titulo = "Médicos") + 
  ggtitle("",
          "Médicos - Cenário 2") +
  theme(legend.position = "none")


cenario2_enf <- 
  gerar_mapa(df = cenario2, 
             var_perc = "mediana_rr_enf_recod",
             titulo = "Enfermeiros") + 
  ggtitle("",
          "Enfermeiros - Cenário 2") + 
  theme(legend.position = "none")


# cenário 3 ---------------------------------------------------------------

cenario3 <- 
  resultados_regioes |>
  filter(simulacao == "842") |> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaud"="reg_id")) |> 
  distinct() |> 
  mutate(mediana_rr_med_recod = 
           if_else(rr_med > 150, 150,
                   rr_med), 
         mediana_rr_enf_recod = 
           if_else(rr_enf > 150, 150,
                   rr_enf))


dist_uf3 <- 
  cenario3 |> 
  mutate(Classificação = case_when(
    rr_med >= 100 & 
      rr_enf >= 100 ~ "Ambos superávit",
    rr_med >= 100 &
      rr_enf < 100 ~ "Superávit em médicos e\ndeficit de enfermeiros",  
    rr_enf >= 100 &
      rr_med < 100 ~ "Superávit em enfermeiros e\ndeficit de médicos",        
    rr_med <= 100 & 
      rr_enf <= 100 ~ "Ambos déficit")) |> 
  group_by(uf_sigla, Classificação) |> 
  count() |> 
  ggplot(aes(x = uf_sigla, y = n, 
             fill = Classificação)) + 
  geom_col(position = "fill") +  
  coord_flip() + 
  xlab(" ") +
  ylab("") +
  theme_minimal() + 
  theme(legend.position = "none") + 
  guides(fill = guide_legend(nrow = 2)) +
  ggtitle("Cenário 3") +
  theme(text = element_text(size = 16))




cenario3_med <-  gerar_mapa(df = cenario3, 
                            var_perc = "mediana_rr_med_recod",
                            titulo = "Médicos") + 
  ggtitle("",
          "Médicos - Cenário 3") +
  theme(legend.position = "none")


cenario3_enf <- 
  gerar_mapa(df = cenario3, 
             var_perc = "mediana_rr_enf_recod",
             titulo = "Enfermeiros") + 
  ggtitle("",
          "Enfermeiros - Cenário 3")+ 
  theme(legend.position = "none")


# cenario 4 ---------------------------------------------------------------


cenario4 <- 
  resultados_regioes |>
  filter(simulacao == "933") |> 
  mutate(cod_regsaud = as.numeric(cod_regsaud)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaud"="reg_id")) |> 
  distinct() |> 
  mutate(mediana_rr_med_recod = 
           if_else(rr_med > 150, 150,
                   rr_med), 
         mediana_rr_enf_recod = 
           if_else(rr_enf > 150, 150,
                   rr_enf)) 

dist_uf4 <- 
  cenario4 |> 
  mutate(Classificação = case_when(
    rr_med >= 100 & 
      rr_enf >= 100 ~ "Ambos superávit",
    rr_med >= 100 &
      rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
    rr_enf >= 100 &
      rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
    rr_med <= 100 & 
      rr_enf <= 100 ~ "Ambos déficit")) |> 
  group_by(uf_sigla, Classificação) |> 
  count() |> 
  ggplot(aes(x = uf_sigla, y = n, 
             fill = Classificação)) + 
  geom_col(position = "fill") +  coord_flip() + 
  xlab(" ") + 
  ylab(" ") +
  theme_minimal() + 
  theme(legend.position = "none") + 
  ggtitle("Cenário 4") + 
  theme(text = element_text(size = 16)) 


cenario4_med <-  gerar_mapa(df = cenario4, 
                            var_perc = "mediana_rr_med_recod",
                            titulo = "Médicos") + 
  ggtitle("",
          "Médicos - Cenário 4") +
  theme(legend.position = "right")

cenario4_enf <- 
  gerar_mapa(df = cenario4, 
             var_perc = "mediana_rr_enf_recod",
             titulo = "Enfermeiros") + 
  ggtitle("",
          "Enfermeiros - Cenário 4")+ 
  theme(legend.position = "right")



mapas_med <- cenario1_med | cenario2_med | cenario3_med | cenario4_med

mapas_enf <- cenario1_enf | cenario2_enf | cenario3_enf | cenario4_enf

mapas <- mapas_med / mapas_enf

distribuicoes <- (dist_uf1 | dist_uf2 | dist_uf3 | dist_uf4)  

ggsave(plot = distribuicoes,
       filename = "~/GitHub/materno_infantil/02_script_tratado/03_graficos/distribuicoes.png",
       dpi = 800, 
       width = 15,
       height = 8)

ggsave(plot = mapas_med,
       filename = "~/GitHub/materno_infantil/02_script_tratado/03_graficos/mapas_cenarios_med.png",
       dpi = 800, 
       width = 15,
       height = 8)

ggsave(plot = mapas,
       filename = "~/GitHub/materno_infantil/02_script_tratado/03_graficos/mapas_cenarios.png",
       dpi = 800, 
       width = 15,
       height = 8)

ggsave(plot = mapas_enf,
       filename = "~/GitHub/materno_infantil/02_script_tratado/03_graficos/mapa_cenarios_enf.png",
       dpi = 800, 
       width = 15,
       height = 6)


# salvando dados para calcular os gaps financeiros ------------------------


ra_uf1 <- cenario1 |> 
  group_by(uf_sigla) |> 
  summarise(nec_med = sum(necessidade_media_med),
            nec_enf = sum(necessidade_media_enf),
            oferta_med = sum(oferta_media_med),
            oferta_enf = sum(oferta_media_enf)) |> 
  mutate(ra_med = oferta_med - nec_med,
         ra_enf = oferta_enf - nec_enf) |> 
  mutate(cenario = "Cenário 1") 

ra_uf2 <- cenario2 |> 
  group_by(uf_sigla) |> 
  summarise(nec_med = sum(necessidade_media_med),
            nec_enf = sum(necessidade_media_enf),
            oferta_med = sum(oferta_media_med),
            oferta_enf = sum(oferta_media_enf)) |> 
  mutate(ra_med = oferta_med - nec_med,
         ra_enf = oferta_enf - nec_enf) |> 
  mutate(cenario = "Cenário 2")  

ra_uf3 <- cenario3 |> 
  group_by(uf_sigla) |> 
  summarise(nec_med = sum(necessidade_media_med),
            nec_enf = sum(necessidade_media_enf),
            oferta_med = sum(oferta_media_med),
            oferta_enf = sum(oferta_media_enf)) |> 
  mutate(ra_med = oferta_med - nec_med,
         ra_enf = oferta_enf - nec_enf) |> 
  mutate(cenario = "Cenário 3") 

ra_uf4 <- cenario4 |> 
  group_by(uf_sigla) |> 
  summarise(nec_med = sum(necessidade_media_med),
            nec_enf = sum(necessidade_media_enf),
            oferta_med = sum(oferta_media_med),
            oferta_enf = sum(oferta_media_enf)) |> 
  mutate(ra_med = oferta_med - nec_med,
         ra_enf = oferta_enf - nec_enf) |> 
  mutate(cenario = "Cenário 4") 

ra_ufs <- rbind(ra_uf1,
                ra_uf2,
                ra_uf3,
                ra_uf4)

write.csv(ra_ufs,
          "~/GitHub/materno_infantil/02_script_tratado/resultados_absolutos_uf.csv")
