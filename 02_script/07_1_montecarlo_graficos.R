# Simulação de Monte Carlo otimizada para a função oferta_vs_demanda
library(tidyverse)
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

options(scipen = 999)

resultados_mc <- read_csv("~/GitHub/materno_infantil/02_script/07_output_montecarlo/resultados_23_08_05.csv") 
                        

modelo <- lm("rr_med ~ consultas + absenteismo + indireta + todos + alto_risco + 
                       acoes_altorisco + consultas_altorisco + participacao_medico +
                       acoes_educacionais + regiao", resultados_regioes)


summary(modelo)


# explorando resultados  --------------------------------------------------

mediana <- 
  resultados_mc |> 
  filter(cod_regsaude == 29022) |> 
  group_by(cod_regsaude) |> 
  summarise(mediana_med = median(rr_med),
            mediana_enf = median(rr_enf), 
            quart1_med = quantile(rr_med, 0.25),
            quart3_med = quantile(rr_med, 0.75),
            quart1_enf = quantile(rr_enf, 0.25),
            quart3_enf = quantile(rr_enf, 0.75))

mediana_med <- mediana$mediana_med
mediana_enf <- mediana$mediana_enf
  
med <- resultados_mc |> 
  filter(cod_regsaude == 29022) |> 
  ggplot(aes(x = rr_med)) + 
  geom_histogram(fill = "darkblue",
                 col = "black") + 
  geom_vline(xintercept = mediana_med,
             linetype = "dashed",
             col = "red") +
  theme_bw() + xlab("Percentual (%)") + 
  ylab("Frequência")  + xlim(0, 200) + 
  ggtitle("Distribuição de Resultado Relativo - Médicos",
          "Região de Saúde: Santo Antônio de Jesus - BA")

enf <- resultados_mc |> 
  filter(cod_regsaude == 29022) |> 
  ggplot(aes(x = rr_enf)) + 
  geom_histogram(fill = "darkgreen",
                 col = "black") + 
  geom_vline(xintercept = mediana_enf,
             linetype = "dashed",
             col = "red") +
  theme_bw() + xlab("Percentual (%)") + 
  ylab("Frequência")  + xlim(0, 200) + 
  ggtitle("Distribuição de Resultado Relativo - Enfermeiros",
          "Região de Saúde: Santo Antônio de Jesus - BA")

a <- med + enf

ggsave(plot = a, 
       filename = "~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/regiao12001.jpeg",
       dpi = 500, 
       width = 10, 
       height = 5)


# Automatizando uma função ------------------------------------------------

regioes <- unique(resultados_mc$cod_regsaude)



distribuicao_resultados <- 
  
  function(reg){
    
    info_regiao <- 
      resultados_mc |> 
      filter(cod_regsaude == reg) |> 
      select(regiao_saude, uf_sigla) |> 
      distinct()
    
    regiao_saude <- info_regiao$regiao_saude
    uf_sigla <- info_regiao$uf_sigla
    
    mediana <- 
      resultados_mc |> 
      filter(cod_regsaude == reg) |> 
      group_by(cod_regsaude) |> 
      summarise(mediana_med = median(rr_med),
                mediana_enf = median(rr_enf))
    
    mediana_med <- mediana$mediana_med
    mediana_enf <- mediana$mediana_enf
    
    med <- 
      resultados_mc |> 
        filter(cod_regsaude == reg) |> 
        ggplot(aes(x = rr_med)) + 
        geom_histogram(fill = "darkblue",
                       col = "black") + 
        geom_vline(xintercept = mediana_med,
                   linetype = "dashed",
                   col = "red") +
        xlim(0, 200) + 
        theme_bw() + xlab("Percentual (%)") + 
        ylab("Frequência")  +
        ggtitle("Distribuição de Resultado Relativo - Médicos",
                paste0("Região de Saúde: ", regiao_saude," - ", uf_sigla))
    
    enf <- 
      resultados_mc |> 
        filter(cod_regsaude == reg) |>  
        ggplot(aes(x = rr_enf)) + 
        geom_histogram(fill = "darkgreen",
                       col = "black") + 
        geom_vline(xintercept = mediana_enf,
                   linetype = "dashed",
                   col = "red") +
        xlim(0, 200) + 
        theme_bw() + xlab("Percentual (%)") + 
        ylab("Frequência")  + 
        ggtitle("Distribuição de Resultado Relativo - Enfermeiros",
              paste0("Região de Saúde: ", regiao_saude," - ", uf_sigla))
    
    b <- med + enf
    
    ggsave(plot = b, 
           filename = paste0("~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/histograma/grafico_regiao_",
                             reg,".jpeg"),
           dpi = 500, 
           width = 12, 
           height = 5)
  }



for(reg in regioes){
  
  distribuicao_resultados(reg = reg)
  
  cat("Processando região:", reg, "\n")
  
  
}


# mapas medianos ----------------------------------------------------------

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

mediano <- resultados_mc |> 
  group_by(regiao,
           uf_sigla, 
           regiao_saude,
           cod_regsaude) |> 
  summarise(mediana_enf = median(rr_enf),
            mediana_med = median(rr_med))


comp_enf_med <- mediano |> 
  rename(Região = regiao) |> 
  ggplot(aes(x = mediana_enf, 
             y = mediana_med,
             col = Região)) + 
  geom_point() +
  facet_wrap(~Região) + 
  geom_vline(xintercept = 100, 
             linetype = "dashed",
             col = "red") + 
  geom_hline(yintercept = 100,
             linetype = "dashed",
             col = "red") +
  geom_smooth(method = "lm", se = FALSE,
              col = "darkblue") +
  xlab("Mediana do RR (Enfermeiros)") +
  ylab("Mediana do RR (Médicos)") + 
  annotate("text", x = 25, y = 200, label = "I", size = 3, fontface = "bold") +
  annotate("text", x = 180, y = 200, label = "II", size = 3, fontface = "bold") +
  annotate("text", x = 25, y = 80, label = "III", size = 3, fontface = "bold") +
  annotate("text", x = 180, y = 80, label = "IV", size = 3, fontface = "bold") +
  theme_bw()

cor.test(mediano$mediana_enf,
    mediano$mediana_med)

ggsave(plot = comp_enf_med,
       filename = "~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/comp_enf_med.jpeg",
       dpi = 600, 
       width = 8, height = 4)


# mapas -------------------------------------------------------------------


baseline_med <- 
  mediano |>
  rename(perc = mediana_med) |> 
  mutate(perc_2 = if_else(perc > 100, 
                                 100, 
                          perc)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct() 


baseline_enf <- 
  mediano |>
  rename(perc = mediana_enf) |> 
  mutate(perc_2 = if_else(perc > 100, 100, perc)) |> 
  left_join(spdf_fortified,
            by = c("cod_regsaude"="reg_id")) |> 
  distinct() 


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


a <- gerar_mapa(baseline_med, 
                "perc_2","right") + ggtitle("RR (%) - Médicos")

b <- gerar_mapa(baseline_enf,
                "perc_2","right") + ggtitle("RR (%) - Enfermeiros")

c <- a + b

ggsave(plot = c,
       filename = "~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/mapa_mediano_enf_med.jpeg",
       dpi = 500, 
       width = 10, height = 5
       )

# Grafico UF --------------------------------------------------------------

d <- baseline_med |> 
  mutate(Região = regiao) |> 
  ggplot(aes(x = fct_reorder(uf_sigla, perc), 
             y = perc, fill = Região)) + 
  geom_boxplot() + coord_flip() +
  geom_hline(yintercept = mean(baseline_med$perc, na.rm = TRUE), 
             linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 1, 
           y = median(baseline_med$perc, na.rm = TRUE) + 0.02, 
           label = "Mediana Nacional", 
           hjust = -0.25, color = "red") +
  ylim(0, 200) +
  scale_fill_brewer(palette = "Set2", guide = guide_legend(nrow = 2)) +
  theme_minimal() + ylab("Percentual (%)") + 
  xlab("UF") +
  ggtitle("Distribuição de RR por UF",
          "Médicos") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))

e <- baseline_enf |> 
  mutate(Região = regiao) |> 
  ggplot(aes(x = fct_reorder(uf_sigla, perc), 
             y = perc, fill = Região)) + 
  geom_boxplot() + coord_flip() +
  geom_hline(yintercept = mean(baseline_enf$perc, na.rm = TRUE), 
             linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 1, 
           y = median(baseline_enf$perc, na.rm = TRUE) + 0.02, 
           label = "Mediana Nacional", 
           hjust = -0.25, color = "red") +
  ylim(0, 200) +
  scale_fill_brewer(palette = "Set2", guide = guide_legend(nrow = 2)) +
  theme_minimal() + ylab("Percentual (%)") + 
  xlab("UF") +
  ggtitle("Distribuição de RR por UF",
          "Enfermeiros") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))


f <- d | e

ggsave(plot = f,
       filename = "~/GitHub/materno_infantil/02_script/07_output_montecarlo/figuras/dist_enf_med.jpeg",
       dpi = 500, 
       width = 12, height = 7)

