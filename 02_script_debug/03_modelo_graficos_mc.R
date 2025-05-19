
library(tidyverse)

resumo_regiao <- 
  read_csv("~/GitHub/materno_infantil/02_script_debug/02_output_mc/resumo_resultados23_19_05.csv") |> 
  select(-`...1`)

resultados_regioes <- 
  read_csv("~/GitHub/materno_infantil/02_script_debug/02_output_mc/resultados_23_19_05.csv") |> 
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


modelo <- lm("rr_med ~ acoes_hab + prenatal_hab + absenteismo + alto_risco + indireta_med + 
             acoes_alto + prenatal_alto + coleta_exames + 
             visita + consulta_puerperal + consulta_cd + 
             enf_coleta_exames + enf_coleta_cito + enf_prenatal + 
             enf_imunizacao + enf_puerperal + enf_visita + 
             enf_cd + enf_acoes + todos + regiao", 
             data = resultados_regioes)

summary(modelo)


# Gráficos ----------------------------------------------------

resumo_regiao  |> 
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
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~Região)  + 
  theme_bw() +
  xlab("Enfermeiros - RR(%)") + 
  ylab("Médicos - RR(%)") +
  xlim(0,250) + ylim(0,250)


dist_uf <- 
  resumo_regiao |> 
  mutate(classificacao = case_when(
    media_rr_med >= 100 & 
    media_rr_enf >= 100 ~ "Ambos superávit",
    media_rr_med >= 100 &
    media_rr_enf < 100 ~ "Superávit em médicos e déficit de enfermeiros",  
    media_rr_enf >= 100 &
    media_rr_med < 100 ~ "Superávit em enfermeiros e déficit de médicos",        
    media_rr_med <= 100 & 
    media_rr_enf <= 100 ~ "Ambos déficit")) |> 
  group_by(uf_sigla, classificacao) |> 
  count()

dist_uf |> 
  ggplot(aes(x = uf_sigla, y = n, 
             fill = classificacao)) + 
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
  ylab("Balanceamento (%)")

mediana_br_enf <- median(resumo_regiao$mediana_rr_enf)

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
  ylab("Balanceamento (%) - Enfermagem")
