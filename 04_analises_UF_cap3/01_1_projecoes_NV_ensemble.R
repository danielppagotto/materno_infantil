library(tidyverse)

regioes_saude <- read_csv("04_analises_UF_cap3/01_output_projecoes/hierarquia_atualizada.csv") |> 
  select(uf_sigla, cod_regsaud, regiao_saude) |> 
  distinct()

# as regioes que estavam soltando valores negativos
predicao_nascimentos_2 <- read_csv("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/predicao_nascimentos_2.csv") |> 
  mutate(tipo = case_when(competen < "2021-01-01" ~ "Treino",
                          competen >= "2021-01-01" &
                            competen <= "2023-12-01" ~ "Valor observado - teste",  # Mudei "Teste" para "Valor observado - teste"
                          competen > "2023-12-01" ~ "Previsão")) |> 
  mutate(qtd = case_when(tipo == "Treino" ~ valor_observado,
                         tipo == "Valor observado - teste" ~ valor_observado,
                         tipo == "Previsão" ~ valor_predito)) |>
  mutate(qtd_real_teste = if_else(tipo == "Valor observado - teste",  # Mudei "Teste" para "Valor observado - teste"
                                  valor_predito,
                                  NA)) |> 
  select(-`...1`) |> 
  left_join(regioes_saude, 
            by = c("cod_regsaud"))



predicao_nascimentos <- 
  read_csv("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/predicao_nascimentos.csv") |> 
  mutate(tipo = case_when(competen < "2021-01-01" ~ "Treino",
                          competen >= "2021-01-01" &
                            competen <= "2023-12-01" ~ "Valor observado - teste",  # Mudei "Teste" para "Valor observado - teste"
                          competen > "2023-12-01" ~ "Previsão")) |> 
  mutate(qtd = case_when(tipo == "Treino" ~ valor_observado,
                         tipo == "Valor observado - teste" ~ valor_observado,
                         tipo == "Previsão" ~ valor_predito)) |>
  mutate(qtd_real_teste = if_else(tipo == "Valor observado - teste",  # Mudei "Teste" para "Valor observado - teste"
                                  valor_predito,
                                  NA)) |> 
  select(-`...1`) |> 
  left_join(regioes_saude, 
            by = c("cod_regsaud")) |> 
  filter(cod_regsaud != 31038) |> 
  filter(cod_regsaud != 31053) |> 
  filter(cod_regsaud != 23014)

previsao <- rbind(predicao_nascimentos,
                  predicao_nascimentos_2) |> 
  filter(competen >= "2010-01-01") |> 
  select(uf_sigla, competen, cod_regsaud, regiao_saude, 
         qtd, tipo)

teste <- previsao |> 
            group_by(cod_regsaud) |> 
            count()

write.csv(previsao,
          "~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/modelos_ensemble.csv")


# visualização ------------------------------------------------------------


regiao <- 
  predicao_nascimentos |> 
  filter(cod_regsaud == 42004)  

# a linha roxa é o previsto
a <- regiao |> 
  ggplot(aes(x = competen, y = qtd, col = tipo)) +
  geom_line() + 
  geom_line(aes(x = competen,
                y = qtd_real_teste,
                color = "Valor previsto - teste")) + 
  scale_color_manual(values = c("Previsão" = "red", 
                                "Valor observado - teste" = "green", 
                                "Treino" = "blue",
                                "Valor previsto - teste" = "purple")) +
  xlab("Ano") + 
  ylab("Total de NV") + 
  ggtitle("Previsão de nascidos vivos",
          paste0("Região ", regiao$regiao_saude,
                 " - ", regiao$uf_sigla)) + theme_bw()


# criando uma funcao  -----------------------------------------------------

grafico_funcao <- function(cod_reg){
  
  
  regiao <- 
    predicao_nascimentos |> 
    filter(cod_regsaud == cod_reg)  
  
  nome <- regiao$regiao_saude
  uf <- regiao$uf_sigla
  max_uf <- max(regiao$qtd)
  
  # a linha roxa é o previsto
  a <- regiao |> 
    ggplot(aes(x = competen, y = qtd, col = tipo)) +
    geom_line() + 
    geom_smooth(se = FALSE,
                method = "loess",
                col = "black") +
    geom_line(aes(x = competen,
                  y = qtd_real_teste,
                  color = "Valor previsto - teste")) + 
    scale_color_manual(values = c("Previsão" = "red", 
                                  "Valor observado - teste" = "green", 
                                  "Treino" = "blue",
                                  "Valor previsto - teste" = "purple")) +
    xlab("Ano") + 
    ylab("Total de NV") + 
    ggtitle("Previsão de nascidos vivos",
            paste0("Região ", nome,
                   " - ", uf)) + 
    ylim(0, max_uf * 1.10) + theme_bw()
  
  ggsave(plot = a, 
         filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecao_ensemble/projecao_ensemble_",
                           cod_reg,".jpeg"),
         dpi = 500, 
         width = 12, height = 4)
    
  graf_ano <- regiao |>
                mutate(ano = year(competen)) |> 
                group_by(ano,
                         cod_regsaud,
                         uf_sigla,
                         regiao_saude) |> 
                summarise(qtd = sum(qtd))
  
  max_ano <- max(graf_ano$qtd)
  
  b <- graf_ano |> 
    ggplot(aes(x = ano, 
               y = qtd)) +
    geom_smooth(se = FALSE,
                method = "loess") +
    geom_point() +
    geom_vline(xintercept = 2023,
               linetype = "dashed",
               col = "red") +
    xlab("Ano") + 
    ylab("Total de NV") + 
    ggtitle("Previsão de nascidos vivos",
            paste0("Região ", nome,
                   " - ", uf)) + 
    ylim(0, max_ano * 1.10) + theme_bw()
  
  ggsave(plot = b, 
         filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecao_anual_ensemble/projecao_anual_ensemble_",
                           cod_reg,".jpeg"),
         dpi = 500, 
         width = 12, height = 4)

}

grafico_funcao(42004)

codigos_regiao <- unique(predicao_nascimentos$cod_regsaud)

for(cod in codigos_regiao) {
  tryCatch({
    grafico_funcao(cod)
  }, error = function(e) {
    warning(paste("Erro ao processar região", cod, ":", e$message))
  })
}



