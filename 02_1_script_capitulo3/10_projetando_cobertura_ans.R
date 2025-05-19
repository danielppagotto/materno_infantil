library(tidyverse)

cobertura <- 
  vroom::vroom("~/GitHub/materno_infantil/01_dados/cobertura_ans.csv") |> 
  select(-`...1`) |> 
  mutate(cod_regsaud = 
           as.character(cod_regsaud)) |> 
  janitor::clean_names() |> 
  select(uf_sigla, cod_regsaud, 
         regiao_saude, cobertura) |> 
  mutate(ano = 2023) |> 
  mutate(cenario = "cenario real")

anos_projecao <- 2024:2030

# Função para calcular taxas de crescimento/decrescimento anuais
calcular_taxa_anual <- function(taxa_total, n_anos) {
  # Fórmula para taxa composta: (1 + taxa)^n_anos = 1 + taxa_total
  return((1 + taxa_total)^(1/n_anos) - 1)
}

# Taxa anual para aumento de 10% em 7 anos
taxa_aumento <- calcular_taxa_anual(0.10, 7)

# Taxa anual para diminuição de 10% em 7 anos
taxa_diminuicao <- calcular_taxa_anual(-0.10, 7)

# Criar dataframe vazio para armazenar todas as projeções
# Usamos cobertura como base em vez de dados
projecoes_completas <- cobertura

# Para cada região de saúde, criar os três cenários de projeção
for (i in 1:nrow(cobertura)) {
  cobertura_base <- cobertura$cobertura[i]
  regiao <- cobertura$regiao_saude[i]
  uf <- cobertura$uf_sigla[i]
  cod <- cobertura$cod_regsaud[i]
  
  # Para cada ano de projeção
  for (ano in anos_projecao) {
    anos_passados <- ano - 2023
    
    # Cenário 1: Aumento gradual de 10% até 2030
    cobertura_cenario1 <- cobertura_base * (1 + taxa_aumento)^anos_passados
    
    # Cenário 2: Manter constante
    cobertura_cenario2 <- cobertura_base
    
    # Cenário 3: Diminuição gradual de 10% até 2030
    cobertura_cenario3 <- cobertura_base * (1 + taxa_diminuicao)^anos_passados
    
    # Adicionar ao dataframe de projeções
    projecoes_completas <- rbind(projecoes_completas, 
                                 data.frame(
                                   uf_sigla = uf,
                                   cod_regsaud = cod,
                                   regiao_saude = regiao,
                                   cobertura = cobertura_cenario1,
                                   ano = ano,
                                   cenario = "aumento"
                                 ))
    
    projecoes_completas <- rbind(projecoes_completas, 
                                 data.frame(
                                   uf_sigla = uf,
                                   cod_regsaud = cod,
                                   regiao_saude = regiao,
                                   cobertura = cobertura_cenario2,
                                   ano = ano,
                                   cenario = "constante"
                                 ))
    
    projecoes_completas <- rbind(projecoes_completas, 
                                 data.frame(
                                   uf_sigla = uf,
                                   cod_regsaud = cod,
                                   regiao_saude = regiao,
                                   cobertura = cobertura_cenario3,
                                   ano = ano,
                                   cenario = "diminuicao"
                                 ))
  }
}

# Adicionar cenário para os dados originais
projecoes_completas$cenario[is.na(projecoes_completas$cenario)] <- "Base"

# Arredondar os valores de cobertura para 2 casas decimais
projecoes_completas$cobertura <- round(projecoes_completas$cobertura, 2)

# Visualizar os primeiros resultados
head(projecoes_completas, 10)


write.csv(projecoes_completas,
          "~/GitHub/materno_infantil/02_1_script_capitulo3/10_cenarios_ans/cenarios_ans_projetado.csv")
