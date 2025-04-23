library(tidyverse)
library(RODBC)
library(plm)
library(car)
library(lmtest)
library(sandwich)

resumo_regiao <- vroom::vroom("~/GitHub/materno_infantil/02_script/07_output_montecarlo/resumo_regiao1419.csv") |> 
                  select(-`...1`)

resumo_regiao$cod_regsaude <- as.character(resumo_regiao$cod_regsaude)

dremio_host <- Sys.getenv("endereco")
dremio_port <- Sys.getenv("port")
dremio_uid <- Sys.getenv("uid")
dremio_pwd <- Sys.getenv("datalake")


channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;
                                     HOST=%s;
                                     PORT=%s;
                                     UID=%s;
                                     PWD=%s;
                                     AUTHENTICATIONTYPE=Basic Authentication;
                                     CONNECTIONTYPE=Direct", 
                                     dremio_host, 
                                     dremio_port, 
                                     dremio_uid, 
                                     dremio_pwd))

query <- 
  'SELECT * FROM "@daniel"."taxa_mortalidade_materna"'

query_pib <- 
  'SELECT * FROM "@daniel"."pib_regioes_saude"
   WHERE ano IN (2014, 2015, 2016, 2017, 2018, 2019)'


pbf_query <- 
  'SELECT * FROM "@daniel"."PBF_regiao_saude"
   WHERE ano IN (2014, 2015, 2016, 2017, 2018, 2019)'

taxa_mortalidade <- sqlQuery(channel, 
                     query, 
                     as.is = TRUE)

pbf <- sqlQuery(channel, pbf_query, 
                as.is = TRUE) |> 
       mutate(valor_familia = valor_repasse/qtd_familias)

pbf$ano <- as.numeric(pbf$ano)

pib <- sqlQuery(channel, 
                query_pib, 
                as.is = TRUE) |> 
  select(ano, regiao, cod_regsaud, ppc)

pib$ano <- as.numeric(pib$ano)

taxa_mortalidade$ano <- as.numeric(taxa_mortalidade$ano)

taxa_mortalidade$total_nv <- 
  as.numeric(taxa_mortalidade$total_nv)

taxa_mortalidade$total_obitos_mat <- 
  as.numeric(taxa_mortalidade$total_obitos_mat)

taxa_mortalidade <- 
  taxa_mortalidade |> 
  mutate(mortalidade_materna = 
           (total_obitos_mat/total_nv) * 1000)

dados <- resumo_regiao |> 
  left_join(taxa_mortalidade, 
            by = c("cod_regsaude"="cod_regsaud",
                   "ano"="ano")) |>
  mutate(mortalidade_materna = 
           if_else(is.na(mortalidade_materna), 
                   0, 
                   mortalidade_materna)) |> 
  left_join(pib, 
            by = c("cod_regsaude"="cod_regsaud",
                   "ano"="ano")) |> 
  mutate(ppc_log = log(ppc)) |>
  left_join(pbf, 
            by = c("cod_regsaude"="cod_regsaud",
                   "ano"="ano")) |> 
  select(uf_sigla.x,
         regiao,
         cod_regsaude,
         regiao_saude.x,
         ano,
         media_percentual, 
         media_diferenca,
         mortalidade_materna,
         ppc,
         ppc_log,
         qtd_familias,
         valor_repasse,
         valor_familia) |> 
  rename(uf_sigla = uf_sigla.x,
         regiao_saude = regiao_saude.x)
  

pib2019 <- pib |> filter(ano == 2019)

junto <- resultado_resumo_por_regiao |> 
  mutate(cod_regsaude = as.character(cod_regsaude)) |> 
  left_join(pib2019, by = c("cod_regsaude"="cod_regsaud")) 

junto |> 
  ggplot(aes(x = ppc, y = media_percentual)) + geom_point(aes(col = uf_sigla)) + 
  geom_smooth(method = "lm") + theme_minimal() 

cor.test(junto$media_percentual,junto$ppc)

# rodando modelo ----------------------------------------------------------

# Instalação dos pacotes necessários (descomente se precisar instalar)
# install.packages("plm")
# install.packages("lmtest")
# install.packages("car")
# install.packages("sandwich")

# Carregando os pacotes
library(plm)
library(lmtest)
library(car)
library(sandwich)
library(effects)

# Supondo que seus dados estão em um arquivo CSV
# Substitua 'seu_arquivo.csv' pelo nome real do seu arquivo
# dados <- read.csv('seu_arquivo.csv')

# Se os dados já estiverem no ambiente R, você pode pular a linha acima
# e usar o dataframe que já existe

# Vamos assumir que seus dados estão no dataframe 'dados'
# e que você tem uma coluna de identificação para cada unidade (ex: município, estado, etc.)
# e uma coluna para o ano

# Se você não tem uma coluna de identificação, precisará criá-la
# Por exemplo, se cada linha representa um município:
# dados$id <- 1:nrow(dados)


if(!is.factor(dados$regiao)) {
  dados$regiao <- as.factor(dados$regiao)
}

# Preparando os dados para análise de painel
# Usando cod_regsaude como identificador e ano como tempo
painel <- pdata.frame(dados, index = c("cod_regsaude", "ano"))

# Estatísticas descritivas
summary(painel[, c("mortalidade_materna", "ppc", "valor_familia", "media_diferenca")])

# Modelo de efeitos fixos
modelo_fixo <- plm(mortalidade_materna ~ ppc + 
                   valor_familia + media_percentual + regiao, 
                   data = painel, 
                   model = "within")

summary(modelo_fixo)

# Modelo de efeitos aleatórios
modelo_aleatorio <- plm(mortalidade_materna ~ ppc + valor_familia + media_diferenca, 
                        data = painel, 
                        model = "random")

# Modelo pooled (pooling)
modelo_pooled <- plm(mortalidade_materna ~ ppc + valor_familia + media_diferenca, 
                     data = painel, 
                     model = "pooling")

# Teste de Hausman para escolher entre efeitos fixos e aleatórios
teste_hausman <- phtest(modelo_fixo, modelo_aleatorio)
print(teste_hausman)

# Teste F para escolher entre efeitos fixos e pooled
teste_f <- pFtest(modelo_fixo, modelo_pooled)
print(teste_f)

# Teste de multiplicador de Lagrange para escolher entre efeitos aleatórios e pooled
teste_lm <- plmtest(modelo_pooled, type = "bp")
print(teste_lm)

# Vamos imprimir o resumo do modelo mais adequado
# (aqui estamos supondo que é o de efeitos fixos, mas você deve seguir o resultado dos testes)
summary(modelo_fixo)

# Teste de heterocedasticidade
teste_hetero <- bptest(modelo_fixo)
print(teste_hetero)



# Se houver heterocedasticidade, podemos obter erros-padrão robustos
if(teste_hetero$p.value < 0.05){
  cat("\nModelo com erros-padrão robustos à heterocedasticidade:\n")
  robusto <- coeftest(modelo_fixo, vcov = vcovHC(modelo_fixo, type = "HC1"))
  print(robusto)
}


# Teste de correlação serial
teste_serial <- pbgtest(modelo_fixo)
print(teste_serial)

# Gráficos para diagnóstico
par(mfrow = c(2, 2))
plot(modelo_fixo)

# Interpretação dos resultados
cat("\nInterpretação dos resultados:\n")
cat("Coeficientes do modelo:\n")
print(coef(modelo_fixo))

# Criando variáveis com defasagem (lag) para análise de efeitos ao longo do tempo
painel_lag <- painel
painel_lag$ppc_lag <- lag(painel_lag$ppc)
painel_lag$valor_familia_lag <- lag(painel_lag$valor_familia)
painel_lag$media_diferenca_lag <- lag(painel_lag$media_diferenca)

# Modelo com variáveis defasadas para verificar efeitos não contemporâneos
modelo_lag <- plm(mortalidade_materna ~ ppc + ppc_lag + 
                    valor_familia + valor_familia_lag + 
                    media_diferenca + media_diferenca_lag, 
                  data = painel_lag, 
                  model = "within")
cat("\nModelo com variáveis defasadas (efeitos não contemporâneos):\n")
summary(modelo_lag)

# Teste de efeitos fixos por tempo
modelo_two_ways <- plm(mortalidade_materna ~ ppc + valor_familia + media_diferenca, 
                       data = painel, 
                       model = "within", 
                       effect = "twoways")
cat("\nModelo com efeitos fixos por indivíduo e tempo (two-ways):\n")
summary(modelo_two_ways)

# Teste para decidir entre efeitos fixos por indivíduo ou efeitos fixos por indivíduo e tempo
teste_efeitos_tempo <- pFtest(modelo_two_ways, modelo_fixo)
print(teste_efeitos_tempo)

# Como você já possui as colunas necessárias, não precisamos criar dados de exemplo
# Vamos apenas garantir que os dados estejam completos para a análise

# Verificando valores ausentes
na_check <- sapply(dados[, c("mortalidade_materna", "ppc", "valor_familia", "media_diferenca", 
                             "cod_regsaude", "ano")], function(x) sum(is.na(x)))
print("Valores ausentes por coluna:")
print(na_check)

# Removendo linhas com valores ausentes nas variáveis de interesse (se necessário)
dados_completos <- na.omit(dados[, c("mortalidade_materna", "ppc", "valor_familia", 
                                     "media_diferenca", "cod_regsaude", "ano")])

# Verificando a estrutura do painel
pdim(painel)

# Análise descritiva por ano
by(dados$mortalidade_materna, dados$ano, summary)
by(dados$ppc, dados$ano, summary)
by(dados$valor_familia, dados$ano, summary)
by(dados$media_diferenca, dados$ano, summary)

# Verificando a correlação entre as variáveis independentes
cor_matrix <- cor(dados[, c("ppc", "valor_familia", "media_diferenca")], 
                  use = "complete.obs")
print("Matriz de correlação:")
print(cor_matrix)

dados |> 
  filter(ano == 2019) |> 
  select(ppc, valor_familia, 
         mortalidade_materna, media_percentual) |> 
  cor()
    
# Teste de efeitos fixos por tempo
modelo_two_ways <- plm(mortalidade_materna ~ ppc + valor_familia + media_percentual, 
                       data = painel, 
                       model = "within", 
                       effect = "twoways")
cat("\nModelo com efeitos fixos por indivíduo e tempo (two-ways):\n")
summary(modelo_two_ways)

# Teste para decidir entre efeitos fixos por indivíduo ou efeitos fixos por indivíduo e tempo
teste_efeitos_tempo <- pFtest(modelo_two_ways, modelo_fixo)
print(teste_efeitos_tempo)

# Análise do efeito da variável regiao usando modelo entre-grupos (between)
modelo_between <- plm(mortalidade_materna ~ ppc_log + valor_familia + media_percentual + regiao, 
                      data = painel, 
                      model = "between")
cat("\nModelo entre-grupos (between) incluindo regiao:\n")
summary(modelo_between)

# Para analisar a interação entre regiao e as outras variáveis
# Criando um modelo pooled com interações
modelo_interacao <- plm(mortalidade_materna ~ ppc * regiao + valor_familia * regiao + 
                          media_percentual * regiao, 
                        data = painel, 
                        model = "pooling")
cat("\nModelo com interações entre variáveis e região:\n")
summary(modelo_interacao)

# Visualizando os efeitos da interação (se aplicável)
if("effects" %in% installed.packages()[,"Package"]) {
  # Efeito da interação entre ppc e regiao
  plot_ppc <- plot(effect("ppc:regiao", modelo_interacao))
  
  # Efeito da interação entre valor_familia e regiao
  plot_valor_familia <- plot(effect("valor_familia:regiao", modelo_interacao))
  
  # Efeito da interação entre media_percentual e regiao
  plot_media_percentual <- plot(effect("media_percentual:regiao", modelo_interacao))
}

# Análise adicional: modelo hierárquico/multinível
# Este pode ser útil quando há variáveis em diferentes níveis (como região)
if("lme4" %in% installed.packages()[,"Package"]) {
  library(lme4)
  
  # Preparando os dados em formato de data.frame padrão (não pdata.frame)
  dados_ml <- as.data.frame(dados)
  
  # Modelo multinível com intercepto aleatório por região
  modelo_ml1 <- lmer(mortalidade_materna ~ ppc + valor_familia + media_percentual + 
                       (1|regiao), data = dados_ml)
  
  cat("\nModelo hierárquico/multinível com intercepto aleatório por região:\n")
  print(summary(modelo_ml1))
  
  # Modelo multinível com intercepto e coeficientes aleatórios por região
  modelo_ml2 <- lmer(mortalidade_materna ~ ppc + valor_familia + media_percentual + 
                       (1 + ppc|regiao), data = dados_ml)
  
  cat("\nModelo hierárquico/multinível com intercepto e coeficiente de ppc aleatórios por região:\n")
  print(summary(modelo_ml2))
}




# pressupostos ------------------------------------------------------------

# 1. Testes de pressupostos para modelo between

# Carregando pacotes necessários
library(plm)
library(lmtest)
library(car)
library(nortest)
library(MASS)

# Assumindo que você já tem o modelo_between definido
# modelo_between <- plm(mortalidade_materna ~ ppc + valor_familia + media_percentual + regiao, 
#                      data = painel, model = "between")

# Extraindo resíduos do modelo
residuos <- residuals(modelo_between)

# 1. Teste de Normalidade dos Resíduos
# Shapiro-Wilk Test (para amostras menores que 5000)
teste_shapiro <- shapiro.test(residuos)
print("Teste de Normalidade (Shapiro-Wilk):")
print(teste_shapiro)

# Anderson-Darling Test (alternativa para Shapiro-Wilk)
if("nortest" %in% installed.packages()[,"Package"]) {
  teste_ad <- ad.test(residuos)
  print("Teste de Normalidade (Anderson-Darling):")
  print(teste_ad)
}

# Q-Q Plot para visualizar a normalidade
par(mfrow=c(1,2))
qqnorm(residuos, main="Q-Q Plot dos Resíduos")
qqline(residuos, col="red")

# Histograma dos resíduos
hist(residuos, breaks=20, main="Histograma dos Resíduos", xlab="Resíduos")

# Obter dados fitted do modelo
fitted_values <- fitted(modelo_between)

# 2. Teste de Homocedasticidade (variância constante)
# Breusch-Pagan Test
bp_test <- bptest(modelo_between)
print("Teste de Homocedasticidade (Breusch-Pagan):")
print(bp_test)

# Gráfico dos resíduos versus valores ajustados
plot(fitted_values, residuos, main="Resíduos vs Valores Ajustados", 
     xlab="Valores Ajustados", ylab="Resíduos")
abline(h=0, col="red")

# 3. Teste de multicolinearidade entre as variáveis independentes
# Calculando o VIF (Variance Inflation Factor)
# Criando um modelo linear com as mesmas especificações que usamos no modelo between
# precisamos extrair as médias das variáveis por grupo (já que é isso que o modelo between usa)

# Obtendo as médias por grupo
dados_medias <- aggregate(dados[, c("mortalidade_materna", "ppc", "valor_familia", "media_percentual")], 
                          by=list(cod_regsaude=dados$cod_regsaude, regiao=dados$regiao), mean)

# Criando modelo linear com as médias
modelo_ols <- lm(mortalidade_materna ~ ppc + valor_familia + media_percentual + regiao, 
                 data=dados_medias)

# Calculando VIF
if("car" %in% installed.packages()[,"Package"]) {
  vif_values <- vif(modelo_ols)
  print("Teste de Multicolinearidade (VIF):")
  print(vif_values)
}

# 4. Teste de outliers influentes
# Distância de Cook
cooksd <- cooks.distance(modelo_ols)
plot(cooksd, main="Distância de Cook", ylab="Distância", xlab="Observação")
abline(h = 4/length(cooksd), col="red")  # Limite de corte comum

# Identificando observações potencialmente influentes
influential_obs <- which(cooksd > 4/length(cooksd))
if(length(influential_obs) > 0) {
  cat("Observações potencialmente influentes:", influential_obs, "\n")
}

# 5. Teste de linearidade (parcial)
# Plotando resíduos parciais para cada variável contínua
if("car" %in% installed.packages()[,"Package"]) {
  par(mfrow=c(2,2))
  crPlots(modelo_ols, terms=c("ppc", "valor_familia", "media_percentual"))
}

# 6. Teste de independência dos resíduos
# Como o modelo between trabalha com médias, a independência dos erros
# é geralmente menos preocupante, mas podemos verificar autocorrelação espacial
# se tivermos informações de coordenadas geográficas

# 7. Resumo dos testes e recomendações
cat("\n============ RESUMO DOS TESTES DE PRESSUPOSTOS ============\n")

# Normalidade
if(teste_shapiro$p.value < 0.05) {
  cat("Normalidade: Rejeitada (p < 0.05). Considere transformações ou modelos robustos.\n")
} else {
  cat("Normalidade: Não rejeitada (p >= 0.05). Pressuposto atendido.\n")
}

# Homocedasticidade
if(bp_test$p.value < 0.05) {
  cat("Homocedasticidade: Rejeitada (p < 0.05). Use erros padrão robustos.\n")
} else {
  cat("Homocedasticidade: Não rejeitada (p >= 0.05). Pressuposto atendido.\n")
}

# Multicolinearidade
if(exists("vif_values")) {
  if(any(vif_values > 10)) {
    cat("Multicolinearidade: Problema detectado (VIF > 10). Considere remover variáveis altamente correlacionadas.\n")
  } else if(any(vif_values > 5)) {
    cat("Multicolinearidade: Problema moderado (5 < VIF < 10). Monitorar.\n")
  } else {
    cat("Multicolinearidade: Não detectada (VIF < 5). Pressuposto atendido.\n")
  }
}

# Outliers influentes
if(length(influential_obs) > 0) {
  cat("Outliers influentes: Detectados. Considere analisar ou remover observações:", influential_obs, "\n")
} else {
  cat("Outliers influentes: Não detectados em níveis preocupantes.\n")
}

# Se houver violações, usar modelos robustos
if(teste_shapiro$p.value < 0.05 || bp_test$p.value < 0.05) {
  # Modelo robusto com erros padrão corrigidos
  library(sandwich)
  library(lmtest)
  
  robusto <- coeftest(modelo_between, vcov = vcovHC(modelo_between, type = "HC1"))
  cat("\nResultados com erros padrão robustos (devido a violações de pressupostos):\n")
  print(robusto)
  
  # Transformação Box-Cox (se necessário para normalidade)
  if(teste_shapiro$p.value < 0.01 && all(dados_medias$mortalidade_materna > 0)) {
    cat("\nConsiderando transformação Box-Cox para normalizar a variável dependente\n")
    bc <- boxcox(mortalidade_materna ~ ppc + valor_familia + media_percentual + regiao, 
                 data = dados_medias)
    lambda <- bc$x[which.max(bc$y)]
    cat("Lambda ideal para transformação Box-Cox:", lambda, "\n")
    
    # Aplicando transformação e recalculando o modelo
    if(abs(lambda) < 0.01) {  # próximo de zero
      dados_medias$mm_transformed <- log(dados_medias$mortalidade_materna)
      cat("Aplicando transformação logarítmica (lambda próximo de zero)\n")
    } else {
      dados_medias$mm_transformed <- (dados_medias$mortalidade_materna^lambda - 1)/lambda
      cat("Aplicando transformação Box-Cox com lambda =", lambda, "\n")
    }
    
    modelo_transformado <- lm(mm_transformed ~ ppc + valor_familia + media_percentual + regiao, 
                              data = dados_medias)
    cat("\nModelo com variável dependente transformada:\n")
    print(summary(modelo_transformado))
  }
}

dados <- base_modelo
# Solução alternativa para erros padrão robustos em modelo between

# 1. Criar um dataset com as médias de cada variável por unidade cross-sectional (cod_regsaude)
dados_medias <- aggregate(dados[, c("mortalidade_materna", "ppc", "valor_familia", "media_percentual")], 
                          by=list(cod_regsaude=dados$cod_regsaude, regiao=dados$regiao), mean)

dados_medias |> 
  select(mortalidade_materna,
         ppc, valor_familia, media_percentual) |> 
  
  
# 2. Verificar a estrutura do dataset de médias
head(dados_medias)

# 3. Ajustar um modelo OLS com os dados médios
# Isso é equivalente ao modelo between
modelo_between_alt <- lm(mortalidade_materna ~ ppc + valor_familia + media_percentual + regiao, 
                         data=dados_medias)

# 4. Resumo do modelo
summary(modelo_between_alt)

# 5. Comparar com o modelo between original
cat("\nComparação dos coeficientes:\n")
coef_between <- coef(modelo_between)
coef_ols <- coef(modelo_between_alt)
cbind(between=coef_between, ols=coef_ols)

# 6. Testes de pressupostos
# Normalidade
shapiro.test(residuals(modelo_between_alt))

# Homocedasticidade
library(lmtest)
bptest(modelo_between_alt)

# 7. Erros padrão robustos (funciona com o modelo OLS)
library(sandwich)
library(lmtest)
coeftest(modelo_between_alt, vcov = vcovHC(modelo_between_alt, type = "HC1"))

# 8. Visualizações importantes
par(mfrow=c(2,2))
plot(modelo_between_alt)

# 9. Teste para outliers influentes
cooksd <- cooks.distance(modelo_between_alt)
plot(cooksd, main="Distância de Cook", 
     ylab="Distância", 
     xlab="Observação")
abline(h = 4/length(cooksd), col="red")

# Identificando observações potencialmente influentes
influential_obs <- which(cooksd > 4/length(cooksd))
if(length(influential_obs) > 0) {
  cat("\nObservações potencialmente influentes (por índice):", influential_obs, "\n")
  # Obtendo os valores reais das observações
  print(dados_medias[influential_obs, ])
}

# 10. Verificação de multicolinearidade
library(car)
vif(modelo_between_alt)

