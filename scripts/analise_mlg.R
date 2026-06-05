library(tidyverse)
library(easystats)
library(see)

# 1 - Leitura local dos dados
dados_to <- read_csv("dados/tocantins.csv") |> 
  mutate(
    V2007 = as.factor(V2007),
    V2009 = as.numeric(V2009),
    peso_amostral = as.numeric(V1028)
  )

# 2 - Ajuste do MLG Gama com pesos amostrais
ajuste_gama <- glm(
  VD4016 ~ V2007 + V2009, 
  family  = Gamma(link = "log"), 
  weights = peso_amostral,
  data    = dados_to
)


## A. Resumo Detalhado dos Parâmetros
model_parameters(ajuste_gama, exponentiate = TRUE, vcov = "HC1", cluster = "UPA")

## B. Diagnóstico de Resíduos e Suposições
validacao <- check_model(ajuste_gama)
plot(validacao)

## C. Desempenho Global do Modelo (R², AIC, BIC)
model_performance(ajuste_gama)

## D. Relatório Automatizado em Texto Corrido
report(ajuste_gama)


# Gerar valores preditos médios com intervalos de confiança
predicoes <- estimate_relation(ajuste_gama, at = c("V2007", "V2009"))






