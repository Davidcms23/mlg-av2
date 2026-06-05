library(PNADcIBGE)
library(tidyverse)
library(easystats)

variaveis_selecionadas <- c("UF", "UPA", "V1028", "V2007", "V2009", "VD3004", "VD4016")

pnadc_bruta <- get_pnadc(
  year = 2019, 
  quarter = 4, 
  vars = variaveis_selecionadas,
  deflator = FALSE 
)

dados_trabalho <- pnadc_bruta$variables  |> 
  filter(UF == "Tocantins") |> 
  mutate(
    renda = as.numeric(VD4016),
    peso_amostral = as.numeric(V1028),
    sexo = as.factor(V2007),
    idade = as.numeric(V2009),
    escolaridade = as.factor(VD3004)
  )  |> 
  filter(renda > 0, !is.na(renda), !is.na(peso_amostral))

modelo_renda <- glm(
  renda ~ sexo + idade + escolaridade,
  family = Gamma(link = "log"),
  weights = peso_amostral,
  data = dados_trabalho
)

(performance_modelo <- model_performance(modelo_renda))

check_model(modelo_renda)


parametros_robustos <- model_parameters(
  modelo_renda, 
  vcov = "HC1", 
  cluster = "UPA", 
  exponentiate = TRUE 
)
parametros_robustos

library(ggeffects)
efeitos <- ggpredict(modelo_renda, terms = c("escolaridade", "sexo"))
plot(efeitos) + 
  theme_minimal() +
  labs(title = "Renda Esperada Condicional", y = "Renda (R$)", x = "Nível de Instrução")
