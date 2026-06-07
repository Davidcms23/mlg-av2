# -------------------------------------------------------------------------
# Script 1b: Análise Exploratória e Justificativa Funcional (Média-Variância)
# -------------------------------------------------------------------------

library(tidyverse)

# 1. Leitura dos dados limpos
dados_to <- read_csv("dados/tocantins.csv")

# -------------------------------------------------------------------------
# Gráfico A: Dispersão e Assimetria (Inspirado nos Slides 7 a 10)
# Mostra a concentração de renda na base e a expansão da variância
# -------------------------------------------------------------------------
dados_to |> 
  ggplot(aes(x = VD4031, y = VD4016, color = V2007)) +
  geom_point(alpha = 0.4, position = position_jitter(width = 1)) +
  theme_minimal() +
  labs(
    title = "Dispersão da Renda por Horas Trabalhadas",
    subtitle = "Evidência de heterocedasticidade estrutural",
    x = "Horas Semanais Trabalhadas",
    y = "Renda Principal (R$)",
    color = "Sexo"
  )

dados_to |> 
  ggplot(aes(x = log(VD4031), y = log(VD4016), color = V2007)) +
  geom_point(alpha = 0.4, position = position_jitter(width = 1)) +
  theme_minimal() +
  labs(
    title = "Dispersão da Renda por Horas Trabalhadas",
    subtitle = "Evidência de heterocedasticidade estrutural",
    x = "Horas Semanais Trabalhadas",
    y = "Renda Principal (R$)",
    color = "Sexo"
  )

# -------------------------------------------------------------------------
# Gráfico B: Verificação da Relação Média-Variância (Inspirado no Slide 16)
# Passo fundamental para justificar a escolha da Distribuição Gama
# -------------------------------------------------------------------------

# Agrupamento dos dados por faixas de Idade e Raça para criar "clusters" locais
renda_agrupada <- dados_to |> 
  mutate(Faixa_Idade = cut(V2009, breaks = 10)) |> 
  group_by(Faixa_Idade, V2010) |> 
  summarise(
    media_renda = mean(VD4016, na.rm = TRUE),
    var_renda   = var(VD4016, na.rm = TRUE),
    .groups     = "drop"
  ) |> 
  filter(media_renda > 0, var_renda > 0)

# Plotagem da relação linear em escala logarítmica
renda_agrupada |> 
  ggplot(aes(x = log(media_renda), y = log(var_renda))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Verificação Empírica da Função de Variância",
    subtitle = "A inclinação da reta define a distribuição (Gama ~ 2, Normal Inversa ~ 3)",
    x = "log(Médias Locais da Renda)",
    y = "log(Variâncias Locais da Renda)"
  )

modelo_variancia <- lm(log(var_renda) ~ log(media_renda), data = renda_agrupada)
# Coeficientes da Relação Média-Variância
coef(modelo_variancia)
