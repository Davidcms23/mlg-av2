# -------------------------------------------------------------------------
# Diagnóstico Visual: Histograma Empírico vs. Distribuição Gama Teórica
# -------------------------------------------------------------------------

# 1. Extração do parâmetro shape (inverso da dispersão)
gama_shape <- 1 / summary(ajuste_gama)$dispersion

# 2. Criação de um "grid" de dados simulados para gerar curvas suaves
# Fixamos a Idade na média para isolar o efeito do Sexo
idade_media <- mean(dados_to$V2009, na.rm = TRUE)

grid_teorico <- expand_grid(
  V2007 = levels(dados_to$V2007),
  # Sequência suave de renda (de 1 a 10.000, ajuste conforme seu estado)
  VD4016 = seq(1, 10000, length.out = 500) 
) |> 
  mutate(
    V2009 = idade_media,
    # Predict recupera o 'mu' (média) já com o antilog (escala em Reais)
    mu_predito = predict(ajuste_gama, newdata = cur_data(), type = "response"),
    # Na parametrização do R, rate = shape / mu
    gama_rate = gama_shape / mu_predito,
    # Calcula a densidade teórica (eixo Y)
    densidade = dgamma(VD4016, shape = gama_shape, rate = gama_rate)
  )

# 3. Plotagem combinada (Dados Empíricos + Curva Teórica)
ggplot() +
  # Histograma dos dados reais (escala de densidade)
  geom_histogram(
    data = dados_to,
    aes(x = VD4016, y = after_stat(density), fill = V2007),
    binwidth = 500, boundary = 0, alpha = 0.4, position = "identity"
  ) +
  # Linhas da densidade teórica do modelo GLM
  geom_line(
    data = grid_teorico,
    aes(x = VD4016, y = densidade, color = V2007),
    linewidth = 1.2
  ) +
  # Recorte do eixo X para facilitar a visualização (ignora outliers extremos no gráfico)
  coord_cartesian(xlim = c(0, 10000)) +
  labs(
    title = "Ajuste Teórico da Distribuição Gama à Renda Empírica",
    subtitle = paste("Idade fixada na média (", round(idade_media), "anos)"),
    x = "Renda do Trabalho Principal (R$)",
    y = "Densidade Probabilística",
    fill = "Sexo", color = "Sexo"
  ) +
  theme_minimal()
