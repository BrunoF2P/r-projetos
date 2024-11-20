install.packages("tidyverse")
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

dataset <- read.csv("/home/bruno/Documentos/Desenvolvimento/r-projetos/accidents_2017_to_2023_portugues.csv",encoding = "UTF-8")

# Media
calcular_media <- function(vetor) {
  soma <- 0
  contador <- 0
  for (valor in vetor) {
    if (!is.na(valor)) {
      soma <- soma + valor
      contador <- contador + 1
    }
  }
  return(soma / contador)
}

lm_manual_formula <- function(formula, data) {
  # Converte a fórmula em variáveis x e y
  mf <- model.frame(formula, data)
  x <- mf[[2]]  # Esse e o segundo
  y <- mf[[1]]  # Esse e o primeiro
  
  # Calcula as médias
  y_bar <- calcular_media(y)
  x_bar <- apply(x, 2, calcular_media)
  
  # Calcula o coeficiente angular (beta1)
  beta1 <- apply(x, 2, function(x_col) {
    sum((x_col - x_bar) * (y - y_bar)) / sum((x_col - x_bar)^2)
  })
  
  # Calcula o intercepto (beta0)
  beta0 <- y_bar - sum(beta1 * x_bar)
  
  list(intercepto = beta0, coeficiente = beta1)
}


# Q2 (A)

modelo <- lm_manual_formula(feridos ~ veiculos, data = dataset)

summary(modelo)


ggplot(dataset, aes(x = veiculos, y = feridos)) +
  geom_point(color = "#1f77b4", size = 2, alpha = 0.7) +
  # Não estou usando lib viu professor
  geom_abline(
    intercept = modelo$intercepto,
    slope = modelo$coeficiente,
    color = "#e31a1c", size = 1.5
  ) +
  labs(
    title = "Relação entre o Número de Veículos e o Número de Feridos",
    x = "Número de Veículos Envolvidos",
    y = "Número de Feridos",
    caption = "Fonte: Dados de Acidentes 2017-2023"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 1),
    panel.grid = element_blank()
  ) 
ggsave("images/grafico_relacao_nveiculos_nferidos.png", width = 8, height = 6)
