install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

dataset <- read.csv("/home/bruno/Documentos/Desenvolvimento/r-projetos/accidents_2017_to_2023_portugues.csv",encoding = "UTF-8")

# Q2 (A)

modelo <- lm(feridos ~ veiculos, data = dataset)

summary(modelo)


ggplot(dataset, aes(x = veiculos, y = feridos)) +
  geom_point(color = "#1f77b4", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#e31a1c", se = FALSE, size = 1.5) +
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

## --------------------------------------------------------------------------------

# Q2 (B)
modelo_multiplo <- lm(formula = feridos ~ veiculos + causa_acidente + condicao_metereologica, data = dataset)

summary(modelo_multiplo)

# Extraindo os coeficientes do modelo ajustado
coeficientes <- summary(modelo_multiplo)$coefficients
coef_df <- as.data.frame(coeficientes)
coef_df$variavel <- rownames(coef_df)

# Remover os prefixos das variáveis
coef_df$variavel_formatada <- gsub("causa_acidente|condicao_metereologica", "", coef_df$variavel)

# Selecionar as variáveis mais significativas
coef_df_filtrado <- coef_df[2:20, ]

# Ordenar novamente após a formatação
coef_df_filtrado <- coef_df_filtrado[order(abs(coef_df_filtrado$Estimate), decreasing = TRUE), ]

ggplot(coef_df_filtrado, aes(x = reorder(variavel_formatada, Estimate), y = Estimate)) + 
  geom_bar(stat = "identity", fill = "#377eb8", alpha = 0.8) + 
  coord_flip() + 
  labs(
    title = "Causas de acidente que mais impactam no número de feridos",
    x = "Causas de Acidente", 
    y = "Impacto (Coeficiente)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank()
  )
ggsave("images/grafico_coeficiente_impacto_feridos.png", width = 8, height = 6)

## Se o coeficiente estimado for positivo, significa que o aumento nesse problmea está associado a um aumento no número de feridos.

## Se o coeficiente estimado for negativo, significa que o aumento nessa problmea está associado a uma redução no número de feridos.

## --------------------------------------------------------------------------------

# Q2 (C)

# Condicional de mortes > 0
dataset$acidente_fatal <- ifelse(dataset$mortos > 0, 1, 0)
dataset$acidente_fatal <- as.factor(dataset$acidente_fatal)

dataset$fase_dia <- factor(dataset$fase_dia)
dataset$causa_acidente <- factor(dataset$causa_acidente)
dataset$tipo_pista <- factor(dataset$tipo_pista)

# Ajuste do modelo logístico
modelo_logistico <- glm(acidente_fatal ~ fase_dia + causa_acidente + tipo_pista, 
                        data = dataset, 
                        family = binomial)

# Criar um novo dataframe com condições para calcular a probabilidade
condicoes <- expand.grid(
  fase_dia = unique(dataset$fase_dia),
  causa_acidente = unique(dataset$causa_acidente),
  tipo_pista = unique(dataset$tipo_pista)
)


# Remover linhas com qualquer valor NA
condicoes <- condicoes[complete.cases(condicoes), ]

# Garantindo que os fatores em 'condicoes' possuem os mesmos níveis do dataset original
condicoes$fase_dia <- factor(condicoes$fase_dia, levels = levels(dataset$fase_dia))
condicoes$causa_acidente <- factor(condicoes$causa_acidente, levels = levels(dataset$causa_acidente))
condicoes$tipo_pista <- factor(condicoes$tipo_pista, levels = levels(dataset$tipo_pista))


# Realizando previsões de probabilidade
condicoes$probabilidade <- predict(modelo_logistico, newdata = condicoes, type = "response")

# Exibindo as condições com as probabilidades
print(condicoes)

dados_agrupados_condicoes <- condicoes %>%
  group_by(fase_dia, tipo_pista) %>%
  summarise(probabilidade_media = mean(probabilidade, na.rm = TRUE))

ggplot(dados_agrupados_condicoes, aes(x = fase_dia, y = probabilidade_media, fill = tipo_pista)) +
  
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ tipo_pista) +
  labs(
    title = "Média da Probabilidade de Acidente Fatal por Fase do Dia",
    x = "Fase do Dia",
    y = "Probabilidade Média de Acidente Fatal",
    fill = "Tipo de Pista",
    caption = "Fonte: Dados de Acidentes 2017-2023"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(linetype = "dashed", color = "gray", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = label_percent(scale = 0.1))

ggsave("images/grafico_probabilidade_acidentesf_pdia.png", width = 8, height = 6)


## --------------------------------------------------------------------------------

# Q2 (D)


# Transformando em fatores
dataset$condicao_metereologica <- factor(dataset$condicao_metereologica)
dataset$tipo_pista <- factor(dataset$tipo_pista)
dataset$tracado_via <- factor(dataset$tracado_via)

# Ajuste do modelo de regressão de Poisson
modelo_poisson <- glm(mortos ~ condicao_metereologica + tipo_pista + tracado_via,
                      data = dataset,
                      family = poisson)

# Exibir o resumo do modelo de Poisson
summary(modelo_poisson)

# Obter os coeficientes do modelo de Poisson
coeficientes_poisson <- summary(modelo_poisson)$coefficients
coeficientes_poisson_df <- data.frame(
  Fator = rownames(coeficientes_poisson),
  Coeficiente = coeficientes_poisson[, 1],
  Erro_Padrao = coeficientes_poisson[, 2],
  p_valor = coeficientes_poisson[, 4]
)

# Filtrar apenas as variáveis significativas
coeficientes_poisson_df <- coeficientes_poisson_df[coeficientes_poisson_df$Fator != "(Intercept)", ]
coeficientes_poisson_df <- coeficientes_poisson_df[coeficientes_poisson_df$p_valor < 0.05, ]

#Removendonomes do lado da variáveis
coeficientes_poisson_df$Fator <- gsub("condicao_metereologica", "", coeficientes_poisson_df$Fator)
coeficientes_poisson_df$Fator <- gsub("tipo_pista", "", coeficientes_poisson_df$Fator)
coeficientes_poisson_df$Fator <- gsub("tracado_via", "", coeficientes_poisson_df$Fator)


ggplot(coeficientes_poisson_df, aes(x = reorder(Fator, Coeficiente), y = Coeficiente)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Fatores mais importantes na causa de acidentes que levam a fatalidade",
    x = "Fatores",
    y = "Coeficiente (Efeito sobre o número de mortos)",
    caption = "Fonte: Dados de Acidentes 2017-2023"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(size = 10, face = "italic")
  )
ggsave("images/grafico_fatores_relevantes_nmortos.png", width = 8, height = 6)

