install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

dataset <- read.csv("/home/bruno/Documentos/Desenvolvimento/r-projetos/accidents_2017_to_2023_portugues.csv",encoding = "UTF-8")


# Q1 (A)
estatisticas <- dataset %>%
  summarise(
    media_mortos = mean(mortos, na.rm = TRUE),
    mediana_mortos = median(mortos, na.rm = TRUE),
    desvpad_mortos = sd(mortos, na.rm = TRUE),
    media_feridos_leves = mean(feridos_leves, na.rm = TRUE),
    mediana_feridos_leves = median(feridos_leves, na.rm = TRUE),
    desvpad_feridos_leves = sd(feridos_leves, na.rm = TRUE),
    media_veiculos = mean(veiculos, na.rm = TRUE),
    mediana_veiculos = median(veiculos, na.rm = TRUE),
    desvpad_veiculos = sd(veiculos, na.rm = TRUE)
  )

# Transformando os dados em formato longo pq o ggplot chora muito
estatisticas_longa <- estatisticas %>%
  pivot_longer(cols = everything(), names_to = "Estatística", values_to = "Valor")

estatisticas_longa <- estatisticas_longa %>%
  mutate(
    Categoria = case_when(
      grepl("mortos", Estatística) ~ "Mortos",
      grepl("feridos_leves", Estatística) ~ "Feridos Leves",
      grepl("veiculos", Estatística) ~ "Veículos"
    )
  )

# Substituindo underscores por espaços nos nomes das estatísticas
estatisticas_longa$Estatística <- gsub("_", " ", estatisticas_longa$Estatística)


ggplot(estatisticas_longa, aes(x = Categoria, y = Valor, fill = Estatística)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Estatísticas dos Acidentes: Mortos, Feridos Leves e Veículos",
    x = "Categoria",
    y = "Valor",
    caption = "Fonte: Dados de Acidentes 2017-2023"
  ) +
  theme_minimal() 

ggsave("images/grafico_estatisticas_acidentes.png", width = 8, height = 6)

## --------------------------------------------------------------------------------

# Q1 (B)
causa_acidentes <- dataset %>%
  group_by(causa_acidente) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  head(10)


ggplot(causa_acidentes, aes(x = reorder(causa_acidente, total), y = total, fill = causa_acidente)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 10 Causas mais frequentes de acidentes",
    x = "Causa do Acidente", 
    y = "Número de Acidentes",
    caption = "Fonte: Dados de Acidentes 2017-2023"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 8, color = "gray"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10) 
  )

ggsave("images/grafico_top_10_causas.png", width = 8, height = 6)

## --------------------------------------------------------------------------------

# Q1 (C)
distribuicao_acidentes <- dataset %>%
  group_by(fase_dia, tipo_acidente) %>%
  summarise(total_acidentes = n(), .groups = 'drop') %>%
  arrange(desc(total_acidentes))


ggplot(distribuicao_acidentes, aes(x = fase_dia, y = total_acidentes, fill = tipo_acidente)) +
  geom_bar(stat = "identity", size = 0.5) +
  labs(
    title = "Distribuição de Acidentes ao Longo do Dia por Tipo de Acidente",
    x = "Fase do Dia",
    y = "Número de Acidentes",
    fill = "Tipo de Acidente",
    caption = "Fonte: Dados de Acidentes 2017-2023"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 8, color = "gray"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "top",
  ) +
  coord_flip()


ggsave("images/grafico_distribuicao_acidentes.png", width = 8, height = 6)

## --------------------------------------------------------------------------------

# Q1 (D)
lesoes_por_condicao <- dataset %>%
  group_by(condicao_metereologica) %>%
  summarise(media_feridos = mean(feridos, na.rm = TRUE)) 

# Gráfico de barras mostrando a média de feridos por condição meteorológica
ggplot(lesoes_por_condicao, aes(x = condicao_metereologica, y = media_feridos, fill = condicao_metereologica)) +
  geom_bar(stat = "identity", width = 0.7) +  # Usa 'stat = identity' para barras com valores já calculados
  labs(
    title = "Média de Feridos por Condição Meteorológica",
    x = "Condição Meteorológica",
    y = "Média de Feridos",
    fill = "Condição Meteorológica",
    caption = "Fonte: Dados de Acidentes 2017-2023"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 8, color = "gray"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "top",

  )
ggsave("images/grafico_lesoes_condicao_meteorologica.png", width = 8, height = 6)

## --------------------------------------------------------------------------------
