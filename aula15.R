library(ggplot2) # pacote para gráficos

# Importar dados do arquivo "grilo.txt"
dados <- read.csv("grilo.txt")

# Criar histograma da variável 'frequencia'
ggplot(dados, aes(x = frequencia)) + 
  geom_histogram(bins = 10, col = 'white')

# Calcular a correlação entre 'temperatura' e 'frequencia'
cor(dados$temperatura, dados$frequencia)

# Calcular a matriz de correlação para todas as variáveis
cor(dados)

# Gráfico de dispersão com linha de regressão linear
ggplot(data = dados, aes(x = temperatura, y = frequencia)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = 'lm', se = TRUE)

# Ajustar modelo de regressão linear
modelo_linear <- lm(formula = frequencia ~ temperatura, data = dados)
modelo_linear

# Resumo dos dados da variável 'temperatura'
summary(dados$temperatura)

# Criar dataframe para previsão
w <- data.frame(temperatura = c(21, 23.6, 30.9))

# Previsão de frequência para as temperaturas especificadas
predict(modelo_linear, newdata = w)

# ------------------------------------------------------------------------------

library(palmerpenguins) # pacote com dados de pinguins
library(dplyr)  # pacote para manipulação de dados

# Importar dados dos pinguins e remover valores nulos (NA)
dados <- penguins
dados <- na.omit(dados)

# Estrutura dos dados
str(dados)

# Remover colunas não necessárias
dados <- dados[, -c(1, 2, 7, 8)]

# Calcular matriz de correlação para as variáveis selecionadas
cor(dados[3:6])

# Calcular a correlação entre 'flipper_length_mm' e 'body_mass_g'
cor(dados$flipper_length_mm, dados$body_mass_g)

# Gráfico de dispersão com linha de regressão linear para os dados dos pinguins
ggplot(data = dados, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = 'lm', se = TRUE)

# Calcular correlação para cada espécie de pinguim
dados |>
  filter(species == "Gentoo") |>
  select(flipper_length_mm, body_mass_g) |>
  cor()

dados |>
  filter(species == "Adelie") |>
  select(flipper_length_mm, body_mass_g) |>
  cor()

dados |>
  filter(species == "Chinstrap") |>
  select(flipper_length_mm, body_mass_g) |>
  cor()

# Ajustar modelo de regressão linear simples
modelo_linear <- lm(formula = flipper_length_mm ~ body_mass_g, data = dados)
modelo_linear

# Ajustar modelo de regressão linear múltipla
modelo2 <- lm(formula = flipper_length_mm ~ body_mass_g + bill_length_mm, data = dados)
summary(modelo2)