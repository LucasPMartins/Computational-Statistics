# Este script realiza várias simulações estatísticas e visualizações.

# Calcula a média de uma amostra de tamanho 500 de uma distribuição uniforme de inteiros de 1 a 6.
# Armazena o resultado na variável 'media'.
media <- mean(sample(1:6, size = 500, replace = TRUE))
media

# Inicializa um vetor vazio 'resultados' para armazenar as médias de múltiplas amostras.
resultados <- c()

# Loop 1000 vezes para gerar amostras de tamanho 500 de uma distribuição uniforme de inteiros de 1 a 6,
# calcular suas médias e armazenar essas médias em 'resultados'.
for(j in 1:1000) {
  resultados[j] <- mean(sample(1:6, size = 500, replace = TRUE)) 
}
resultados

# Plota um histograma do vetor 'resultados'.
hist(resultados)

# Cria um data frame 'dados' com os resultados.
dados <- data.frame(resultados)

# Plota um histograma usando ggplot2.
ggplot(dados, aes(x = resultados)) + geom_histogram()

# Plota uma densidade usando ggplot2.
ggplot(dados, aes(x = resultados)) + geom_density(fill = 'darkblue') + theme_minimal()

# Gera uma população de 100000 elementos com distribuição binária (0.9 para 0 e 0.1 para 1).
populacao <- sample(0:1, size = 100000, replace = TRUE, prob = c(0.9, 0.1))
valor_real <- mean(populacao)

# Tira uma amostra de tamanho 500 da população e calcula a média.
amostra <- sample(populacao, size = 500, replace = TRUE)
media <- mean(amostra)
media

# Calcula o intervalo de confiança de 95% para a média da amostra.
parte_inferior <- media - 1.96 * (sqrt(media * (1 - media)) / sqrt(500))
parte_superior <- media + 1.96 * (sqrt(media * (1 - media)) / sqrt(500))

valor_real

# Inicializa vetores para armazenar os limites inferiores e superiores dos intervalos de confiança.
inferiores <- c()
superiores <- c()

# Loop 100 vezes para gerar amostras, calcular médias e intervalos de confiança.
for(j in 1:100) {
  amostra <- sample(populacao, size = 500, replace = TRUE)
  media <- mean(amostra)
  inferiores[j] <- media - 1.96 * (sqrt(media * (1 - media)) / sqrt(500))
  superiores[j] <- media + 1.96 * (sqrt(media * (1 - media)) / sqrt(500))
}

inferiores[23]
superiores[23]

# Cria um data frame 'intervalos' com os limites inferiores, superiores e um contador.
intervalos <- data.frame(inferiores, superiores, contador = 1:100)

# Plota os intervalos de confiança usando ggplot2.
ggplot(intervalos) + geom_segment(aes(x = inferiores, xend = superiores, y = contador, yend = contador, col = categoria)) +
  geom_vline(xintercept = valor_real, col = 'red')

# Cria uma categoria para identificar se o intervalo contém o valor real ou não.
categoria <- ifelse(intervalos$inferiores > valor_real | intervalos$superiores < valor_real, 0, 1)

# Adiciona a categoria ao data frame 'intervalos'.
intervalos$categoria <- as.factor(categoria)
