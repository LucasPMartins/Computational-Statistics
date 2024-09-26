# Aula 5 - Iris
# Carrega o conjunto de dados iris, que é um dataset embutido no R contendo medidas de flores Iris
data(iris)

# Exibe o conjunto de dados iris em uma janela de visualização (interface gráfica do RStudio ou similar)
View(iris)

# Passo 1 - Dividir em treino e teste
# Define uma semente para garantir que a amostragem aleatória seja reproduzível
set.seed(1711)

# Obtém o número total de linhas no conjunto de dados iris
qtd = nrow(iris)

# Embaralha aleatoriamente as linhas do conjunto de dados iris
iris = iris[sample(qtd), ]

# Define o número de amostras que serão usadas para o conjunto de treinamento, que é 80% do total
n = round(0.8 * qtd)

# Cria o conjunto de treinamento contendo as primeiras 'n' linhas do conjunto de dados embaralhado
treino = iris[1:n,]
teste = iris[-(1:n), ]

ggplot(data = treino, mapping = aes(x = Species)) +
    geom_bar() + theme_classic()


ggplot(data = treino, mapping = aes(x = Petal.Length)) +
  geom_histogram(bins = 100, fill = "red") + theme_classic()


ggplot(data = treino, mapping = aes(y = Petal.Length)) +
  geom_boxplot(bins = 10,fill = "yellow") + facet_wrap(~Species)

# summary(treino$Petal.Length)



ggplot(data = treino, mapping = aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +               # Ajusta o tamanho dos pontos e a transparência
  facet_wrap(~Species) +                            # Cria painéis separados para cada espécie
  labs(title = "Distribuição do Comprimento e Largura das Pétalas por Espécie",
       x = "Comprimento da Pétala (cm)",
       y = "Largura da Pétala (cm)",
       color = "Espécie") +                         # Adiciona títulos e rótulos aos eixos e à legenda
  theme_minimal() +                                # Usa um tema minimalista para um visual limpo
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centraliza e ajusta o tamanho do título
    axis.title.x = element_text(size = 14),                             # Ajusta o tamanho do rótulo do eixo x
    axis.title.y = element_text(size = 14),                             # Ajusta o tamanho do rótulo do eixo y
    axis.text = element_text(size = 12),                                # Ajusta o tamanho do texto dos eixos
    legend.title = element_text(size = 14),                             # Ajusta o tamanho do título da legenda
    legend.text = element_text(size = 12),                              # Ajusta o tamanho do texto da legenda
    panel.background = element_rect(fill = "lightgrey"),                 # Define a cor de fundo dos painéis
    strip.background = element_rect(fill = "lightblue"),                 # Define a cor de fundo das facetas
    strip.text = element_text(size = 12, face = "bold"),                 # Ajusta o tamanho e estilo do texto das facetas
    plot.background = element_rect(fill = "gray")                       # Define a cor de fundo do gráfico
  ) +
  scale_color_manual(values = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green"))  # Define cores específicas para cada espécie

resultados = c()

for(i in 1:nrow(teste)){
  
  if(teste$Petal.Length[i] < 2.5){
    resultados[i] = "setosa";
  }else {
  
    if(teste$Petal.Width[i] < 1.75){
      resultados[i] = "versicolor";
    }
    else{
      resultados[i] = "virginica"
    }
  }
    
}
  
mean(teste$Species == resultados)

# Modelo de classificação
