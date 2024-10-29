library(dplyr) # pacote para manipulação de dados
library(tidytext) # pacote com funções para análise de texto
library(SnowballC) # pacote para stemming, que é a redução de palavras à sua raiz
library(quanteda) # analise quatitativa
library(quanteda.textmodels) #modelagem de texto

noticias <- read.csv("noticias.csv")  # carrega o arquivo de notícias
View(noticias) # mostra o arquivo de notícias

stopwords_pt <- data.frame(word = stopwords("pt")) # carrega as stopwords em português

# Seleciona a primeira linha do dataframe de notícias, tokeniza o texto em palavras,
# remove as stopwords, conta a frequência das palavras e seleciona as 20 palavras mais frequentes
noticias[1,] |>
  unnest_tokens(word,texto) |>
  anti_join(stopwords_pt) |>
  count(word, sort = TRUE) |>
  top_n(20)

# Seleciona a primeira linha do dataframe de notícias, tokeniza o texto em palavras,
# remove as stopwords, aplica stemming nas palavras, conta a frequência das palavras
# e seleciona as 20 palavras mais frequentes
noticias[1,] |>
  unnest_tokens(word,texto) |>
  anti_join(stopwords_pt) |>
  mutate(word = wordStem(word,"portuguese")) |>
  count(word, sort = TRUE) |>
  top_n(20)
  
# Tokeniza o texto das notícias, removendo pontuações, símbolos, números e URLs,
# remove as stopwords em português e aplica stemming nas palavras
noticias_tokens <- tokens(noticias$texto,
              remove_punct = TRUE,
              remove_symbols = TRUE,
              remove_numbers = TRUE,
              remove_url = TRUE) |>
  tokens_remove(stopwords('pt')) |>
  tokens_wordstem(language = 'portuguese')

matriz_frequencia <- dfm(noticias_tokens) # cria uma matriz de frequência
matriz_frequencia

n <- round(0.8 * nrow(noticias)) # calcula o tamanho do conjunto de treino
n

indices <- sample(1:nrow(noticias),size=n,replace=FALSE) # seleciona os índices do conjunto de treino
treino <- matriz_frequencia[indices,] # seleciona o conjunto de treino
teste <- matriz_frequencia[-indices,] # seleciona o conjunto de teste

modelo_nb <- textmodel_nb(treino,noticias$categorias[indices]) # cria o modelo Naive Bayes

previsao <- predict(modelo_nb,newdata = teste) # faz a previsão com o modelo Naive Bayes
previsao

mean(previsao == noticias$categorias[-indices]) # calcula a acurácia do modelo
