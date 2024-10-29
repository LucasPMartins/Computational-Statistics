library(janeaustenr) # pacote com os livros da Jane Austen
library(tidytext) # pacote com funções para análise de texto
library(dplyr) # pacote para manipulação de dados
library(stringr) # pacote para manipulação de strings
library(stopwords) # pacote com stopwords em inglês
library(ggplot2) # pacote para criação de gráficos
library(tidyr) # pacote para manipulação de dados

livros <- austen_books() # carrega os livros da Jane Austen
unique(livros$book) # mostra os livros disponíveis

# livros[livros$book == "Emma",] ou livros |> filter(book == "Emma")
# Para filtrar os livros da Jane Austen que são "Emma"

# Filtra o livro "Emma", divide o texto em palavras, conta a frequência de cada 
#palavra e seleciona as 10 mais frequentes
livros |>
  filter(book == "Emma")|>
  unnest_tokens(output = word, input = text) |>
  count(word,sort=TRUE) |>
  top_n(10) 

stopwords_en <- data.frame(word = stopwords("en")) # carrega as stopwords em inglês

# Filtra o livro "Emma", divide o texto em palavras, remove as stopwords,
# conta a frequência de cada palavra e seleciona as 10 mais frequentes
livros |>
  filter(book == "Emma")|>
  unnest_tokens(output = word, input = text) |>
  anti_join(stopwords_en) |>
  count(word, sort = TRUE) |>
  top_n(10)

# Filtra o livro "Emma" e imprime as 20 primeiras linhas
livros |>
  filter(book == "Emma") |>
  print(n = 20)

# Salva o livro "Emma" em um objeto chamado "emma"
emma <- livros |>
  filter(book == "Emma") 

# Adiciona uma coluna chamada "capitulos" ao objeto "emma" que indica o número do capítulo
capitulos <- str_detect(emma$text, "^CHAPTER [IVXLCDM]+")
capitulos <- cumsum(capitulos) # soma cumulativa dos capítulos
capitulos
emma$capitulos <- capitulos # adiciona a coluna "capitulos" ao objeto "emma"
str(emma) # mostra a estrutura do objeto "emma"

# Filtra o livro "Emma", divide o texto em palavras, remove as stopwords,
# junta com os sentimentos do lexicon "bing", conta a frequência de sentimentos
# por capítulo, calcula a diferença entre sentimentos positivos e negativos,
# e cria um gráfico de barras com a diferença de sentimentos por capítulo
emma |>
  unnest_tokens(word, text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"), relationship = "many-to-many") |>
  count(capitulos, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total)) + geom_col()

# 1. Agrupa os dados pelo nome do livro.
# 2. Cria uma coluna 'capitulos' que identifica os capítulos dos livros com base em padrões de texto.
# 3. Desagrupa os dados.
# 4. Separa os textos em tokens de palavras.
# 5. Remove as stopwords (palavras comuns que não agregam significado relevante).
# 6. Realiza uma junção interna com o léxico de sentimentos "bing" para identificar palavras positivas e negativas.
# 7. Conta o número de palavras positivas e negativas por livro e capítulo.
# 8. Reorganiza os dados para que cada sentimento (positivo e negativo) seja uma coluna.
# 9. Calcula o total de sentimentos (positivos - negativos) por capítulo.
# 10. Gera um gráfico de barras mostrando a variação dos sentimentos ao longo dos capítulos para cada livro, com uma faceta para cada livro.
livros |>
  group_by(book) |>
  mutate(capitulos = cumsum(str_detect(text,regex("^chapter (\\d|[IVXCDLM])+", ignore_case = TRUE)))) |>
  ungroup() |>
  unnest_tokens(word,text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"),relationship = "many-to-many") |>
  count(book,capitulos,sentiment) |>
  spread(sentiment, n,fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total,fill=book)) + geom_col(show.legend = FALSE) + facet_wrap(~book,scales = "free_x") + theme_minimal()
