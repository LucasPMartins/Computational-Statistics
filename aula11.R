library(tidytext) # biblioteca para análise de texto
library(dplyr) # biblioteca para manipulação de dados
library(ggplot2) # biblioteca para criação de gráficos
library(rvest) # biblioteca para extração de dados da web
library(tidyr) # biblioteca para manipulação de dados
library(stopwords) # biblioteca com stopwords em vários idiomas

url <- 'https://www.bbc.com/portuguese/articles/cglknn698glo' #url da notícia

html <- read_html(url) #lê o html da página

# extrai o texto da página
texto <- html |> html_elements('p.bbc-hhl7in') |> html_text2() |> paste(collapse = " ")

# texto <- paste(texto, collapse = ' ')

conjunto <- data.frame(texto) # cria um data frame com o texto
conjunto # mostra o data frame

conjunto |> unnest_tokens(output = word,input = texto) |> count(word, sort = TRUE) |> top_n(10)

# A função unnest_tokens é usada para dividir o texto em tokens (geralmente palavras).
# input = texto especifica a coluna do conjunto que contém o texto a ser tokenizado.
# output = word especifica o nome da nova coluna que conterá os tokens (palavras).
# |> count(word, sort = TRUE):

# A função count conta a frequência de cada palavra (token) na coluna word.
# sort = TRUE ordena os resultados pela frequência em ordem decrescente.
# |> top_n(10):

# A função top_n(10) seleciona as 10 palavras mais frequentes.

stopwords_br <- data.frame(word= stopwords('pt')) # cria um data frame com as stopwords em português
# stopwords = palavras que são muito comuns e não são úteis para análise de texto

conjunto |> unnest_tokens(output = word,input = texto) 
|> anti_join(stopwords_br) |> count(word, sort=TRUE)|> top_n(10) 
|> mutate(word=reorder(word,n)) 
|> ggplot(aes(y = word, x = n)) + geom_col(fill='orange')+theme_minimal()
# A função anti_join(stopwords_br) remove as stopwords do texto.
# A função mutate(word=reorder(word,n)) reordena as palavras pelo número de ocorrências.
# A função ggplot(aes(y = word, x = n)) cria um gráfico de barras com as palavras no eixo y e o número de ocorrências no eixo x.
# A função geom_col(fill='orange') define a cor das barras como laranja.
# A função theme_minimal() aplica um tema minimalista ao gráfico.

# ------------------------------------------------------------------------------

library(tidyr) # biblioteca para manipulação de dados
library(janeaustenr) # biblioteca com textos de Jane Austen
library(stringr) # biblioteca para manipulação de strings

livro <- prideprejudice # texto do livro Orgulho e Preconceito

livro <- data.frame(texto =livro)  # cria um data frame com o texto do livro
livro

capitulos <- str_detect(livro$texto,"^Chapter \\d+")  # detecta os capítulos do livro
capitulos <- cumsum(capitulos)  # cria um vetor com os capítulos

stopwords_en <- data.frame(word = stopwords("en"))


livro |> unnest_tokens(output= word, input = texto) |> count(word,sort = TRUE) |> top_n(10)
livro |> unnest_tokens(output = word, input = texto) |> anti_join(stopwords_en) |> count(word,sort=TRUE) |> top_n(10)|> mutate(word=reorder(word,n))|> ggplot(aes(y= word,x=n)) + geom_col(fill='orange')+theme_minimal()
# A função anti_join(stopwords_en) remove as stopwords do texto em inglês.
# A função mutate(word=reorder(word,n)) reordena as palavras pelo número de ocorrências.
# A função ggplot(aes(y = word, x = n)) cria um gráfico de barras com as palavras no eixo y e o número de ocorrências no eixo x.

sentimentos <- get_sentiments("bing") # carrega o dicionário de sentimentos Bing

livro |>mutate(capitulo= capitulos) |> unnest_tokens(word,texto) |>inner_join(sentimentos) |> count(capitulo,sentiment) |> spread(sentiment,n,fill=0) |> mutate(total = positive - negative) |> ggplot(aes(x=capitulo,y=total)) +geom_col(fill='darkred')+theme_minimal()
# A função mutate(capitulo= capitulos) adiciona a coluna capitulo ao data frame.
# A função inner_join(sentimentos) junta o data frame com o dicionário de sentimentos.
# A função count(capitulo,sentiment) conta o número de sentimentos em cada capítulo.
# A função spread(sentiment,n,fill=0) transforma os sentimentos em colunas.
# A função mutate(total = positive - negative) calcula o total de sentimentos positivos e negativos.
# A função ggplot(aes(x=capitulo,y=total)) cria um gráfico de barras com os capítulos no eixo x e o total de sentimentos no eixo y.
