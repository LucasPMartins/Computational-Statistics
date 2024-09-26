
library(httr)
library(dplyr)
library(jsonlite)

url <- "https://blaze.ac/api/roulette_games/history?startDate=2024-08-14T16:42:29.179Z&endDate=2024-09-13T16:42:29.179Z&page=1"

GET(url)
dados <- content(GET(url), "text")

dados <- fromJSON(dados)
table(dados$records$color)

url_base = "https://blaze.ac/api/roulette_games/history?startDate=2024-08-14T16:42:29.179Z&endDate=2024-09-13T16:42:29.179Z&page="

resultados <- c()
for(j in 1:50){
  url <- paste0(url_base, j)
  dados <- content(GET(url), "text")
  dados <- fromJSON(dados)
  resultados <- c(resultados, dados$records$color)
}
resultados
prop.table(table(resultados))