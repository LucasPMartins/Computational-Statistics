
urna <- 1:30


sorteio <- sample(urna, size = 1)
while(length(unique(sorteio)) < 30){
  bilhete <- sample(urna, size = 1)
  sorteio <- c(sorteio, bilhete)
}

length(sorteio)

resultados <- c()

for(i in 1:10000){
  sorteio <- sample(urna, size = 1)
  while(length(unique(sorteio)) <30){
    bilhete <- sample(urna, size = 1)
    sorteio <- c(sorteio, bilhete)
  }
  resultados[i] <- length(sorteio)
}

mean(resultados)
