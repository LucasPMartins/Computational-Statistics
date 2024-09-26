# Aula 2

a <- c(1,2,5)
b <- c(10,12)
d <- c(a,6)
print(d)
barplot(d) # plota um grafico de barras

# Laço FOR
vetor <- c(8,9,1)
tam <- length(vetor)
vetor[1]

for(i in 1:tam){
  for(j in 1:tam){
    if(vetor())
  }
    
  print(i)
}


# Aniversario

aniversarios <- sample(x = 1:365, size = 40, replace = TRUE)
aniversarios
quantidade <- sum(duplicated(aniversarios))
# ou any(duplicated(aniversarios))
print("Quantidade: ")
quantidade

resultados <- c() #Crio um vetor vazio inicialmente

for(j in 1:10000)
{
  aniversarios <- sample(x = 1:365, size = 23, replace = TRUE)
  resultados[j] <- any(duplicated(aniversarios)) # Se existir algum true, resultados[j] recebe TRUE
}
plot(mean(resultados))


# Função para calcular a probabilidade em uma sala com N alunos:

calcular_prob <- function(n) {
      resultados <- c() #Crio um vetor vazio inicialmente
  
      for(j in 1:10000)
      {
        aniversarios <- sample(x = 1:365, size = n, replace = TRUE)
        resultados[j] <- any(duplicated(aniversarios)) 
      }
      return(mean(resultados))
}

calcular_prob(n=23)


# Megasena

bilhete <- c(4,5,12,43,21,34)

semanas = 0
acertos = 0
while(acertos < 4) {
  
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE) # vetor de tamanho 6 
  #com numeros no intervalo 1:60
  acertos = sum(bilhete %in% sorteio) # %in% verifica se cada elemento de bilhete
  # está em sorteio, retornando um vetor de TRUE e FALSE, assim com o SUM conta 
  # quantos TRUE deu
  
  semanas = semanas+1
}

print("Quantidade de anos: ")
(semanas/52)

