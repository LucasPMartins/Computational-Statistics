#           PRIMEIRA LISTA DE EXERCÍCIOS
#             Estatística Computacional

# Exercício 1 ----------------------------------------

# a)
vetor_a <- c(10:30)
print(vetor_a)
# b)
vetor_b <- c(30:10)
print(vetor_b)
# c)
vetor_c <- c(vetor_a[1:length(vetor_a)-1],vetor_b)
print(vetor_c)

# Exercício 2 ----------------------------------------

help(rep)
help(seq)

# a) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 
# 8), em que há dez ocorrências do número 2
vetor_a <- c(rep(seq(2,8,2),10))
print(vetor)

# b) rie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8
# , 2), em que há onze ocorrências do número 2 e dez
#ocorrências dos números 4, 6 e 8.
vetor_b <- c(vetor_a,2)

# Exercício 3 ----------------------------------------

# a)
vetor <- c(20:30)
sum(vetor**2 + 4*vetor)
# Outra forma de se fazer:
soma <- 0
for (valor in vetor_3a) {
  soma <- soma + valor**2 + 4*valor
}

# b)
vetor <- c(10:20)
sum((3**vetor/vetor)+(2**vetor/vetor**2))

# Exercício 4 ----------------------------------------

#       Sorteio(1 até 100,tamanho,repetição)
vetor <- sample(1:100,40,replace = TRUE)

# a) Quantas bolas pares foram sorteadas?
pares <- sum(vetor %% 2 == 0)
# ou 
pares <- vetor[vetor %% 2 == 0]
length(pares)

# b) Quantas bolas maiores do que 70 foram sorteadas?
maior_70 <- sum(vetor > 70)

# c) Em quais retiradas (posições) foram sorteadas as
#bolas ímpares?
posicoes_impares <- which(vetor %% 2 != 0)
print(posicoes_impares)

# Exercício 5 ----------------------------------------

lancamentos_4 <-function(){
  dado <- c()
  count <- 0
  while(count < 2){
    valor <-sample(1:6,1)
    dado <- c(dado,valor)
    if(valor == 4){
      count <- count + 1
    }
  }
  return(length(dado))
}

vezes <- lancamentos_4()
print(vezes)

# Exercício 6 ----------------------------------------

quantidade <- c()

for(i in 1:10000){
  valor <- lancamentos_4()
  quantidade <- c(quantidade,valor)
}

mean(quantidade)

# Exercício 7 ----------------------------------------

fibonacci <- function(n){
  sequencia <- c(1,1)
  anterio <- 1
  atual <- 2
  n <- n - 2
  while(n > 0){
    sequencia <- c(sequencia,(sequencia[anterio] + sequencia[atual]))
    n <- n - 1
    anterio <- anterio + 1
    atual <- atual + 1
  }
  return (sequencia)
}

teste <- fibonacci(10)
teste

# Exercício 8 ----------------------------------------

