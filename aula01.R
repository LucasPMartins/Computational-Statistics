2 * 4 # multiplicação
2 / 3 # divisão
2 + 1 # soma
3 ** 9 # potenciação
3^4  # potencia
10 %% 2 # resto
3 - 4 # subtração

variavel <- 'string' # atribuição de uma string em uma variavel
c(1,2,3) # concatenação -> transforma o conteudo dentro () em uma lista

lista <- c(1,2,3)
lista[1]  # index começa no 1
lista[lista<2] # faz uma lista somento com os valores < 2
sum(lista[lista<=3]) # função sum soma os objetos na lista <= 3

TRUE + FALSE # True = 1 e False = 0

x <- c(1,2,3)
x[1:2] # Pega elementos de 1 a 2.
mean(x) # Média do vetor
length(x) # Retorna tamanho de x
x < 30 # Como X é lista, ele vai fazer o teste para todos Elementos de X
x[x < 30] # Extraia todos numeros que sao (x < 30)
# Colchetes serve pra extrair coisas []
x[c(2,3)]  # pega as posiçoes 2,3 do vetor X
# x[2,3] Errado, DIMENSAO nao é 2x2, sim 1x1

# Para somar vetores, o vetor deve ser um multiplo do outro, nesse caso ele vai 
# reciclar o vetor MENOR para caber no vetor MAIOR, 

which(x<30)

nova_lista = c(1,2,'str')
nova_lista # na concatenção faz a tipagem mais fácil, um vetor só pode ter um tipo
# '1' '2' 'str'

length(lista) # tamanho do vetor

# Exemplo de vetor numérico
vetor_numerico <- c(1, -2, 3, 0, -5, 6)

# Transformando o vetor numérico em um vetor booleano
vetor_booleano <- vetor_numerico > 0

# Exibindo o vetor booleano
vetor_booleano

?sample

dados <- sample(1:6 , size = 1000, replace = TRUE) # Gera números aleatórios entre
# 1 : 6, size é a quandidade de num e replace é se o numero do intervalo pode se
# repetir
dados

mean(dados == 3) # media dos valores de DADOS que são == 3
1/6

p <- 23:10345 #Cria vetor com numeros de 23 a 10345
p[1]

# Programa de criar numeros aleatorios
runif(1)
runif(2)
runif(2)
class('a') == class("a") # TRUE
0/0 # NaN not a number
1/0 # Inf Infinito