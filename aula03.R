dados <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
# dados <- dados[,-1]

length(dados[,2]) - sum(dados[,2]) # mortos
sum(dados[,1])/length(dados[,1]) # taxa de sobrevivencia
dados <- dados[, -c(1,12)] # exclui colunas 1 e 12
summary(dados) # resumo da tabela

dados$Survived <- as.factor(dados$Survived)
dados$Sex <- as.factor(dados$Sex)
dados$Pclass <- as.factor(dados$Pclass)

str(dados)

homens <- dados[dados$Sex == "male",]
summary(homens)

table(homens$Survived)
barplot(table(homens$Survived))

mulheres <- dados[dados$Sex == 'female',]
table(mulheres$Survived)
barplot(table(mulheres$Survived))

sum(homens$Survived == 1)

sum(homens$Survive == 1 & homens$Pclass == 3)