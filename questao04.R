dados <- read.csv(file = 'papagaio.txt',header = TRUE)

str(dados)

head(dados)
tail(dados)

summary(dados)

dados$especie <- as.factor(dados$especie)

ggplot(data=dados,aes(x=especie)) + geom_bar() + theme_minimal()

# c)
ggplot(dados,aes(x=envergadura,y=peso,color=especie)) + geom_point()+ theme_minimal()

#Os papagaio da raça arctica são mais leve e pequenos comparados as outras especies

# D)

library(rpart)
library(rpart.plot)

n <- round(0.8*nrow(dados))
indices_treino <- sample(1:nrow(dados),size=n,replace = F)

treino <- dados[indices_treino,]
teste <- dados[-indices_treino,]

modelo <- rpart(formula = especie ~ peso + tamanho + envergadura,data=treino)
rpart.plot(modelo,extra=101)

previsao <- predict(modelo,newdata = teste, type = "class")
taxa_arvore <- mean(previsao == teste$especie)

tabela <- table(previsao, teste$tamanho) 
tabela

# E)
library(class)

dados <- na.omit(dados)
str(treino)
treino_padronizado <- scale(treino[,-4]) 
teste_padronizado <- scale(teste[,-4])

classe_treino <- treino$especie
classe_teste <- teste$especie

modelo_knn <- knn(train = treino_padronizado,test = teste_padronizado,cl = classe_treino,k=1)

mean(modelo_knn == teste$especie)
