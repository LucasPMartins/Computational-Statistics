library(ggplot2)
library(class)
library(rpart)
library(rpart.plot)

dados <- read.csv(file = "cancer.csv",header = TRUE)

n <- round(0.8*nrow(dados))
indice_treino <- sample(1:nrow(dados),n)

treino <- dados[indice_treino,]
teste <- dados[-indice_treino,]

modelo_arvore <- rpart(formula = diagnosis~ ., data = treino, method = "class") 

rpart.plot(modelo_arvore,extra = 101)

previsao <- predict(modelo_arvore,newdata = teste,type = "class")
previsao

mean(previsao ==teste$diagnosis)
