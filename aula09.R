library(ggplot2)
library(palmerpenguins)
library(class)

dados <- read.table("cancer.csv",header = TRUE,sep = ",")
str(dados)

n <- round(0.8*nrow(dados))
indice_treino <- sample(1:nrow(dados),n)

treino <- dados[indice_treino,]
teste <- dados[-indice_treino,]

treino_padronizado <- scale(treino[,-1])
teste_padronizado <- scale(teste[,-1])

classe_treino <- treino$diagnosis
classe_teste <- teste$diagnosis

cor(treino[,-1])

modelo1 <- knn(train = treino_padronizado,test = teste_padronizado, cl=classe_treino,k=1)
mean(modelo1 == teste$diagnosis)
#Matriz de confusão, verifica se está correto a taxa
table(modelo1, classe_teste)

taxa <- c()
for(i in 1:10){
  modelo <- knn(train = treino_padronizado,test = teste_padronizado, cl=classe_treino,k=i)
  mean(modelo == teste$diagnosis)
  taxa[i] <- mean(modelo == classe_teste)
}
df <- data.frame(i=1:10,taxa

ggplot(data = df,aes(x=i,y=taxa)) + geom_boxplot() + theme_minimal()
