library(ggplot2)
library(palmerpenguins)
library(class)

data("penguins")
pinguins <- penguins
str(pinguins)
pinguins <- pinguins[,-c(2,7,8)]
pinguins <- na.omit(pinguins)

n <- round(0.8*nrow(pinguins))
n
indices_treino <- sample(1:nrow(pinguins),size=n,replace = FALSE)

treino <- pinguins[indices_treino,]
teste <- pinguins[-indices_treino,]
# Removeu especie pois é não numerica, padronização
treino_padronizado <- scale(treino[,-1])
teste_padronizado <- scale(teste[,-1])
# verifica o nome dos fatores
names(treino)

classe_treino <- treino$species
classe_teste <- teste$species

?knn

cor(treino[,-1])

modelo1 <- knn(train = treino_padronizado,test = teste_padronizado, cl=classe_treino,k=1)
mean(modelo1 == teste$species)

taxa <- c()
for(i in 1:10){
  modelo <- knn(train = treino_padronizado,test = teste_padronizado, cl=classe_treino,k=i)
  mean(modelo == teste$species)
  taxa[i] <- mean(modelo == teste$species)
}
df <- data.frame(i=1:10,taxa)

ggplot(data = df,aes(x=i,y=taxa)) + geom_line()


# validação cruzada