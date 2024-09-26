dados <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
dados <- dados[,-c(1,9:12)]

dados$Survived <- as.factor(dados$Survived)
dados$Pclass <- as.factor(dados$Pclass)
dados$Sex <- as.factor(dados$Sex)

str(dados)

summary(dados)

ggplot(data = dados,aes(x = Survived)) + geom_bar(fill = "blue") + theme_minimal()

ggplot(data = dados,aes(x = Survived,fill = Sex)) + geom_bar()+ scale_fill_manual(values = c("male"="#30f","female"="#d43")) + theme_minimal()

ggplot(data = dados,aes(x = Sex,fill = Survived)) + geom_bar() + scale_fill_manual(values= c("0" = 'red','1' = 'lightgreen' ),label = c("1"="Sobreviveu","0"="Morreu")) +facet_wrap(~Pclass) + 
theme_minimal() + ggtitle("Análise de sobrevivência de homens e mulheres")

iris <- iris[sample(nrow(iris)),]
iris

n<- round(0.8*nrow(iris))
treinamento <- iris[1:n,]
teste <- iris[-(1:n),]

ggplot(data= treinamento, aes(x=Petal.Length,y=Petal.Width,color=Species)) + geom_point(size=5,alpha=0.5) + theme_minimal()
