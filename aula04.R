# O parâmetro 'header = TRUE' indica que a primeira linha do arquivo contém os nomes das colunas.
# O parâmetro 'sep = ","' colunas separadas por vírgulas (formato CSV).
dados <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
dados <- dados[,-c(1,9:12)]
# Remover as colunas 1, 9, 10, 11 e 12 do data frame 'titanic'.
# A notação '-c(1,9:12)' indica que as colunas não devem ser incluídas no novo data frame

dados$Survived <- as.factor(dados$Survived)
dados$Pclass <- as.factor(dados$Pclass)
dados$Sex <- as.factor(dados$Sex)

# Exibir a estrutura do data frame 'titanic'.
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
