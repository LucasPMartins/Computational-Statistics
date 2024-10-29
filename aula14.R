library(ggplot2) # pacote para criação de gráficos

data(iris) # carrega o dataset iris

ggplot(data= iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point() # cria um gráfico de dispersão

iris$Species <- as.factor(iris$Species) # converte a coluna Species para fator

str(iris) # mostra a estrutura do dataset iris

cor(iris$Petal.Length, iris$Petal.Width) # calcula a correlação entre Petal.Length e Petal.Width

cor(iris[,-5]) # calcula a correlação entre todas as variáveis do dataset iris

setosa <- iris[iris$Species == "setosa",] # filtra as observações da espécie setosa
cor(setosa$Petal.Length, setosa$Petal.Width) # calcula a correlação entre Petal.Length e Petal.Width para a espécie setosa

versicolor <-iris[iris$Species == "versicolor",] # filtra as observações da espécie versicolor
cor(versicolor$Petal.Length, versicolor$Petal.Width) # calcula a correlação entre Petal.Length e Petal.Width para a espécie versicolor

virginica <- iris[iris$Species == "virginica",] # filtra as observações da espécie virginica
cor(virginica$Petal.Length, virginica$Petal.Width) # calcula a correlação entre Petal.Length e Petal.Width para a espécie virginica

# -------------------------------------------------------------------

dados <-read.csv("femur.csv")   # carrega o arquivo de dados
dados$genero <- as.factor(dados$genero) # converte a coluna genero para fator
dados <- dados[,-1] # remove a primeira coluna do dataset
str(dados) # mostra a estrutura do dataset dados

male <- dados[dados$genero == 'Male',] # filtra as observações do gênero masculino
cor(male$altura, male$femur) # calcula a correlação entre altura e femur para o gênero
mean(male$altura) # calcula a média da altura para o gênero masculino

female <- dados[dados$genero == 'Female',]  # filtra as observações do gênero feminino
cor(female$femur, female$altura) # calcula a correlação entre femur e altura para o gênero
mean(female$altura) # calcula a média da altura para o gênero feminino

ggplot(data = male, aes(x= femur,y = altura)) + geom_point() # cria um gráfico de dispersão para o gênero masculino

ggplot(data = female, aes(x= femur,y = altura)) + geom_point() # cria um gráfico de dispersão para o gênero feminino

modelo_linear1 <- lm(data = male,formula = altura ~ femur) # cria um modelo de regressão linear para o gênero masculino
modelo_linear1

modelo_linear2 <- lm(data = female, formula = altura ~ femur) # cria um modelo de regressão linear para o gênero feminino
modelo_linear2

summary(male$femur) # mostra um resumo dos dados do femur para o gênero masculino
summary(female$femur) # mostra um resumo dos dados do femur para o gênero feminino
