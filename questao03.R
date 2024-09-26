
dados <- read.csv("chicago.csv",header = TRUE)
dados <- dados[,-1]
dados$season <- as.factor(dados$season)
str(dados)

# a)
mortes_cvd <- sum(dados[,2])

winter <- dados[dados$season == "Winter",]
summer <- dados[dados$season == "Summer",]
spring <- dados[dados$season == "Spring",]
autumn <- dados[dados$season == 'Autumn',]

# Criando um vetor com as mortes por estação
cvd_by_season <- c(Winter = sum(winter$cvd), Spring = sum(spring$cvd), Summer = sum(summer$cvd), Autumn = sum(autumn$cvd))

# Estação com mais e menos mortes
max_season <- names(which.max(cvd_by_season))
min_season <- names(which.min(cvd_by_season))
      
# b)

death_resp <- dados$resp
years <- as.factor(dados$year)
levels(years)

resp_by_year <- c('1997' = sum(death_resp[years == 1997]),'1998' = sum(death_resp[years == 1998]),'1999'=sum(death_resp[years == 1999]),'2000'=sum(death_resp[years == 2000]))
# Ano com mais mortes
names(which.max(resp_by_year))

# C)

# Vetores de temperatura e umidade
temp <- dados$temp
rhum <- dados$rhum
season <- as.factor(dados$season)

mean(temp[season == "Winter"])
mean(temp[season == "Spring"])
mean(temp[season == "Summer"])
mean(temp[season == "Autumn"])

mean(rhum[season == "Winter"])
mean(rhum[season == "Spring"])
mean(rhum[season == "Summer"])
mean(rhum[season == "Autumn"])

ggplot(data = dados, aes(x=temp,color=season)) + geom_boxplot()+ theme_minimal()

# D)

library(ggplot2)
ggplot(data=dados,aes(x=time,y=temp, color = season)) + geom_point() + theme_minimal()

