---
title: "Tutorial para classificar pinguins"
author: "Lucas Martins"
lang: 'pt'
date: 2024-08-23
format: html
---

## Objetivos deste tutorial

O objetivo deste tutorial é criar um modelo de classificação que seja capaz de 
prever a espécie de um pinguim a partir de seu peso, tamanho da asa, tamanho do 
bico e profundidade do bico. Para isso, construiremos do zero um modelo KNN.

## Lendo o conjunto

Vamos iniciar nosso trabalho lendo o pacote 'palmerpenguins' e salvando o conjunto em um objeto chamado 'pinguins'.
{r}
library(palmerpenguins) #lendo o pacote
pinguins <- penguins
str(pinguins)
A seguir, vamos imprimir um resumo estatístico de cada uma ds variáveis dos dados.

{r}
summary(pinguins)
A partir do resumo das variáveis, identificamos alguns valores faltantes. Vamos removê-los.
{r}
pinguins <- na.omit(pinguins)
summary(pinguins)
#Dividindo em treino e teste
{r}
pinguins <- pinguins[sample(nrow(pinguins)),]
n = round(0.8 * nrow(pinguins))
treino = pinguins[1:n,]
teste = pinguins[-(1:n),]

```
Graficos
{r}
library(ggplot2)
ggplot(data = treino, aes(x = species , y = flipper_length_mm)) + geom_boxplot()+ theme_minimal()
+ labs(x="Especie",y = "Tamanho da Asa")


{r}
ggplot(data = treino,mapping = (aes(x = body_mass_g, y = bill_length_mm, color= species))) + geom_point(alpha=0.5) + theme_minimal() + labs(x="Peso",y = "Tamanho do bico")


Classificando o primeiro pinguim do teste
{r}
distancias <- c()
for( k in 1:nrow(treino)){
  distancias[k] <- sqrt((teste$body_mass_g[1] - treino$body_mass_g[k])**2 + (teste$bill_length_mm[1] - treino$bill_length_mm[k])**2)
}
distancias
treino$species[order(distancias)[1]]
teste$species[1]


{r}
classificacao <- c()
for(j in 1:nrow((treino))){
  distancias <- c()
for( k in 1:nrow(treino)){
  distancias[k] <- sqrt((teste$body_mass_g[j] - treino$body_mass_g[k])**2 + (teste$bill_length_mm[j] - treino$bill_length_mm[k])**2)
}
  classificacao[j] <- as.character(treino$species[order(distancias)[1]])
}
mean(classificacao == teste$species)