library(rpart)
library(rpart.plot)
library(ggplot2)
library(randomForest)
library(dplyr)


#                 LISTA FINAL DE ESTATISTICA COMPUTACIONAL

#  EXERCICIO 1 =================================================================

# A) ---------------------------------------------------------------------------

diabetes <- read.csv('diabetes.txt',sep = ';')
str(diabetes)

n <- round(0.8*nrow(diabetes))
indices_treino <- sample(1:nrow(diabetes),size = n, replace = FALSE)

treino <- diabetes[indices_treino,]
teste <- diabetes[-indices_treino,]

diabetes$Diabetic <- as.factor(diabetes$Diabetic)
treino$Diabetic <- as.factor(treino$Diabetic)
teste$Diabetic <- as.factor(teste$Diabetic)

ggplot(diabetes, aes(x = as.factor(Diabetic), y = PlasmaGlucose, 
                     fill = as.factor(Diabetic))) +
  geom_boxplot() +
  theme_minimal()

ggplot(diabetes, aes(x = BMI, fill = as.factor(Diabetic))) +
  geom_histogram(alpha = 0.7) +
  theme_minimal()

ggplot(diabetes, aes(x = Age, fill = as.factor(Diabetic))) +
  geom_histogram(alpha = 0.7) +
  theme_minimal()

ggplot(diabetes, aes(x = Age, y = BMI, color = as.factor(Diabetic))) +
  geom_point(alpha = 0.6) +
  theme_minimal()

'''
Concentração de Glicose no Plasma: Os pacientes com diabetes tendem a ter 
valores mais altos de glicose em comparação aos pacientes sem diabetes.
Índice de Massa Corporal (IMC): Observa-se que pacientes diabéticos apresentam 
IMC mais alto, sugerindo uma possível relação entre peso corporal e diabetes.
Distribuição de Idade: A idade média dos pacientes com diabetes parece ser maior
do que a dos pacientes sem diabetes, indicando que a idade pode ser um fator 
relevante.
'''

# B) ---------------------------------------------------------------------------

# Modelo de árvore de decisão usando o conjunto de dados de treinamento
modelo_arvore <- rpart(formula = Diabetic~ ., data = treino, method = "class") 

# Plot da árvore de decisão
rpart.plot(modelo_arvore,extra = 101)

# Função para classificar um paciente com base nas regras da árvore
classificar_paciente <- function(Pregnancies, PlasmaGlucose, 
                                 DiastolicBloodPressure, TricepsThickness, 
                                 SerumInsulin, BMI, DiabetesPedigree, Age) {
  if(Pregnancies >= 2){
    if(BMI >= 22){
      if(SerumInsulin >= 52){
        if(Age >= 36){
          return('Paciente tem diabetes!')
        }
        else{
          if(Age < 24){
            return('Paciente tem diabetes!')
          }
          else{
            if(Age >= 27){
              return('Paciente tem diabetes!')
            }  
          }
        }
      }
      else{
        if(Age >= 36){
          return('Paciente tem diabetes!')
        }
      }
    }
  }
  return('Paciente não tem diabetes!')
}

# Exemplo de uso
classificar_paciente(Pregnancies = 3, PlasmaGlucose = 150, 
                     DiastolicBloodPressure = 80, 
                     TricepsThickness = 20, SerumInsulin = 60, BMI = 25, 
                     DiabetesPedigree = 0.5, Age = 40)

# Previsão para o conjunto de teste
previsao <- predict(modelo_arvore,newdata = teste,type = "class")

# Cálculo da acurácia
mean(previsao == teste$Diabetic)

# C) ---------------------------------------------------------------------------

# Crie o modelo de floresta aleatória
modelo_floresta <- randomForest(Diabetic ~ ., data = treino, ntree = 100)
# ntree define o número de árvores

# Faça previsões para o conjunto de teste
previsao_floresta <- predict(modelo_floresta, newdata = teste)

# Calcular a acurácia
acuracia_mf <- mean(previsao_floresta == teste$Diabetic)

# D) ---------------------------------------------------------------------------

# Probabilidades do modelo de árvore de decisão
probabilidades_ad <- predict(modelo_arvore, newdata = teste, type = "prob")

# Verificando as probabilidades para o diagnóstico "Diabetic" (com diabetes)
prob_arvore <- probabilidades_ad[, "1"] 

# Exibindo as primeiras probabilidades
head(prob_arvore)

# Probabilidades do modelo de floresta aleatória
probabilidades_mf <- predict(modelo_floresta, newdata = teste, type = "prob")

# Verificando as probabilidades para o diagnóstico "Diabetic" (com diabetes)
prob_floresta <- probabilidades_mf[,"1"]

# Exibindo as primeiras probabilidades
head(prob_floresta)

# Comparando as previsões de ambos os modelos com os valores reais
acuracia_arvore <- mean(previsao == teste$Diabetic)
acuracia_floresta <- mean(previsao_floresta == teste$Diabetic)

# Exibindo as acurácias
cat("Acurácia do modelo de árvore de decisão:", acuracia_arvore, "\n")
cat("Acurácia do modelo de floresta aleatória:", acuracia_floresta, "\n")

# E) ---------------------------------------------------------------------------

'''
Após treinar os modelos de árvore de decisão e floresta aleatória, observamos 
que a árvore de decisão apresenta uma acurácia de 90%, enquanto a floresta 
aleatória alcança 93%. A árvore de decisão é fácil de interpretar, mas tende a 
overfit se os dados forem muito complexos. Por outro lado, a floresta aleatória 
oferece uma solução mais robusta e precisa, especialmente quando há interações 
complexas entre as variáveis, mas sua interpretabilidade é mais limitada.
'''

#  EXERCICIO 2 =================================================================

# A) ---------------------------------------------------------------------------

cerebelo <- read.csv('cerebelo.csv')

# Primeiro gráfico: Peso do Cerebelo vs Peso do Corpo
ggplot(data = cerebelo, aes(y = Cerebellum_g,x = Body_g)) +
  geom_point() +
  theme_minimal()

# Segundo gráfico: Log do Peso do Cerebelo vs Log do Peso do Corpo
ggplot(data = cerebelo, aes(y = Log_cerebellum,x = Log_body)) +
  geom_point() +
  theme_minimal()

'''
O primeiro gráfico de dispersão mostra o peso do cerebelo em relação ao peso do 
corpo. Parece que, conforme o peso do corpo aumenta, o peso do cerebelo também 
tende a aumentar, embora haja alguma variabilidade, possivelmente devido a dife-
renças específicas entre as espécies.

O segundo gráfico, com valores transformados em logaritmo, mostra uma relação 
mais linear entre as duas variáveis. A transformação logarítmica frequentemente
ajuda a linearizar relações onde uma variável cresce exponencialmente em relação
a outra. Nesse caso, isso sugere que o peso do cerebelo escala de maneira aproxi
madamente proporcional ao peso do corpo em uma escala logarítmica, destacando 
uma relação mais previsível entre as espécies.

Essa comparação revela que o uso de escalas logarítmicas pode ajudar a normali-
zar dados com diferentes escalas, permitindo observar tendências subjacentes com
mais clareza.
'''

# B) ---------------------------------------------------------------------------

cor(cerebelo$Cerebellum_g,cerebelo$Body_g)

# C) ---------------------------------------------------------------------------

cor(cerebelo$Log_cerebellum,cerebelo$Log_body)

# D) ---------------------------------------------------------------------------

'''
Correlação entre o Peso do Cerebelo e o Peso do Corpo: O coeficiente de correla-
ção de aproximadamente 0,35 indica uma correlação positiva, mas fraca, entre o 
peso do cerebelo e o peso do corpo. Isso significa que, embora exista uma 
tendência de o peso do cerebelo aumentar com o peso do corpo, a relação é rela-
tivamente dispersa e menos consistente.

Correlação entre o Logaritmo do Peso do Cerebelo e o Logaritmo do Peso do Corpo:
O coeficiente de correlação de aproximadamente 0,95 para os valores log-transfor
mados indica uma correlação positiva forte entre o logaritmo do peso do cerebelo
e o logaritmo do peso do corpo. Essa relação muito próxima de 1 sugere que, em 
uma escala logarítmica, o peso do cerebelo aumenta de forma quase proporcional 
ao peso do corpo, refletindo uma relação muito mais consistente e linear.

A transformação logarítmica revela uma relação mais clara e linear entre as duas
variáveis. Isso sugere que a escala logarítmica é mais adequada para descrever 
como o peso do cerebelo varia em relação ao peso do corpo, especialmente quando 
as variáveis apresentam crescimento não-linear ou diferenças de escala 
consideráveis.
'''

# E) ---------------------------------------------------------------------------

modelo <- lm(data = cerebelo, formula = Log_cerebellum ~ Log_body)

summary(modelo)

'''
A equação da reta de regressão para os valores log-transformados é dada por:

Log_cerebellum = −2.15741 + 0.78278 × Log_body

> summary(modelo)

Call:
lm(formula = Log_cerebellum ~ Log_body, data = cerebelo)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.58221 -0.16055  0.00174  0.13638  0.56574 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2.15741    0.24449  -8.824 7.53e-07 ***
Log_body     0.78278    0.07213  10.852 6.91e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2991 on 13 degrees of freedom
Multiple R-squared:  0.9006,	Adjusted R-squared:  0.8929 
F-statistic: 117.8 on 1 and 13 DF,  p-value: 6.912e-08

Teste de Hipóteses: Ambos os coeficientes têm valores𝑝muito pequenos (7.53e-07
para o intercepto e 6.91e-08 para Log_body),o que indica que são estatisticamen-
te significativos a níveis de significância comuns (p < 0.001). Esses resultados 
sugerem que há evidências para uma relação significativa entre o log do peso do 
corpo e o log do peso do cerebelo.

'''

# Gráfico de dispersão com os dados transformados em log com a reta de regressão
ggplot(data = cerebelo, aes(y = Log_cerebellum, x = Log_body)) +
  geom_point() +
  geom_smooth(method = 'lm', color = "blue") +
  theme_minimal()
  
# F) ---------------------------------------------------------------------------

hist(modelo$residuals)

shapiro.test(modelo$residuals)

'''
Interpretação dos Resultados:
Valor-p: O valor-p de 1 indica que não há evidências suficientes para rejeitar a
hipótese nula de normalidade dos resíduos.Isso significa que,com base nos dados,
os resíduos podem ser considerados normalmente distribuídos.

Conclusão: Como o valor-p é significativamente maior que um nível comum de signi
ficância (como 0,05), podemos concluir que os resíduos seguem uma distribuição 
normal, o que é um bom indicativo para a validade do modelo de regressão linear.
A suposição de normalidade dos resíduos é importante porque reforça que o modelo
fornece estimativas confiáveis e que as inferências estatísticas (como interva-
los de confiança e testes de hipótese) são válidas.
'''  

# G) ---------------------------------------------------------------------------

# Define o peso do corpo em gramas
peso <- 100000

# Converte o peso do corpo para a escala logarítmica (base 10)
log_peso <- log10(peso)

# Coeficientes do modelo de regressão
b <- -2.15741
a <- 0.78278

# Calcula o logaritmo do peso do cerebelo usando a equação de regressão
log_cerebelo <- a * log_peso + b

# Converte de volta para a escala original (gramas)
celebelo_peso <- 10^log_cerebelo
celebelo_peso

'''
A previsão para o peso do cerebelo de uma espécie que pesa 100.000g 
é aproximadamente 57,1 g
'''

#  EXERCICIO 3 =================================================================

# A) ---------------------------------------------------------------------------

# Carrega o conjunto de dados
olive <- read.csv('olive.txt')

# Padroniza os dados e salvar no objeto 'dados_padronizados'
dados_padronizados <- scale(olive[,-1]) # Remove a coluna não numérica

# Visualiza a estrutura dos dados padronizados
str(dados_padronizados)

# B) ---------------------------------------------------------------------------

# Cria o modelo K-means com k = 3
clusterizacao <- kmeans(dados_padronizados, centers = 3)

# Adiciona o vetor de clusters ao conjunto de dados como uma variável categórica
olive$cluster_k3 <- as.factor(clusterizacao$cluster)

# Visualização do data frame com a nova variável cluster_k3
head(olive)

# Gráfico de barras com K = 3
ggplot(olive, aes(x = cluster_k3, fill = region)) +
  geom_bar() +
  theme_minimal()

'''
Comentários sobre os resultados:
Utilizando k = 3, podemos perceber pelo gráfico que o cluster 2 é dominada pela 
região Southern Italy e o cluster 1 é dominada pela região Northern Italy, 
oque indica forte correlação entre a variável region e as características dos 
dados que formam o cluster. Isso poderia sugerir que o K-means foi capaz de 
identificar agrupamentos naturais baseados em uma combinação de variáveis 
numéricas, refletindo diferentes padrões regionais. Além disso, o terceiro 
cluster está dividido quase igualmente entre as regiões Northern Italy, Southern
Italy e Sardinia. Isso pode indicar que este cluster representa um conjunto mais
heterogêneo de observações, onde as características numéricas não são tão 
exclusivas de uma região específica
'''

# C) ---------------------------------------------------------------------------

# Cria o modelo K-means com k = 4
clusterizacao <- kmeans(dados_padronizados, centers = 4)

# Adiciona o vetor de clusters ao conjunto de dados como uma variável categórica
olive$cluster_k4 <- as.factor(clusterizacao$cluster)

# Visualização do data frame com a nova variável cluster_k4
head(olive)

# Gráfico de barras com K = 4
ggplot(olive, aes(x = cluster_k4, fill = region)) +
  geom_bar() +
  theme_minimal()

'''
Comentários sobre os resultados:
Ao utilizar k = 4, houve uma mudança significativa na distribuição dos clusters 
em comparação com o modelo de 3 clusters. Cada cluster agora é quase exclusivo 
de uma única região, o que indica que as observações dentro de cada cluster 
compartilham características muito semelhantes que estão mais alinhadas com os 
padrões encontrados em uma região específica. Essa mudança sugere que, ao aumen-
tar o número de clusters, o modelo foi capaz de capturar melhor as nuances 
regionais dos dados.
'''

# Cria o modelo K-means com k = 5
clusterizacao <- kmeans(dados_padronizados, centers = 5)

# Adiciona o vetor de clusters ao conjunto de dados como uma variável categórica
olive$cluster_k5 <- as.factor(clusterizacao$cluster)

# Visualização do data frame com a nova variável cluster_k5
head(olive)

# Gráfico de barras com K = 5
ggplot(olive, aes(x = cluster_k5, fill = region)) +
  geom_bar() +
  theme_minimal()

'''
Comentários sobre os resultados:
Semelhante ao k = 4 quando usamos k = 5, a distribuição dos clusters continua 
semelhante ao modelo com k = 4, ou seja, cada cluster permanece quase exclusivo 
de uma única região. Isso indica que o aumento do número de clusters não alterou 
significativamente a separação das observações de acordo com a variável region.
'''

# ==============================================================================