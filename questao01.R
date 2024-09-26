
jorel <- 7
juju <- 18

moeda <- c('CARA','COROA')

while(jorel != 0 & juju != 0){
  jogada <- sample(moeda,1,replace = TRUE)
  if (jogada == "CARA"){
    jorel <- jorel - 1
    juju <- juju + 1
  }else{
    jorel <- jorel + 1
    juju <- juju - 1
  }
}

if(jorel == 0){
  print("Vovó Juju ganhou!")
}else{
  print("Jorel ganhou!")
}

resultados <- c()

for(i in 1:10000){
  jorel <- 7
  juju <- 18
  
  while(jorel != 0 & juju != 0){
    jogada <- sample(moeda,1,replace = TRUE)
    if (jogada == "CARA"){
      jorel <- jorel - 1
      juju <- juju + 1
    }else{
      jorel <- jorel + 1
      juju <- juju - 1
    }
  }
  
  if(jorel == 0){
    resultados <- c(resultados,"Vovó Juju")
  }else{
    resultados <- c(resultados,"Jorel")
  }
}

mean(resultados == "Vovó Juju")
mean(resultados == "Jorel")
