setwd("C:/Users/arand_000.Pavel/Downloads")
best <- read.csv(file="Best.csv", header=T, sep=";")
best <- data.frame(name=best[,2],pass=best[,3],goal=best[,4])
best <- subset(best, best$name!="")

champ <- read.csv(file="Champ.csv", header=T, sep=";")
mega <- data.frame(name=people$name,pass=rep(50,29),goal=rep(50,29),all=rep(2,29))
mega <- mega[c(2,5,6,7,8,14,15,16,17,21,24,25,26,28,29),]
row.names(mega) <- seq(1:length(mega$name))


for (i in 1:(length(champ$name))) {
  if(!is.na(match(champ[i,1],mega$name))) {
  k=match(champ[i,1],mega$name)
  mega[k,2] <- (mega[k,2]*mega[k,4]/100+champ[i,2])/(mega[k,4]+champ[i,2]+champ[i,3])*100
  mega[k,3] <- (mega[k,3]*mega[k,4]/100+champ[i,3])/(mega[k,4]+champ[i,2]+champ[i,3])*100
  mega[k,4] <- mega[k,4]+champ[i,2]+champ[i,3]
  }
}

for (i in 1:(length(best$name))) {
  if(!is.na(match(best[i,1],mega$name))) {
    k=match(best[i,1],mega$name)
    mega[k,2] <- (mega[k,2]*mega[k,4]/100+best[i,2])/(mega[k,4]+best[i,2]+best[i,3])*100
    mega[k,3] <- (mega[k,3]*mega[k,4]/100+best[i,3])/(mega[k,4]+best[i,2]+best[i,3])*100
    mega[k,4] <- mega[k,4]+best[i,2]+best[i,3]
  }
}

champ <- read.csv(file="Stol.csv", header=T, sep=";")

for (i in 1:(length(champ$name))) {
  if(!is.na(match(champ[i,1],mega$name))) {
    k=match(champ[i,1],mega$name)
    mega[k,2] <- (mega[k,2]*mega[k,4]/100+champ[i,2])/(mega[k,4]+champ[i,2]+champ[i,3])*100
    mega[k,3] <- (mega[k,3]*mega[k,4]/100+champ[i,3])/(mega[k,4]+champ[i,2]+champ[i,3])*100
    mega[k,4] <- mega[k,4]+champ[i,2]+champ[i,3]
  }
}



