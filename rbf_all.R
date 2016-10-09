library(cluster)
library(dplyr)
library(stringi)
library(parallel)

cl <- makeCluster(4)

dane_filtered <- read.csv("new_source_csv/mini2.csv")
dane_filtered[is.na(dane_filtered)] <- -120
for(k_med in c(20,40,60,80,100)){
  
  kmed <- kmeans(dane_filtered[dane_filtered$serie == 1,-c(1:6)], centers = k_med, nstart = 10)
  #kmed <- clara(dane_filtered[dane_filtered$serie == 1,-c(1:6)], k = k_med)
  #sigma <- kmed$clusinfo[,3]
  #neurons <- kmed$medoids
  kmed$withinss
  sigma <- sqrt(kmed$withinss/kmed$size)
  neurons <- kmed$centers
  
  hidden_layer <- function(x, beta, neurons){
    ####
    # x - wektor postaci (x,y, ....), gdzie x,y - zmienne zależne, a ... - zmienne niezależne
    # beta - wektor wag do rbf
    # neurons - srodki uzyskane z clusteringu
    ####
    input <- as.numeric(x[-c(1:6)])
    pos <- as.numeric(x[c(1:6)])
    dist <- apply(neurons, MARGIN = 1, FUN = function(t) { sum((input - t)^2)})
    out <-  exp(-beta * dist) 
    return(c(pos, out))
  }
  
  hiddenlayer <- parApply(cl = cl, X = dane_filtered, MARGIN = 1, FUN = hidden_layer, beta = 1/(2*sigma^2), neurons = kmed$centers)
  hiddenlayer <- data.frame(t(hiddenlayer))
  
  
  colnames(hiddenlayer)[c(1:6)] <- colnames(dane_filtered)[c(1:6)]
  
  write.csv(x = hiddenlayer, file = paste0("new_training_csv/dane_rbf_", "all", "_", k_med,".csv"), row.names = FALSE)
}