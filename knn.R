library(cluster)
library(dplyr)
library(stringi)
library(parallel)
library(xgboost)
library(FNN)
cl <- makeCluster(4)
#### no_rbf ####
dane <- read.csv("new_source_csv/mini2.csv")
dane[is.na(dane)] <- -120


train_set <- dane %>% filter(serie == 1) %>% select(-c(1:6))
train_labels <- dane[dane$serie == 1, c(3,4)]
test_set <- dane %>% filter(serie == 3) %>% select(-c(1:6))
test_labels <- dane[dane$serie == 3, c(3,4)]

for(i in c(5,8,10,15,20,30,50,80,100)){
  knn_model <- get.knnx(data = train_set, query = test_set, algorithm = "kd_tree", k = i)
  
  #knn_model$nn.index
  
  get_location <- function(labels,inds){
    colMeans(labels[inds,])
  }
  
  
  pred <- t(apply(knn_model$nn.index, MARGIN = 1, get_location, labels = train_labels))
  
  
  errs <- sqrt((pred[,1] - test_labels[,1])^2 + (pred[,2] - test_labels[,2])^2)
  
  print(i)
  print(median(errs))
  print(mean(errs))
  print(quantile(errs, 0.8))
}

#### rbf ####

dane <- read.csv("new_training_csv/dane_rbf_0_20.csv")


train_set <- dane %>% filter(serie == 1) %>% select(-c(1:6))
train_labels <- dane[dane$serie == 1, c(3,4)]
test_set <- dane %>% filter(serie == 3) %>% select(-c(1:6))
test_labels <- dane[dane$serie == 3, c(3,4)]

for(i in c(5,8,10,15,20,30,50,80,100)){
  knn_model <- get.knnx(data = train_set, query = test_set, algorithm = "kd_tree", k = i)
  
  #knn_model$nn.index
  
  get_location <- function(labels,inds){
    colMeans(labels[inds,])
  }
  
  
  pred <- t(apply(knn_model$nn.index, MARGIN = 1, get_location, labels = train_labels))
  
  
  errs <- sqrt((pred[,1] - test_labels[,1])^2 + (pred[,2] - test_labels[,2])^2)
  
  print(i)
  print(median(errs))
  print(mean(errs))
  print(quantile(errs, 0.8))
}
