library(cluster)
library(dplyr)
library(stringi)
library(parallel)
library(xgboost)
library(data.table)
library(PythonInR)

# exporting to python
pyExit()
PythonInR:::pyConnectWinDll(dllName="python34.dll", dllDir='C:/Users/Bartek.bartek-pc/Anaconda3/envs/python343',
                            majorVersion=3, pythonHome='C:/Users/Bartek.bartek-pc/Anaconda3/envs/python343',  pyArch="64bit")
pyVersion()
pyImport("numpy", as = "np")
pyImport("pandas", as = "pd")
pyImport("sys")
pyImport("os")


# funkcje
data_load <- function(mode, k = 70){
  stopifnot(mode %in% c("rbf", "raw"))
  dane <- vector(mode = "list", 6)
  dane_AP <- read.csv("source_csv/wifi_AP_MiNI.csv", sep = ";")
  
  mini_aps <- dane_AP$BSSID
  mini_aps <- stri_replace_all_fixed(str = mini_aps, pattern = "-", replacement = "")
  columns <- c(colnames(dane)[1:2],colnames(dane)[stri_sub(colnames(dane), from = 14) %in% mini_aps])
  
  if(mode == "rbf"){
    for(i in 1:6){
      floor <- i-1
      dane[[i]] <- fread(paste0("new_training_csv/dane_rbf_", floor, "_", k,  ".csv"))
    }
  }
  if(mode == "raw"){
    for(i in 1:6){
      floor <- i-1
      dane[[i]] <- fread(paste0("source_csv/dane_filtered_", floor, ".csv"))
      columns <- c(colnames(dane[[i]])[1:2],colnames(dane[[i]])[stri_sub(colnames(dane[[i]]), from = 14) %in% mini_aps])
      dane[[i]] <- dane[[i]] %>% select_( .dots = columns)
    }
  }
  
  return(dane)
}

make_train_test <- function(dane){
  k <- ncol(dane[[1]])
  test_set <- vector("list", 6)
  test_labels_x <- vector("list", 6)
  test_labels_y <- vector("list", 6)
  train_set <- vector("list", 6)
  train_labels_x <- vector("list", 6)
  train_labels_y <- vector("list", 6)
  for(floor in 0:5){
    i <- floor + 1
    test_set[[i]] <- dane[[i]][serie == 3, 7:k, with = FALSE]
    train_set[[i]] <- dane[[i]][serie == 1, 7:k, with = FALSE]
    test_labels_x[[i]] <- dane[[i]][serie==3]$x
    test_labels_y[[i]] <- dane[[i]][serie==3]$y
    train_labels_x[[i]] <- dane[[i]][serie==1]$x
    train_labels_y[[i]] <- dane[[i]][serie==1]$y
  }
  return(list(test_set = test_set,
              test_labels_x = test_labels_x ,
              test_labels_y = test_labels_y,
              train_set = train_set,
              train_labels_x = train_labels_x,
              train_labels_y = train_labels_y))
}

test_nn <- function(dane, 
                    layer_size = 50,
                    epoch_number = 150, k){
  
  err_table <- data.table(floor = integer(),
                          serie = integer(),
                          errs = numeric())
  tt_data <- make_train_test(dane = dane)
  for(floor in 1:6){
    train_set <- as.matrix(tt_data$train_set[[floor]])
    dimnames(train_set)[[1]] <- 1:nrow(train_set)
    test_set <- as.matrix(tt_data$test_set[[floor]])
    dimnames(test_set)[[1]] <- 1:nrow(test_set)
    train_labels_x <- tt_data$train_labels_x[[floor]]
    train_labels_y <- tt_data$train_labels_y[[floor]]
    test_labels_x <- tt_data$test_labels_x[[floor]]
    test_labels_y <- tt_data$test_labels_y[[floor]]
    n <- ncol(train_set)
    pySet("X_train", train_set, useNumpy = TRUE)
    pySet("X_test", test_set, useNumpy = TRUE)
    Y_train <- cbind(train_labels_x,train_labels_y)
    dimnames(Y_train)[[1]] <- 1:nrow(Y_train)
    pySet("Y_train", Y_train, useNumpy = TRUE)
    Y_test <- cbind(test_labels_x,test_labels_y)
    dimnames(Y_test)[[1]] <- 1:nrow(Y_test)
    pySet("Y_test", Y_test, useNumpy = TRUE)
    pySet("k", k, useNumpy = TRUE)
    pySet("layer_size", layer_size, useNumpy = TRUE)
    pySet("epoch_number", epoch_number, useNumpy = TRUE)
    pyExecfile("rbf_floor_r.py")
    try(pred <- pyGet("pred"), silent = TRUE)
    try(pred <- pyGet("pred"), silent = TRUE)
    errs <- sqrt(rowSums(pred - Y_test)^2)
    tmp_df <- data.table(floor = rep(floor-1, length(errs)),
                         series = rep(1, length(errs)),
                         errs = errs)
    err_table <- rbindlist(list(err_table, tmp_df))
  }
  return(err_table)
}

 

params <- expand.grid(
  layer_size = c(0,20,30,50,70,80),
  epoch_number = 450
)

wyniki <- data.table(
  mode = character(),
  floor = integer(),
  med_err = numeric(),
  err_80 = numeric(),
  mean_err = numeric(),
  total_med = numeric(),
  total_mean = numeric(),
  total_80 = numeric(),
  cluster_no = integer()
)


start_t <- Sys.time()
for(mode in c("rbf")){
  for(k in c(20,40,60,80,100)){
  dane <- data_load(mode, k)
    for(i in 1:nrow(params)){
      print(c(k,i))
      out <- test_nn(dane = dane, 
                     layer_size = params$layer_size[i],
                     epoch_number = params$epoch_number[i], k)
      tmp <- cbind(mode = rep(mode, nrow(out)),
                   out)
      tmp[,total_med := median(errs)]
      tmp[,total_mean := mean(errs)]
      tmp[,total_80 := quantile(errs, 0.8)]
      tmp[,cluster_no := k]
      tmp <- tmp[, .(med_err = median(errs), err_80 = quantile(errs, probs = 0.8), mean_err = mean(errs), 
                     total_med = total_med[1], total_mean = total_mean[1], total_80 = total_80[1], cluster_no = cluster_no[1]),.(mode, floor)]
      wyniki <- rbind(wyniki, tmp)
    }
  save(wyniki, file = paste0("wyniki_nn_final_new_", k, ".RDA"))
  }
}
end_t <- Sys.time()
end_t - start_t 
save(wyniki, file = "wyniki_nn_final_new.RDA")
setorder(wyniki, floor, med_err)
