library(data.table)
library(stringi)
library(xgboost)

##### Potrzebne funkcje

data_load <- function(mode, k = 70){
  stopifnot(mode %in% c("rbf", "raw"))
  dane <- vector(mode = "list", 6)
  
  if(mode == "rbf"){
    for(i in 1:6){
      floor <- i-1
      dane[[i]] <- fread(paste0("new_training_csv/dane_rbf_", floor, "_", k,  ".csv"))
    }
  }
  if(mode == "raw"){
    for(i in 1:6){
      floor <- i-1
      dane[[i]] <- fread(paste0("new_source_csv/dane_filtered_", floor, ".csv"))
      dane[[i]][is.na(dane[[i]])] <- -120
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

test_xgb <- function(dane, nrounds = 80,
                     eta = 0.3, gamma = 1, max_depth = 6,
                     subsample = 1,
                     colsample_bytree = 1){
  err_table <- data.table(floor = integer(),
                     errs = numeric())
  tt_data <- make_train_test(dane = dane)
  for(floor in 1:6){
    x_boost <- xgboost(data = data.matrix(tt_data$train_set[[floor]]*1.0), 
                       label = tt_data$train_labels_x[[floor]], 
                       nrounds = nrounds, 
                       booster = "gbtree", 
                       objective = "reg:linear",
                       verbose = 0,
                       eta = eta,
                       gamma = gamma,
                       max_depth = max_depth)
    
    y_boost <- xgboost(data = data.matrix(tt_data$train_set[[floor]]*1.0), 
                       label = tt_data$train_labels_y[[floor]], 
                       nrounds = nrounds, 
                       booster = "gbtree", objective = "reg:linear",
                       verbose = 0,
                       eta = eta,
                       gamma = gamma,
                       max_depth = max_depth)
    
    x_pred <- predict(x_boost, data.matrix(tt_data$test_set[[floor]]*1.0))
    y_pred <- predict(y_boost, data.matrix(tt_data$test_set[[floor]]*1.0))
    errs <- sqrt( (tt_data$test_labels_x[[floor]] - x_pred)^2 + (tt_data$test_labels_y[[floor]] - y_pred)^2)
    tmp_df <- data.table(floor = rep(floor-1, length(errs)),
                         errs = errs)
    err_table <- rbindlist(list(err_table, tmp_df))

  }
  return(err_table)
}
#####

params <- expand.grid(
  nrounds = c(250, 450),
  eta = c(0.05),
  gamma = c(0.5, 0.2),
  max_depth = c(4, 6, 8)
)
params$eta[2]

wyniki <- data.table(
  mode = character(),
  floor = integer(),
  med_err = numeric(),
  err_80 = numeric(),
  mean_err = numeric(),
  nrounds = integer(),
  eta = numeric(),
  gamma = numeric(),
  max_depth = integer(),
  k = integer(),
  total_med = numeric(),
  total_mean = numeric(),
  total_80 = numeric()
)

start_t <- Sys.time()
for(mode in c("raw", "rbf")){
  for(k in c(20,40,60,80,100)){
    if(mode == "raw" & k > 20) next
    dane <- data_load(mode, k)
    for(i in 1:nrow(params)){
      print(paste0(mode, ", ", k))
      print(paste0(i, " of ", nrow(params)))
      out <- test_xgb(dane,  
                      nrounds = params$nrounds[i],
                      eta = params$eta[i],
                      max_depth = params$max_depth[i],
                      subsample = params$subsample[i],
                      colsample_bytree = params$colsample_bytree[i])
      tmp <- cbind(mode = rep(mode, nrow(out)),
                          out)
      tmp[,total_med := median(errs)]
      tmp[,total_mean := mean(errs)]
      tmp[,total_80 := quantile(errs, 0.8)]
      tmp <- tmp[, .(med_err = median(errs), err_80 = quantile(errs, probs = 0.8), mean_err = mean(errs),
                     total_med = total_med[1], total_mean = total_mean[1], total_80 = total_80[1]),.(mode, floor)]
      tmp[, k:= k]
      wyniki <- rbind(wyniki, cbind(tmp, params[rep(i,nrow(tmp)),]))
    }
  }
}
end_t <- Sys.time()
end_t - start_t
save(wyniki, "wyniki_xgb.RDA")

