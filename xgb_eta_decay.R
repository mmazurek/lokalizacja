library(data.table)
library(stringi)
library(xgboost)

##### Potrzebne funkcje

data_load <- function(mode, k = 20, missing = TRUE){
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
      if(!missing){
        dane[[i]][is.na(dane[[i]])] <- -120
      }
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


floor <- 3
for(floor in 1:6){
# normal
dane <- data_load("raw", 80, FALSE)
tt_data <- make_train_test(dane = dane)
gamma <- 1
max_depth <- 10
nrounds <- 650
eta <- 0.01
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
normal_mean <- median(errs)

gamma <- 2
max_depth <- 10
nrounds <- 650

#eta_decay
dane <- data_load("raw", 80, TRUE)
tt_data <- make_train_test(dane = dane)

dtrain_x <- xgb.DMatrix(data.matrix(tt_data$train_set[[floor]]*1.0), label = tt_data$train_labels_x[[floor]], missing = NaN)
dtrain_y <- xgb.DMatrix(data.matrix(tt_data$train_set[[floor]]*1.0), label = tt_data$train_labels_y[[floor]], missing = NaN)

dtest_x <- xgb.DMatrix(data.matrix(tt_data$test_set[[floor]]*1.0), label = tt_data$test_labels_x[[floor]], missing = NaN)
dtest_y <- xgb.DMatrix(data.matrix(tt_data$test_set[[floor]]*1.0), label = tt_data$test_labels_x[[floor]], missing = NaN)

# ###
# # advanced: start from a initial base prediction
# #
# print('start running example to start from a initial prediction')
# # train xgboost for 1 round
# param <- list(max_depth=2, eta=1, nthread = 2, silent=1, objective='binary:logistic')
# bst <- xgb.train(param, dtrain, 1, watchlist)
# # Note: we need the margin value instead of transformed prediction in set_base_margin
# # do predict with output_margin=TRUE, will always give you margin values before logistic transformation
# ptrain <- predict(bst, dtrain, outputmargin=TRUE)
# ptest  <- predict(bst, dtest, outputmargin=TRUE)
# # set the base_margin property of dtrain and dtest
# # base margin is the base prediction we will boost from
# setinfo(dtrain, "base_margin", ptrain)
# setinfo(dtest, "base_margin", ptest)
# 
# print('this is result of boost from initial prediction')
# bst <- xgb.train(params = param, data = dtrain, nrounds = 1, watchlist = watchlist)

eta = 0.2
x_boost_init <- xgboost(data = dtrain_x,
                   nrounds = 10, 
                   booster = "gbtree", 
                   objective = "reg:linear",
                   verbose = 0,
                   eta = eta,
                   gamma = gamma,
                   max_depth = 16)

y_boost_init <- xgboost(data = dtrain_y, 
                   nrounds = 10, 
                   booster = "gbtree", objective = "reg:linear",
                   verbose = 0,
                   eta = eta,
                   gamma = gamma,
                   max_depth = 16)

x_pred_init <- predict(x_boost_init, dtest_x)
y_pred_init <- predict(y_boost_init, dtest_y)

setinfo(dtrain_x, "base_margin", predict(x_boost_init, dtrain_x))
setinfo(dtrain_y, "base_margin", predict(y_boost_init, dtrain_y))
setinfo(dtest_x, "base_margin", x_pred_init)
setinfo(dtest_y, "base_margin", y_pred_init)

eta = 0.01

x_boost <- xgboost(data = dtrain_x,
                        nrounds = 300, 
                        booster = "gbtree", 
                        objective = "reg:linear",
                        verbose = 0,
                        eta = eta,
                        gamma = gamma,
                        max_depth = 6)

y_boost <- xgboost(data = dtrain_y, 
                        nrounds = 300, 
                        booster = "gbtree", 
                        objective = "reg:linear",
                        verbose = 0,
                        eta = eta,
                        gamma = gamma,
                        max_depth = 6)

x_pred <- predict(x_boost, dtest_x)
y_pred <- predict(y_boost, dtest_y)

errs <- sqrt( (tt_data$test_labels_x[[floor]] - x_pred)^2 + (tt_data$test_labels_y[[floor]] - y_pred)^2)
decay_mean <- median(errs)
decay_mean


print(c(normal_mean, decay_mean))
}