library(cluster)
library(dplyr)
library(stringi)
library(parallel)
library(xgboost)
cl <- makeCluster(4)

dane <- read.csv("training_csv/dane_pietra_70.csv")
table(dane$z,dane$floor)
#dane <- dane[dane$floor %in% c(0,4,8,12,16,19.5),]
#dane$floor <- ceiling(dane$z/4)
n <- nrow(dane)
#dane$floor <- as.factor(dane$floor)
train_ind <- sample(1:n, size = floor(0.75*n), replace = FALSE)
train_set <- dane[train_ind,5:74]
test_set <- dane[-train_ind,5:74]
train_labels <- dane[train_ind, 4]
test_labels <- dane[-train_ind, 4]


model_xgb <- xgboost(data = as.matrix(train_set), label = train_labels, booster = "gbtree",
                     objective = "", num_class = 6, nrounds = 20)

model_xgb2 <- xgboost(data = xgb.DMatrix(data = as.matrix(train_set), label = train_labels), booster = "gbtree",
                     objective = "reg:linear", nrounds = 20)


predictions <- predict(model_xgb, as.matrix(test_set))
predictions2 <- predict(model_xgb2, as.matrix(test_set))
predictions3 <- predict(model_xgb2, xgb.DMatrix(as.matrix(cbind(test_labels, test_set))[,-1]))
predictions4 <- predict(model_xgb2, xgb.DMatrix(as.matrix(test_set), label = test_labels))

all(predictions4 == predictions3)
all(predictions2 == predictions3)
all(predictions2 == predictions)

mean(predictions == test_labels)
