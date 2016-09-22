library(cluster)
library(dplyr)
library(stringi)
library(parallel)

dane <- read.csv("source_csv/mini.csv")
dane_AP <- read.csv("source_csv/wifi_AP_MiNI.csv", sep = ";")
mini_aps <- dane_AP$BSSID
mini_aps <- stri_replace_all_fixed(str = mini_aps, pattern = "-", replacement = "")
columns <- c(colnames(dane)[c(2,4,5,6,7)],colnames(dane)[stri_sub(colnames(dane), from = 14) %in% mini_aps])
dane <- dane %>% select_( .dots = columns)

write.csv(dane, file = "new_source_csv/mini2.csv")


for(i in 0:5){
  dane_filtered <- dane %>% filter(floor == i)
  write.csv(dane_filtered, paste0("new_source_csv/dane_filtered_", i, ".csv"))
}
