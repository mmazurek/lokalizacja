library(data.table)

load("wyniki2.RDA")
wyniki_xgb <- wyniki
load("wyniki_nn.RDA")
wyniki_nn <- wyniki
rm(wyniki)

#####################################################################################################

### najlepsze parametry dla normalnego testowania (uniq == FALSE)

xgb_false <- wyniki_xgb[uniq == FALSE]
nn_false <- wyniki_nn[uniq == FALSE]

# przyznanie "punktów"
setorder(xgb_false, floor)
xgb_score <- xgb_false[,.(med_score = rank(med_err), perc_score = rank(err_80), 
                          mode,
                          nrounds, eta, gamma, max_depth), floor]

xgb_summary_false <- xgb_score[,.(med_score = sum(med_score), perc_score = sum(perc_score)),
                         .(mode, nrounds, eta, gamma, max_depth)]

xgb_summary_false[,total_score := med_score + perc_score]

setorder(nn_false, floor)
nn_score <- nn_false[,.(med_score = rank(med_err), perc_score = rank(err_80), 
                          mode, layer_size, epoch_number), floor]

nn_summary_false <- nn_score[,.(med_score = sum(med_score), perc_score = sum(perc_score)),
                               .(mode, layer_size, epoch_number)]

nn_summary_false[,total_score := med_score + perc_score]

# wybór najlepszych parametrów
setorder(xgb_summary_false, total_score)
setorder(nn_summary_false, total_score)
best_xgb_false <- xgb_summary_false[1,(1:5), with = FALSE]
best_nn_false <- nn_summary_false[1,(1:3), with = FALSE]

setkey(xgb_false, mode, nrounds,  eta, gamma, max_depth)
xgb_false[best_xgb_false]

setkey(nn_false, mode, layer_size, epoch_number)
nn_false[best_nn_false]

### widać że jeśli wspołrzędne powtarzają się w zbiorze uczącym i testowym, to xgb jest duzo lepszy


#############################################################################################################


### najlepsze parametry dla normalnego testowania (uniq == TRUE)

xgb_true <- wyniki_xgb[uniq == TRUE]
nn_true <- wyniki_nn[uniq == TRUE]

# przyznanie "punktów"
setorder(xgb_true, floor)
xgb_score <- xgb_true[,.(med_score = rank(med_err), perc_score = rank(err_80), 
                          mode,
                          nrounds, eta, gamma, max_depth), floor]

xgb_summary_true <- xgb_score[,.(med_score = sum(med_score), perc_score = sum(perc_score)),
                               .(mode, nrounds, eta, gamma, max_depth)]

xgb_summary_true[,total_score := med_score + perc_score]

setorder(nn_true, floor)
nn_score <- nn_true[,.(med_score = rank(med_err), perc_score = rank(err_80), 
                        mode, layer_size, epoch_number), floor]

nn_summary_true <- nn_score[,.(med_score = sum(med_score), perc_score = sum(perc_score)),
                             .(mode, layer_size, epoch_number)]

nn_summary_true[,total_score := med_score + perc_score]

# wybór najlepszych parametrów
setorder(xgb_summary_true, total_score)
setorder(nn_summary_true, total_score)
best_xgb_true <- xgb_summary_true[1,(1:5), with = FALSE]
best_nn_true <- nn_summary_true[1,(1:3), with = FALSE]

setkey(xgb_true, mode, nrounds,  eta, gamma, max_depth)
xgb_true[best_xgb_true]

setkey(nn_true, mode, layer_size, epoch_number)
nn_true[best_nn_true]

### natomiast przy testowaniu na punktach "nowych" sici neuronowe zaczynają wykazywać przewagę