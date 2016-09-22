
# coding: utf-8

# In[4]:

import pandas as pd
import numpy as np
import sys
import os
os.environ["THEANO_FLAGS"] = "device=cpu"
# In[5]:
# nr pietra i ilosc skupien (potrzebne to wczytania wlasciwego pliku)
print(sys.argv)
nr = sys.argv[1]
k = sys.argv[2]
mode = sys.argv[3]
layer_size = int(sys.argv[4])
path = "training_csv/dane_rbf_" + nr + "_" + k + ".csv"
if(mode == "xy"):
    path = "training_csv/dane_rbf_xy_" + nr + "_" + k + ".csv"
dane = pd.read_csv(path, index_col = False)
print(nr)

# In[6]:

n = dane.shape[0]


# In[7]:

train = dane.sample(frac = 0.75)


# In[8]:

train_indeks = np.where(dane.serie == 1)


# In[9]:

test_indeks = np.where(dane.serie == 3)


# In[10]:

train_indeks


# In[11]:

dane_train = dane.loc[train_indeks]


# In[12]:

dane_test = dane.loc[test_indeks]


# In[24]:

Y_train = np.asmatrix(dane_train[["x","y"]])


# In[25]:

Y_test = np.asmatrix(dane_test[["x","y"]])


# In[26]:

X_train = np.asmatrix(dane_train.drop(["x", "y", "z", "X", "serie", "floor"],1))


# In[27]:

X_test = np.asmatrix(dane_test.drop(["x", "y", "z", "X", "serie", "floor"],1))


# In[28]:

print(X_train.shape)
print(X_test.shape)


# In[29]:

import keras


# In[30]:

from keras.models import Sequential
from keras.layers import Dense, Activation, Dropout
model = Sequential()


# In[31]:
if layer_size > 0:
	model.add(Dense(output_dim=layer_size, input_dim=int(k)))
	model.add(Activation("tanh"))
	model.add(Dense(output_dim=2))
	model.add(Activation("linear"))
if layer_size == 0:
	model.add(Dense(output_dim=2, input_dim=int(k)))
	model.add(Activation("linear"))

# In[32]:

model.compile(loss='mse', optimizer="sgd")


# In[33]:

model.fit(X_train, Y_train, nb_epoch=300, batch_size=32, verbose = 2)


# In[ ]:
test_ind = np.random.random_integers(1,X_test.shape[1], 20)
pred = model.predict(X_test)
print(pred.shape)
print(Y_test.shape)
out = np.hstack((Y_test, pred, int(nr)*np.ones((pred.shape[0],1)), int(nr)*np.ones((pred.shape[0],1))))
out_db = pd.DataFrame(out)
if(mode == "xy"):
    out_db.to_csv("output_csv/out_xy_" + k + ".csv", mode='a', header = False)
if(mode != "xy"):
    out_db.to_csv("output_csv/out_" + k + "_" + str(layer_size) + ".csv", mode='a', header = False)
dists = np.linalg.norm(pred-Y_test, axis = 1)
print(dists.shape)
perc80 = np.percentile(dists, 80)
mean_error = np.mean(dists)
median_error = np.median(dists)
max_error = max(dists)
min_error = min(dists)
print(mean_error)


out_stats = np.hstack((int(nr) ,perc80, mean_error, median_error, max_error, min_error, layer_size))
out_stats.shape = (1,7)
out_stats_db = pd.DataFrame(out_stats)
if(mode == "xy"):
    out_stats_db.to_csv("output_csv/outstats_xy_" + k + ".csv", mode='a', header = False)
if(mode != "xy"):
    out_stats_db.to_csv("output_csv/outstats_" + k + "_" + str(layer_size) + ".csv", mode='a', header = False)
