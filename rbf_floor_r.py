
# coding: utf-8

# In[4]:


os.environ["THEANO_FLAGS"] = "device=cpu"
# In[5]:
# X_train, X_test, Y_train, Y_test, k, layersize

# In[28]:


# In[29]:

import keras


# In[30]:

from keras.models import Sequential
from keras.layers import Dense, Activation, Dropout
model = Sequential()


# In[31]:
if layer_size > 0:
	model.add(Dense(output_dim=int(layer_size), input_dim=int(k)))
	model.add(Activation("tanh"))
	model.add(Dense(output_dim=2))
	model.add(Activation("linear"))
if layer_size == 0:
	model.add(Dense(output_dim=2, input_dim=int(k)))
	model.add(Activation("linear"))

# In[32]:

model.compile(loss='mse', optimizer="sgd")


# In[33]:

model.fit(X_train, Y_train, nb_epoch=int(epoch_number), batch_size=32, verbose = 0)


# In[ ]:
pred = model.predict(X_test)

