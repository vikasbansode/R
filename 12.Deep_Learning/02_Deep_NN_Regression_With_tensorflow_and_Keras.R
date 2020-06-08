######################### DEEP NEURAL NETWORK WITH TENSORFLOW & KERAS  ###################

# 1.Set working Direct

setwd("D:\\from E\\Deep_Learning")


# 2.Import required libraries

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

# 3.Read Data

data("BostonHousing")
data <- BostonHousing

# 4.Explore Data

str(data)

# 5. Convert factor variable to numeric because NN we need numeric data

data %<>% mutate_if(is.factor,as.numeric)
str(data)

# 6. Build Neural Network 

n <- neuralnet(medv ~ crim+zn+indus+chas+nox+age+dis+rad+tax+ptratio+b+lstat,data = data,
               hidden = c(10,5),
               linear.output = F,
               lifesign = 'full',
               rep = 1)

# 7. Visualize model

plot(n,col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill='lightblue')

# 8. Convert Data into Matrix as Deep neural network with keras and tensorflow only accept matrix

data <- as.matrix(data)

# 9. Remove dimension names as keras does not support for dimnames in matrix

dimnames(data) <- NULL

# 10. Data Partition

set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training <- data[ind==1,1:13]
testing <- data[ind==2,1:13]

# 11. for Deep learning model we have seperate dependent and independent variables

trainingtarget <- data[ind==1,14]
testtarget <- data[ind==2,14]

# 12. Normalize Data

m <- colMeans(training)
s <- apply(training,2,sd)
training <- scale(training,center = m,scale = s)
testing <- scale(testing,center = m,scale = s)

# 13. Build Model

model <- keras_model_sequential()
model %>%
  layer_dense(units = 5,activation = 'relu',input_shape = c(13)) %>%
  layer_dense(units = 1)

# 14. Compile model

model %>% compile(loss='mse',
                  optimizer='rmsprop',
                  metrics='mae')
# 15. Fit Model

mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)

# in the Graph, Green dots are validation loss or mean square error and blue gots are for mean square error based on 
# the data that is being used for developing the model

# 15. Evaluate

model %>% evaluate(testing,testtarget)
pred <- model %>% predict(testing)
mean((testtarget-pred)^2)
plot(testtarget,pred)

# we see error is quit high so this model need significant improvement

############################ FINE TUNE MODEL ##########################
############################ FINE TUNE MODEL ##########################
############################ FINE TUNE MODEL ##########################

# 1. Fine tunning

model <- keras_model_sequential()
model %>%
  layer_dense(units = 10,activation = 'relu',input_shape = c(13)) %>%
  layer_dense(units = 5,activation = 'relu') %>%
  layer_dense(units = 1)

summary(model)

# Compile model
model %>% compile(loss='mse',
                  optimizer='rmsprop',
                  metrics='mae')
# Fit Model
mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)

# Evaluate
model %>% evaluate(testing,testtarget)
pred <- model %>% predict(testing)
mean((testtarget-pred)^2)
plot(testtarget,pred)

# in the above model we see lot of overfitting so we need more fine tuning
############### MORE FINE TUNNING  adding more layers #############
############### MORE FINE TUNNING  adding more layers #############
############### MORE FINE TUNNING  adding more layers #############

model <- keras_model_sequential()
model %>%
  layer_dense(units = 100,activation = 'relu',input_shape = c(13)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50,activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 20,activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

summary(model)

# 2.Compile model
model %>% compile(loss='mse',
                  optimizer='rmsprop',
                  metrics='mae')

 #OR
model %>% compile(loss='mse',
                  optimizer=optimizer_rmsprop(lr=0.001),
                  metrics='mae')
# 3.Fit Model

mymodel <- model %>%
  fit(training,
      trainingtarget,
      epochs=100,
      batch_size=32,
      validation_split=0.2)

# 4.Evaluate

model %>% evaluate(testing,testtarget)
pred <- model %>% predict(testing)
mean((testtarget-pred)^2)
plot(testtarget,pred)

# This model looks good as compared to above
