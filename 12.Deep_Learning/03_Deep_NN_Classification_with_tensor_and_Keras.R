######################### DEEP NEURAL NETWORK Multilayer Perceptron WITH TENSORFLOW & KERAS  ###################
# 1.Set working Direct

setwd("D:\\from E\\Deep_Learning")

# 2.Import required libraries

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

# 3.Read Data

data <- read.csv("CTG.csv",header = TRUE,sep = ",")

# 4.Explore Data

str(data)

# 3. Convert Data into Matrix as Deep neural network with keras and tensorflow only accept matrix

data <- as.matrix(data)

# 4. Remove dimension names as keras does not support for dimnames in matrix

dimnames(data) <- NULL

# 5. Normalize

data[,1:21] <- normalize(data[,1:21])
data[,22] <- as.numeric(data[,22])-1
summary(data)

# 6. Data Partition

set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training <- data[ind==1,1:21]
testing <- data[ind==2,1:21]

# 7. for Deep learning model we have seperate dependent and independent variables

trainingtarget <- data[ind==1,22]
testtarget <- data[ind==2,22]

# 8. One Hot Encoding

trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)

# 9. Build a Model

model <- keras_model_sequential()

model %>%
  layer_dense(units = 8,activation = 'relu',input_shape = c(21)) %>%
  layer_dense(units = 3,activation = 'softmax')
  
summary(model)

# 9. Compile model

model %>% compile(loss='categorical_crossentropy',
                  optimizer='adam',
                  metrics='accuracy')

# since we have 3 categories for response variables, we are using "categorical_crossentropy" loss function.
# in those situations our response variables have only two categories there we can use "binary_crossentropy" loss function

# 10. Fit Model

mymodel <- model %>%
  fit(training,
      trainLabels,
      epochs=200,
      batch_size=32,
      validation_split=0.2)

# in the Graph, Green dots are validation loss or mean square error and blue gots are for mean square error based on 
# the data that is being used for developing the model
plot(mymodel)

# 11. Evaluate model - Test data

model1 <- model %>% evaluate(testing,testLabels)

# 12. Prediction & confusion Matrix - test Data

prob <- model %>%
          predict_proba(testing)

pred <- model %>%
          predict_classes(testing)

table1 <- table(Predicted=pred,Actual=testtarget)

cbind(prob,pred,testtarget)

# This model is doing good job but there still missmatch. lets fine tune model

############################ FINE TUNE MODEL ##########################
############################ FINE TUNE MODEL ##########################
############################ FINE TUNE MODEL ##########################

# 1. Fine Tunning -- Fine tunning is nothing but adjusting units or hidden layers

model <- keras_model_sequential()

model %>%
  layer_dense(units = 50,activation = 'relu',input_shape = c(21)) %>%
  layer_dense(units = 3,activation = 'softmax')

summary(model)

# 9. Compile model

model %>% compile(loss='categorical_crossentropy',
                  optimizer='adam',
                  metrics='accuracy')

# since we have 3 categories for response variables, we are using "categorical_crossentropy" loss function.
# in those situations our response variables have only two categories there we can use "binary_crossentropy" loss function

# 10. Fit Model

mymodel <- model %>%
  fit(training,
      trainLabels,
      epochs=200,
      batch_size=32,
      validation_split=0.2)

# in the Graph, Green dots are validation loss or mean square error and blue gots are for mean square error based on 
# the data that is being used for developing the model
plot(mymodel)

# 11. Evaluate model - Test data

model2 <- model %>% evaluate(testing,testLabels)

# 12. Prediction - test Data

prob <- model %>%
  predict_proba(testing)

pred <- model %>%
  predict_classes(testing)

# 13. confusion Matrix - test Data

table2 <- table(Predicted=pred,Actual=testtarget)

cbind(prob,pred,testtarget)


#####################################################
# Compare previous model to current one, to check which one is best.

model1
table1

model2
table2


# we still required fine tunning as there is only slight changes

############################ MORE FINE TUNE MODEL ##########################
############################ MORE FINE TUNE MODEL ##########################
############################ MORE FINE TUNE MODEL ##########################

# 1. Fine Tunning -- Fine tunning is nothing but adjusting units or hidden layers

model <- keras_model_sequential()

model %>%
  layer_dense(units = 50,activation = 'relu',input_shape = c(21)) %>%
  layer_dense(units=8,activation = 'relu') %>%
  layer_dense(units = 3,activation = 'softmax')

summary(model)

# 9. Compile model

model %>% compile(loss='categorical_crossentropy',
                  optimizer='adam',
                  metrics='accuracy')

# since we have 3 categories for response variables, we are using "categorical_crossentropy" loss function.
# in those situations our response variables have only two categories there we can use "binary_crossentropy" loss function

# 10. Fit Model

mymodel <- model %>%
  fit(training,
      trainLabels,
      epochs=200,
      batch_size=32,
      validation_split=0.2)

# in the Graph, Green dots are validation loss or mean square error and blue gots are for mean square error based on 
# the data that is being used for developing the model
plot(mymodel)

# 11. Evaluate model - Test data

model3 <- model %>% evaluate(testing,testLabels)

# 12. Prediction - test Data

prob <- model %>%
  predict_proba(testing)

pred <- model %>%
  predict_classes(testing)

# 13. confusion Matrix - test Data

table3 <- table(Predicted=pred,Actual=testtarget)

cbind(prob,pred,testtarget)


#####################################################
# Compare previous model to current one, to check which one is best.

model1
table1

model2
table2

model3
table3

# Conclusion second model is best here compared to both 1st and 3rd one
