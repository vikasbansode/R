rm(list = ls())

library(keras)
library(dplyr)
library(reticulate)

devtools::install_github("rstudio/reticulate")

setwd("D:\\from E\\Deep_Learning")

dataset_train <- read.csv("Google_Stock_Price_Train.csv",header = T,sep = ",")
class(dataset_train)

training_set <- dataset_train[,c(2)]

training_set <- dataset_train[,c(2),drop=FALSE]
class(training_set)

training_set <- dplyr::pull(dataset_train,c(2))

class(training_set)

head(training_set,20)

# Reshaping input
step <- 59

training_set <- c(training_set,replicate(step,tail(training_set,1)))

# Normalize data

training_set_scaled <- (training_set - min(training_set))/(max(training_set)-min(training_set))
head(training_set_scaled)

training_set_scaled <- t(training_set_scaled)
dim(training_set_scaled)

# Creating x_train and y_train

x_train <- NULL
y_train <- NULL

for (i in 1:60){
  s <- i-1+step
  x_train <- rbind(x_train,training_set_scaled[i:s])
  y_train <- rbind(y_train,training_set_scaled[s+1])
} 


cbind(head(x_train),head(y_train))

View(x_train)
View(y_train)


x_train = array(x_train, dim=c(60, step,1))
View(x_train)

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1), activation="relu") %>%  
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>%  
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()

model %>% fit(x_train,y_train, epochs=50, batch_size=32, validation_split=0.2)

model %>% evaluate(x_train,y_train)

pred <- model %>% predict(training_set_scaled)
mean((testtarget-pred)^2)
plot(testtarget,pred)