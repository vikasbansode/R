#Clear the workspace

rm(list = ls())

# Set working Directory

setwd("D:\\from E\\Deep_Learning")

# Download Required file

download.file("https://raw.githubusercontent.com/plotly/datasets/master/monthly-milk-production-pounds.csv",destfile = "monthly-milk-production-pounds.csv")

#Load the necessary packages
require(rnn)

#Function to be used later
#Creating Training and Test Data Set
dataset <- function(data){
  x <- y <- c()
  for (i in 1:(nrow(data)-2)){
    x <- append(x, data[i, 2])
    y <- append(y, data[i+1, 2])
  }
  #Creating New DataFrame
  output <- cbind(x,y)
  return(output[1:nrow(output)-1,])
}

# Read data

data <- read.csv("monthly-milk-production-pounds.csv",header = T,sep = ",")
str(data)
head(data)

#Plotting Sequence

plot(data[,2], main = "Monthly Milk Production in Pounds", xlab = "Month",
     ylab = "Pounds",
     lwd = 1.5, col = "cadetblue", type = "l")

#Ploting Histogram

hist(data[,2], main = "Histogram of Monthly Milk Production in Pounds", xlab
     = "Pounds", col = "red")

# Data Partition
#Creating Test and Training Sets
newData <- dataset(data = data)
#Creating Test and Train Data
rows <- sample(1:120, 120)
trainingData <- scale(newData[rows, ])
testData <- scale(newData[-rows, ])

#Max-Min Scaling

x <- trainingData[,1]
y <- trainingData[,2]
train_x <- (x - min(x))/(max(x)-min(x))
train_y <- (y - min(y))/(max(y)-min(y))

#RNN Model
RNN <- trainr(Y = as.matrix(train_x),X = as.matrix(train_y),
              learningrate = 0.04, momentum = 0.1,
              network_type = "rnn", numepochs = 700, hidden_dim = 3)

y_h <- predictr(RNN, as.matrix(train_x))

#Comparing Plots of Predicted Curve vs Actual Curve: Training Data

plot(train_y, col = "blue", type = "l", main = "Actual vs Predicted Curve",
     lwd = 2)
lines(y_h, type = "l", col = "red", lwd = 1)
cat("Train MSE: ", mse(y_h, train_y))

#Test Data

testData <- scale(newData[-rows, ])
x <- testData[,1]
y <- testData[,2]
test_x <- (x - min(x))/(max(x)-min(x))
test_y <- (y - min(y))/(max(y)-min(y))
y_h2 <- predictr(RNN, as.matrix(x))

#Comparing Plots of Predicted Curve vs Actual Curve: Test Data
plot(test_y, col = "blue", type = "l", main = "Actual vs Predicted Curve",
     lwd = 2)
lines(y_h2, type = "l", col = "red", lwd = 1)
cat("Test MSE: ", mse(y_h2, test_y))

########################### RNN with KERAS ######################
library(keras)

# Read Data

data <- read.csv("monthly-milk-production-pounds.csv",header = T,sep = ",")
str(data)
# convert to matrix

data <- as.matrix(data)
dimnames(data) <- NULL
str(data)

# Data Partition

set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training <- data[ind==1,2]
testing <- data[ind==2,2]

# Data partition for Response variables

trainingtarget <- data[ind==1,1]
testtarget <- data[ind==2,1]

# Normalize Data

m <- colMeans(training)
s <- apply(training,2,sd)
training <- scale(training,center = m,scale = s)
testing <- scale(testing,center = m,scale = s)

# build model with keras

model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = 2, output_dim = 120) %>%
  layer_simple_rnn(units = 32)

# Compile model

model %>% compile(loss='mse',
                  optimizer='rmsprop',
                  metrics='mae')

# Fit Model

mymodel <- model %>%
  fit(train_x,
      train_y,
      epochs=100,
      batch_size=32,
      validation_split=0.2)

# Evaluate Model

model %>% evaluate(testing,testtarget)
pred <- model %>% predict(testing)
mean((testtarget-pred)^2)
plot(testtarget,pred)


###################### WITH IMDB Dataset ###############
library(keras)
max_features <- 10000
maxlen <- 500
batch_size <- 32

# Read Data
imdb <- dataset_imdb(num_words = max_features)
str(imdb)
head(imdb)
# Data Partition
c(c(input_train, y_train), c(input_test, y_test)) %<-% imdb
cat(length(input_train), "train sequences\n")
cat(length(input_test), "test sequences")
cat("Pad sequences (samples x time)\n")
input_train <- pad_sequences(input_train, maxlen = maxlen)
input_test <- pad_sequences(input_test, maxlen = maxlen)
cat("input_train shape:", dim(input_train), "\n")
cat("input_test shape:", dim(input_test), "\n")

# Build a model

model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile a model
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

# Fit Model
history <- model %>% fit(
  input_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)

# 11. Evaluate model - Test data

model1 <- model %>% evaluate(input_test,y_test)

# 12. Prediction & confusion Matrix - test Data

prob <- model %>%
  predict_proba(input_test)

pred <- model %>%
  predict_classes(input_test)

table1 <- table(Predicted=pred,Actual=y_test)

cbind(prob,predicted=pred,y_test)

# In this model we lot os missclassification. hence this model is not good
# lets go with lstm


########################## LSTM ####################
# build lstm model

model <- keras_model_sequential() 

model %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile model

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

# Fit Model

history <- model %>% fit(
  input_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)

# 11. Evaluate model - Test data

model1 <- model %>% evaluate(input_test,y_test)

# 12. Prediction & confusion Matrix - test Data

prob <- model %>%
  predict_proba(input_test)

pred <- model %>%
  predict_classes(input_test)

table1 <- table(Predicted=pred,Actual=y_test)
table1

cbind(prob,predicted=pred,y_test)
