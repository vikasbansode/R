library(keras)

# Set working directiory

setwd("D:\\from E\\Deep_Learning")

# Read Data
data <- read.csv("jena_climate_2009_2016.csv",header = T,sep = ",")
str(data)
head(data)

library(ggplot2)
ggplot(data, aes(x = 1:nrow(data), y = 'T (degC)')) + geom_line()

ggplot(data[1:1440,], aes(x = 1:1440, y = 'T (degC)')) + geom_line()

# Convert data to matrix
data <- data.matrix(data[,-1])

# Normalize data
train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

data <- scale(data, center = mean, scale = std)

# Creating generator

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }
    list(samples, targets)
  }
}


# Preparing the training, validation, and test generators

lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128
train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  step = step,
  batch_size = batch_size
)
val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  step = step,
  batch_size = batch_size
)
test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)
val_steps <- (300000 - 200001 - lookback) / batch_size
test_steps <- (nrow(data) - 300001 - lookback) / batch_size

evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}
evaluate_naive_method()

celsius_mae <- 0.29 * std[[2]]

# Build a model

library(keras)
model <- keras_model_sequential()

model %>%
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

# Compile a model

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

# Fit Model

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
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


######################## Building model with GRU ##################

# Build model
model <- keras_model_sequential()
model %>%
  layer_gru(units = 32, input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_dense(units = 1)

# Compile model

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

# Fit model

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)
