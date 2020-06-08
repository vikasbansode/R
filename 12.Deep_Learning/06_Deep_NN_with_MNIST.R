
# 1. set working directory

setwd("D:\\from E\\Deep_Learning\\vid_6_images")

# 2. Read MNIST Data
library(keras)
mnist <- dataset_mnist()
str(mnist)

# 3. store in to trainx

trainx <- mnist$train$x
trainy <- mnist$train$y

testx <- mnist$test$x
testy <- mnist$test$y

table(mnist$train$y,mnist$train$y)
table(mnist$test$y,mnist$test$y)

# 4. Plot images

par(mfrow=c(3,3))
for(i in 1:9) plot(as.raster(trainx[i,,],max=255))
par(mfrow=c(1,1))

trainx[2,,]
hist(trainx[1,,])

# 5. Five

a <- c(1, 12, 36, 48, 66, 101, 133, 139, 146)
par(mfrow=c(3,3))
for(i in a) plot(as.raster(trainx[i,,],max=255))
par(mfrow=c(1,1))

# 6.Reshape & Rescale

trainx <- array_reshape(trainx,c(nrow(trainx),784))
str(trainx)

testx <- array_reshape(testx,c(nrow(testx),784))

trainx <- trainx/255
testx <- testx/255
hist(trainx[1,])

# 7. One Hot Encoding
trainy <- to_categorical(trainy,10)
testy <- to_categorical(testy,10)
head(trainy)

# 8. Build a Model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 128,activation = 'relu',input_shape = c(784)) %>%
  layer_dense(units = 10,activation = 'softmax')
summary(model)

# The above model is having overfitting problem hence this model is not good. it may not work well with test data.

# make modification and adding more hidden layers

model <- keras_model_sequential()
model %>%
  layer_dense(units = 128,activation = 'relu',input_shape = c(784)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64,activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10,activation = 'softmax')


# Adding more iterations and hidden layers for fine tunning

model <- keras_model_sequential()
model %>%
  layer_dense(units = 256,activation = 'relu',input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128,activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64,activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10,activation = 'softmax')


# Adding more iterations and hidden layers for fine tunning

model <- keras_model_sequential()
model %>%
  layer_dense(units = 512,activation = 'relu',input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 256,activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
   layer_dense(units = 10,activation = 'softmax')


# 9. Compile

model %>%
  compile(loss='categorical_crossentropy',
          optimizer=optimizer_rmsprop(),
          metrics='accuracy')

# 10. Fit Model

history <- model %>%
  fit(trainx,
      trainy,
      epochs=30,
      batch_size=32,
      validation_split=0.2)


# 11. Evaluation and Prediction - Test data
model %>% evaluate(testx,testy)
pred <- model %>% predict_classes(testx)
table(Predicated=pred,Actual=mnist$test$y)

prob <- model %>% predict_proba(testx)
cbind(prob,Predicted_class=pred,Actual=mnist$test$y)[1:5,]

# New Data

library(EBImage)
setwd("D:\\from E\\Deep_Learning\\vid_6_images")
temp <- list.files(pattern = "*.JPG")
mypic <- list()
for (i in 1:length(temp)){mypic[[i]] <- readImage(temp[[i]])}

par(mfrow=c(3,2))
for(i in 1:length(temp)) plot(mypic[[i]])
par(mfrow=c(1,1))
str(mypic)

for(i in 1:length(temp)) {colorMode(mypic[[i]]) <- Grayscale}
for(i in 1:length(temp)) {mypic[[i]] <- 1-mypic[[i]]}
for(i in 1:length(temp)) {mypic[[i]] <- resize(mypic[[i]],28,28)}
for(i in 1:length(temp)) {mypic[[i]] <- array_reshape(mypic[[i]],c(28,28,3))}

new <- NULL

for(i in 1:length(temp)) {new <- rbind(new,mypic[[i]])}
str(new)

newx <- new[,1:784]
newy <- c(7,5,2,0,5,3)

# Prediction
pred <- model %>% predict_classes(newx)
table(Predicted=pred,Actual=newy)
