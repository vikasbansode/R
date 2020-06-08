                      ########################## TRANSFER LEARNING ##########################


###################################
#            Benifits :           #
# - works with small data         #
# - less time                     #
# - low cost                      #
###################################
#################################################################################
# in Keras library there are so many pre-trained different libraries available. #
# 01. Xception                                                                  #
# 02. VGG16                                                                     #
# 03. VGG19                                                                     #
# 04. RestNet50   (Residual Networks 50 layer deep                              #
# 06. InceptionRestNetV2                                                        #
# 07. MobileNet                                                                 #
# 08. MobileNetV2                                                               #
# 09. DenseNet                                                                  #
# 10. NASNet                                                                    #
#################################################################################

# 1.Import libraries

library(keras)
library(EBImage)
library(tensorflow)

# 2.Identify image with RESNET50

pretrained <- application_resnet50(weights = 'imagenet')
setwd("D:\\from E\\Deep_Learning\\Image_dog")
img <- image_load("dog1.jpeg",target_size = c(224,224))
x <- image_to_array(img)
plot(as.raster(x,max=255))
str(x)
summary(x)
hist(x)

# 3. Preprocessing image
x <- array_reshape(x,c(1,dim(x)))
x <- imagenet_preprocess_input(x)
preds <- pretrained %>% predict(x)
imagenet_decode_predictions(preds,top = 5)[[1]]


# CIFAR10 data

data <- dataset_cifar10()
str(data)

trainx <- data$train$x[1:2000,,,]
testx <- data$test$x[1:2000,,,]

trainy <- to_categorical(data$train$y[1:2000,],num_classes = 10)
testy <- to_categorical(data$test$y[1:2000,],num_classes = 10)

# Plot CIFAR10 samples Images
par(mfrow=c(8,8),mar=rep(0,4))
for (i in 1:64) plot(as.raster(trainx[i,,,],max=255))
par(mfrow=c(1,1))

# Idenfification of 2nd CIRAR10 image
x <- trainx[2,,,]
str(x)
x <- resize(x,224,224)
x <- array_reshape(x,c(1,dim(x)))
x <- imagenet_preprocess_input(x)
preds <- pretrained %>% predict(x)
imagenet_decode_predictions(preds,top = 5)[[1]]

# Data Preprocessing
x <- array(rep(0,200*224*224*3),dim = c(2000,224,224,3))
for(i in 1:2000){x[i,,,] <- resize(trainx[i,,,],224,224)}
trainx <- imagenet_preprocess_input(x)

x <- array(rep(0,200*224*224*3),dim = c(2000,224,224,3))
for(i in 1:2000){x[i,,,] <- resize(testx[i,,,],224,224)}
testx <- imagenet_preprocess_input(x)

par(mfrow =c(1,2),mar=rep(0,4))
plot(as.raster(data$train$x[2,,,],max=255))
plot(as.raster(x[2,,,],max=255))

# Model with RESTNET50
pretrained <- application_resnet50(weights = 'imagenet',include_top = FALSE,
                                   input_shape = c(224,224,3))
pretrained

# Build Keras model

model <- keras_model_sequential() %>%
  pretrained %>%
  layer_flatten() %>%
  layer_dense(units = 256,activation = 'relu') %>%
  layer_dense(units = 10,activation = 'softmax')

freeze_weights(pretrained)

# Compile model
model %>%
  compile(loss='categorical_crossentropy',
          optimizer='adam',
          metrics='accuracy')

# Fit Model
history<- model %>%
  fit(trainx,
      trainy,
      epochs=10,
      batch_size=10,
      validation_split=0.2)

# Model Evaluation & Prediction
model %>% evaluate(testx,testy)

pred <- model %>% predict_classes(testx)
tab <- table(Predicted=pred,Actual=data$test$y[1:2000,])

100*diag(tab)/colSums(tab)


