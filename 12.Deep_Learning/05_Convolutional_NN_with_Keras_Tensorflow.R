                      ###################################################################################
                      #                                                                                 #           
                      #           CONVOLUTIONAL NEURAL NETWORK WITH KERAS AND TENSORFLOW                #
                      #                                                                                 #
                      ###################################################################################
###################################################################################
#                       Main Advantages                                           #           
# - Regarded as 'GOld Standard' for image classification.                         #
# - less parameters needed compared to a fully connected network.                 #                                   
# - It has less paramers therefore less processing time.                          #
# - Suitable for image recognition and classification with larage number of images#
#               Layers in Convolution Neural Network                              #
#                  - Input layer                                                  #
#                  - Convolution layer                                            #
#                  - Pooling layer                                                #
#                  - Drop out layer                                               #
#                  - Flattening layer                                             #
#                  - Fully connected layer                                        #
#                  - Output layer                                                 #
###################################################################################


# 1. Set working Directory

setwd("D:\\from E\\Deep_Learning\\vid_4_Images")
                            
# 2. Import packages

library(keras)
library(EBImage)
                                                                  
# 3. Read Images

pic1 <- c('p1.jpg','p2.jpg','p3.jpg','p4.jpg','p5.jpg',
          'c1.jpg','c2.jpg','c3.jpg','c4.jpg','c5.jpg',
          'b1.jpg','b2.jpg','b3.jpg','b4.jpg','b5.jpg'
          )
# 4. Store images in list for train and test

train <- list()                      
for(i in 1:15){train[[i]] <- readImage(pic1[i])}                   

pic2 <- c('p6.jpg','c6.jpg','b6.jpg')
test <- list()
for(i in 1:3){test[[i]] <- readImage(pic2[i])}

# 5. Explore Data
print(train[[12]])
summary(train[[12]])
display(train[[12]])
plot(train[[12]])

par(mfrow=c(3,5))
for(i in 1:15) plot(train[[i]])
par(mfrow=c(1,1))


# 6. Resize images

str(train)
for(i in 1:15){train[[i]] <- resize(train[[i]],100,100)}
for(i in 1:3){test[[i]] <- resize(test[[i]],100,100)}

# 7. Combine images

train <- combine(train)
x <- tile(train,5)
display(x,title='Pictures')

test <- combine(test)
y <- tile(test,3)
display(y,title='Pictures')

str(train)
str(test)        

# 8. Reodrer dimension

train <- aperm(train,c(4,1,2,3))
test <- aperm(test,c(4,1,2,3))

# 9. generate Response variables

trainy <- c(0,0,0,0,0,1,1,1,1,1,2,2,2,2,2)
testy <- c(0,1,2)

# 10. One Hot Encoding

trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)

#[============================================================================================================]

# 11. Build a Model

model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = 'relu',input_shape = c(100,100,3)) %>%
  layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate=0.25) %>%
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = 'relu') %>%
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate=0.25) %>%
  layer_flatten()%>%
  layer_dense(units = 256,activation = 'relu') %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 3,activation = 'softmax')


summary(model)

# 12. Compile Modle

model %>%
  compile(loss='categorical_crossentropy',optimizer= optimizer_sgd(lr=0.01,decay = 1e-6,momentum = 0.9,nesterov = T),
          metrics=c('accuracy'))


# 13. Fit model
history <- model %>%
  fit(train,
      trainLabels,
      epochs = 60,
      batch_size=32,
      validation_split=0.2)
plot(history)

# 14. Evaluation & Prediction - train Data

model %>% evaluate(train,trainLabels)
pred <- model %>% predict_classes(train)
table(Predicted= pred,Actual=trainy)

prob <- model %>% predict_proba(train)
cbind(prob,Predicted_class=pred,Actual=trainy)

# 15. Evaluation & Prediction - test Data

model %>% evaluate(test,testLabels)
pred <- model %>% predict_classes(test)
table(Predicted= pred,Actual=testy)

prob <- model %>% predict_proba(test)
cbind(prob,Predicted_class=pred,Actual=testy)
