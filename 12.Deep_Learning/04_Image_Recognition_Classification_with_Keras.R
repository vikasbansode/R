############################ IMAGE RECOGNITION AND CLASSIFICATION WITH KERAS ####################
# 1. Set working Directory

setwd("D:\\from E\\Deep_Learning\\vid_3_Images")

# 2.load pre-requsite libraries
library(EBImage)
library(keras)

# Read Images

pics <- c('p1.jpg','p2.jpg','p3.jpg','p4.jpg','p5.jpg','p6.jpg',
          'c1.jpg','c2.jpg','c3.jpg','c4.jpg','c5.jpg','c6.jpg'
          )

# 3. Store these images into list

mypic <- list()
for (i in 1:12) {mypic[[i]] <- readImage(pics[i])}

# 4. Exploare the images (EDA)

print(mypic[[1]])
display(mypic[[8]])
summary(mypic[[1]])
hist(mypic[[8]])
str(mypic)

# you may noticed one thing is that pictures come with sizes these numbers could be different.
# so what can we do is we can resize these pictures with same dimension

# 5. Resize Images
for(i in 1:12){mypic[[i]] <- resize(mypic[[i]],28,28)}
str(mypic)  # we everything is in same dimention

# 6. Reshape
28*28*3
for(i in 1:12){mypic[[i]] <- array_reshape(mypic[[i]],c(28,28,3))}
str(mypic) # now we have different structure what we had earlier

# one thing notice here, we still have 12 different items in this list
# so we want to combine this data into one using row bind

# 7. Row Bind
# in row binding we will use first 1:5 for plane and 7:11 for car images for training and 6th for plane image 
# and 12th for car image for testing
trainx <- NULL
for(i in 1:5){trainx <- rbind(trainx,mypic[[i]])} # need to write multiple look just change numbers
for(i in 7:11){trainx <- rbind(trainx,mypic[[i]])}
str(trainx)

# 8. Row Bind - Same for testing
testx <- rbind(mypic[[6]],mypic[[12]])

# 9. Generate Reponse variables
trainy <- c(0,0,0,0,0,1,1,1,1,1)
testy <- c(0,1)

# 10. One Hot Encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)

# 11. Create the Model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256,activation = 'relu',input_shape = c(2352)) %>%
  layer_dense(units = 128,activation = 'relu') %>%
  layer_dense(units = 2,activation = 'softmax')
summary(model)

# 12. Compile Model
model %>%
  compile(loss='binary_crossentropy',
          optimizer=optimizer_rmsprop(),
          metrics=c('accuracy'))

# 13. Fit Model

history <- model %>%
  fit(trainx,
      trainLabels,
      epochs = 30,
      batch_size=32,
      validation_split=0.2)

plot(history)

# 14. Evaluation & Prediction - train Data

model %>% evaluate(trainx,trainLabels)
pred <- model %>% predict_classes(trainx)
table(Predicted=pred,Actual=trainy)

prob <- model %>% predict_proba(trainx)
cbind(prob,Predicted=pred,Actual=trainy)

display(mypic[[11]])  # This is car but model predicted plane which is misclassfication

# 15. Evaluation & Prediction - test Data
model %>% evaluate(testx,testLabels)
pred <- model %>% predict_classes(testx)
table(Predicted=pred,Actual=testy) # For test data model predicted correctly there is no missclassification

display(mypic[[6]])
display(mypic[[12]])
