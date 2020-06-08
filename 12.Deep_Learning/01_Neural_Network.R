setwd("D:\\from E\\Deep_Learning")

getwd()

#Read Data
data <- read.csv("binary.csv",header = TRUE,sep = ",")

#Explore Data
str(data)

# Min-Max Normalization
data$gre <- (data$gre - min(data$gre))/(max(data$gre) - min(data$gre))
data$gpa <- (data$gpa - min(data$gpa))/(max(data$gpa) - min(data$gpa))
data$rank <- (data$rank - min(data$rank))/(max(data$rank) - min(data$rank))

# Data Partition
set.seed(222)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
training <- data[ind==1,]
testing <- data[ind==2,]

# Neural Networks
library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,data = training,hidden = 1,err.fct = 'ce',linear.output = FALSE)
plot(n)

#Prediction
output <- compute(n,training[,-1])
head(output$net.result) ## This is similar to probability
head(training[1,])

#Node Output Calculations with sigmoid Activation Function
in4 <- 0.0455 +(0.82344* 0.7586207)+(1.35186*0.8103448) + (-0.87435*0.6666667)
# hidden_layer_weight + (input_layer_gre_weight * gre) + (input_layer_gpa_weight * gpa) + (input_layer_rank_weight*rank)
#Sigmoid function
out4<-1/(1+exp(-in4))

in5 <- -7.06125+(8.5741*out4)
out5 <- 1/(1+exp(-in5))

# Confusion Matrix and Misclassification Error - training Data
output <- compute(n,training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted=pred1,Actual=training$admit)
sum(diag(tab1))/sum(tab1) # Accuracy
1-sum(diag(tab1))/sum(tab1) #Missclassification

# Confusion Matrix and Misclassification Error - testing Data

output <- compute(n,testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted=pred2,Actual=testing$admit)
sum(diag(tab2))/sum(tab2) # Accuracy
1-sum(diag(tab2))/sum(tab2) #Missclassification
# Above model is still have some consitency  so lets find tune model

#### FINE TUNNING #####
#Adding more neurons in hidden layer
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,data = training,hidden = 5,err.fct = 'ce',linear.output = FALSE)
plot(n)

# Confusion Matrix and Misclassification Error - training Data
output <- compute(n,training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted=pred1,Actual=training$admit)
sum(diag(tab1))/sum(tab1) # Accuracy
1-sum(diag(tab1))/sum(tab1) #Missclassification

# Confusion Matrix and Misclassification Error - testing Data

output <- compute(n,testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted=pred2,Actual=testing$admit)
sum(diag(tab2))/sum(tab2) # Accuracy
1-sum(diag(tab2))/sum(tab2) #Missclassification

########### working more on FINE TUNNING ##########
# Adding two neurons
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,data = training,hidden = c(2,1),err.fct = 'ce',linear.output = FALSE)
plot(n)

# Confusion Matrix and Misclassification Error - training Data
output <- compute(n,training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted=pred1,Actual=training$admit)
tab1
sum(diag(tab1))/sum(tab1) # Accuracy
1-sum(diag(tab1))/sum(tab1) #Missclassification

# Confusion Matrix and Misclassification Error - testing Data

output <- compute(n,testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted=pred2,Actual=testing$admit)
tab2
sum(diag(tab2))/sum(tab2) # Accuracy
1-sum(diag(tab2))/sum(tab2) #Missclassification

# Model is still not good 

########### working more on FINE TUNNING ##########
# NN with repeat calculations
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,data = training,hidden = 5,err.fct = 'ce',linear.output = FALSE,lifesign = 'full',rep =5,
               algorithm = 'rprop+',stepmax = 100000)
# out of 5 repeatations 4 are converged and best among them is 1st one
plot(n,rep = 1)

# Confusion Matrix and Misclassification Error - training Data
output <- compute(n,training[,-1],rep = 1)
p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted=pred1,Actual=training$admit)
tab1
sum(diag(tab1))/sum(tab1) # Accuracy
1-sum(diag(tab1))/sum(tab1) #Missclassification

# Confusion Matrix and Misclassification Error - testing Data

output <- compute(n,testing[,-1],rep = 1)
p2 <- output$net.result
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted=pred2,Actual=testing$admit)
tab2
sum(diag(tab2))/sum(tab2) # Accuracy
1-sum(diag(tab2))/sum(tab2) #Missclassification


# Neural Network Advantage
#1. It is robust with noisy data.
# Neural Network Disadvantage
#-- less interpretable than other models such decision trees.
#-- Usually need longer training time.

