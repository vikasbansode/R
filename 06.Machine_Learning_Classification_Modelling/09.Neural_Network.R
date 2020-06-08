#Neural Network

# Set working Directory

setwd("D:\\from E\\ML")

# Read Data

data <- read.csv("binary.csv",header = TRUE,sep = ",")
str(data)

#  Min-max normalization

data$gre <- (data$gre -min(data$gre))/(max(data$gre) -min(data$gre))
data$gpa <- (data$gpa -min(data$gpa))/(max(data$gpa) -min(data$gpa))
data$rank <- (data$rank -min(data$rank))/(max(data$rank) -min(data$rank))

# Data Partition
set.seed(222)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
training <- data[ind==1,]
testing <- data[ind==2,]

# Neural Network
library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,data = training,
               hidden = 1,
               err.fct = 'ce',
               linear.output = FALSE)
plot(n)

############################################################

# Prediction - Training Data

output <- compute(n,training[,-1])
head(output$net.result)
head(training[1,])

p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)

# Confusion Matrix - Training Data

tab1 <- table(pred1,training$admit)
tab1

# Misclassification Error - Training Data

1-sum(diag(tab1))/sum(tab1)

#################################################################

# Prediction Testing Data

output2 <- compute(n,testing[,-1])
p2 <- output2$net.result
pred2 <- ifelse(p2>0.5,1,0)

# Confusion Matrix - testing Data

tab2 <- table(pred2,testing$admit)
tab2

# Misclassification Error - tesing Data
1-sum(diag(tab2))/sum(tab2)

# compare training and testing misclassification
0.2953737-0.2941176

#################################################################
################ADDING MORE NUERONS/HIDDEN LAYERS IN THE MODEL###
#################################################################
# Adding more neurons or hidden layer in model
library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,data = training,
               hidden = 5,
               err.fct = 'ce',
               linear.output = FALSE,lifesign = 'full',
               rep = 5,
               algorithm = "rprop+",
               stepmax = 100000)
plot(n,rep = 1)

############################################################

# Prediction - Training Data

output <- compute(n,training[,-1])
head(output$net.result)
head(training[1,])

p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)

# Confusion Matrix - Training Data

tab1 <- table(pred1,training$admit)
tab1

# Misclassification Error - Training Data

1-sum(diag(tab1))/sum(tab1)

#################################################################

# Prediction Testing Data

output2 <- compute(n,testing[,-1])
p2 <- output2$net.result
pred2 <- ifelse(p2>0.5,1,0)

# Confusion Matrix - testing Data

tab2 <- table(pred2,testing$admit)
tab2

# Misclassification Error - tesing Data
1-sum(diag(tab2))/sum(tab2)

# compare training and testing misclassification
0.2562278-0.2352941

#################################################################
#############NUERAL NETWORK WITH TWO HIDDEN LAYER################
#################################################################

library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank,data = training,
               hidden = c(2,1),
               err.fct = 'ce',
               linear.output = FALSE)
plot(n)

############################################################

# Prediction - Training Data

output <- compute(n,training[,-1])
head(output$net.result)
head(training[1,])

p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)

# Confusion Matrix - Training Data

tab1 <- table(pred1,training$admit)
tab1

# Misclassification Error - Training Data

1-sum(diag(tab1))/sum(tab1)

#################################################################

# Prediction Testing Data

output2 <- compute(n,testing[,-1])
p2 <- output2$net.result
pred2 <- ifelse(p2>0.5,1,0)

# Confusion Matrix - testing Data

tab2 <- table(pred2,testing$admit)
tab2

# Misclassification Error - tesing Data
1-sum(diag(tab2))/sum(tab2)

# compare training and testing misclassification
0.2562278-0.2352941

#################################################################
#                   ADVANTAGES OF NEURAL NETWORK                #
# - It is robust with noisy Data                                #
#                   DISADVANTAGES OF NEURAL NETWORK             #
# - Less interpretable than other models such as decision Trees #
# - Usually need longer training time                           #  
#################################################################




