setwd("D:\\from E\\ML")

# Import libraries

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Import/Read Data

data <- read.csv("binary.csv",header = TRUE,sep = ",")

# Explore Data
str(data)

# do the Crosstabulation for admit and Rank
xtabs(~admit+rank,data = data)

# Convert rank and admit variable to factor
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)

str(data)

# Visualization
pairs.panels(data[-1])

data %>%
  ggplot(aes(x=admit,y=gpa,fill=admit)) +
  geom_boxplot()+
  ggtitle("Box Plot")

data %>%
  ggplot(aes(x=gpa,fill=admit))+
  geom_density(alpha=0.8,color='black')+
  ggtitle('Density Plot')

#Data Partition

set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# Naive Bayes Model
model <- naive_bayes(admit ~ .,data = train,usekernel = T)
model

train %>%
  filter(admit=="0") %>%
  summarise(mean(gre),sd(gre))

train %>%
  filter(admit=="1") %>%
  summarise(mean(gre),sd(gre))

plot(model)

#Prediction
p <- predict(model,train,type='prob')
head(cbind(p,train))

#Confusion Matrix - train data

p1 <- predict(model,train)
(tab1 <- table(p1,train$admit))
1-sum(diag(tab1))/sum(tab1)

#Confusion Matrix - train data
p2 <- predict(model,test)
tab2 <- table(p2,test$admit)
1-sum(diag(tab2))/sum(tab2)
