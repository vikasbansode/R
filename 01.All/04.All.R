setwd("D:\\from E\\ML")

# Data Collection
data("iris")
data <- iris
# Exlore Data
str(data)
dim(data)
length(data)
head(data)
tail(data)
names(data)
levels(data)
class(data$Species)
class(data$Sepal.Length)
summary(data)
# Data cleaning
# Detect Trailing and leading spaces
summary(data)
# Handling Trailing and Leading space
# Here we observed trailing and Leading spaces for Gender attribute, lets clean them
data$Gender <- trimws(data$Gender,which = c('right'))
data$Gender <- trimws(data$Gender,which = c('left'))
summary(data)
# Detect Punctuation marks
summary(data)
# Handling Punctuation marks
#Here we see some punctuation marks for Smoke Atrribute, lets clean them
data$smoke <- gsub("[[:punct:]]|[[:digit:]]|(http[[:alpha:]]*:\\/\\/)","",data$Smoke)
# Detect Outliers
boxplot(data$Sepal.Length)
boxplot(data$Sepal.Width)
boxplot(data$Petal.Length)
boxplot(data$Petal.Width)
# Handling Outliers
IQR <- 15.00 - 9.00
bench <- 15.00 + 1.5*IQR
bench
# Display outliers
data$Age[data$Age > bench]
# Replace outliers with NA values
# NOTE : You can replace , omit, remove outliers from data according to your requirment
data$Age[data$Age > bench]  <- NA
# Detect Missing Values
library(mice)
library(VIM)
md.pattern(data)
md.pairs(data)
p <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,p)
# OR to get exact rows of missing values
apply(is.na(data),2,which)
# Data Preparation
# Derived columns
aa <- data.frame(x = c(2,4,6),y=c(3,5,7))
aa
aa$z <- (aa$x + aa$y)-2
aa
# Handling Missing Values
install.packages("Hmisc")
library(Hmisc)
install.packages("DMwR")
library('DMwR')
install.packages("mice")
library(mice)
install.packages("rpart")
library(rpart)
install.packages('zoo')
library(zoo)

file.choose()
setwd("D:\\from E\\ML")
df <- read.csv("missing_values.csv",header = T,sep = ",")
# Detect missing values
md.pattern(df)
md.pairs(df)
p <- function(x){sum(is.na(x))/length(x)*100}
apply(df,2,p)
is.na(df)
which(is.na(df))
apply(is.na(df),2,which)
# imputation with mean / median / mode
# we are replacing Height missing values with mean

mean(data$Height[!is.na(data$Height)])
mean(data$Height,na.rm = TRUE)
round(mean(data$Height,na.rm = TRUE),2)
data$Height[which(is.na(data$Height))] <- round(mean(data$Height,na.rm = TRUE),2)

# you can omit or replace the missing values according to your requirement 
# here I m replacing with mean
round(mean(data$Age,na.rm = TRUE))
data$Age[which(is.na(data$Age))] <- round(mean(data$Age,na.rm = TRUE))
# imputation with na.rm, na.omit,na.exclude,na.fill
# na.fill(x,"extend")
# na.fill(x,)

df
class(df$day)
library(lubridate)
df$day <- mdy(as.character(df$day))
df

df$event[df$event ==''] <- NA
df
df1 <- df
na.fill(df1$temperature,"extend")
na.fill(df1$temperature,c("extend",NA))
na.fill(df$temperature,list(NA,NULL,NA))
na.aggregate(df$temperature)
na.action()
na.aggregate()
na.aggregate.default()
na.approx()
na.approx.default()
na.contiguous()
na.exclude()
na.fail()
na.fill()
na.fill.default()
na.fill0()
na.replace()
# Data Transformation
setwd("D:\\from E\\ML")
data <- read.csv("Cardiotocographic.csv",header = T,sep = ",")
data$NSPF <- factor(data$NSP)
str(data)
data <- as.matrix(data)
class(data)
# Exploratory Data Analysis
# Summarization
data("iris")
# Univerate Analysis
summary(iris)
table(iris$Species)
# Bivoriate Analysis
cor(iris$Sepal.Length,iris$Sepal.Width)
cor(iris$Petal.Length,iris$Petal.Width)
# Multivariate Analysis
aggregate(iris$Sepal.Length, by=list(Species=iris$Species), FUN=mean) 
aggregate(iris$Sepal.Width, by=list(Species=iris$Species), FUN=mean) 
aggregate(iris$Petal.Length, by=list(Species=iris$Species), FUN=mean) 
aggregate(iris$Petal.Width, by=list(Species=iris$Species), FUN=mean)
aggregate(data.frame("SepalLength(cm)"=iris$Sepal.Length,"SepalWidth(cm)"=iris$Sepal.Width,"PetalLength(cm)"=iris$Petal.Length,
                     "PetalWidth(cm)"=iris$Petal.Width), by=list(Species=iris$Species), FUN=mean)
barplot(table(iris$Species))
# Correlation Matrix
library(corrplot)
cor(iris[-5])
corrplot(cor(iris[-5]),method = 'pie')
corrplot(cor(iris[-5]),method = 'color')
corrplot(cor(iris[-5]),method = 'number')
corrplot(cor(iris[-5]),method = 'shade')
corrplot(cor(iris[-5]),method = 'ellipse')
corrplot(cor(iris[-5]),method = 'circle')
# Visualization
# Histogram
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
# Pair plots
library(psych)
pairs.panels(iris[-5],gap=0,bg=c("red","yellow","blue")[iris$Species],pch=21)
pairs(iris)
plot(iris[,1:4],
     main="Relationships between characteristics of iris flowers",
     pch=19,
     col="blue",
     cex=0.9)
# Box plot to check overlap
data <- read.csv("binary.csv",header = T,sep = ",")
str(data)
library(ggplot2)
data %>%
  ggplot(aes(x=admit,y=gre,fill=admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")

data %>%
  ggplot(aes(x=admit,y=gpa,fill=admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")

# Density plot to check overlap

data %>%
  ggplot(aes(x=gre,fill=admit)) +
  geom_density(alpha=0.8,color='black') +
  ggtitle('Density Plot')

data %>%
  ggplot(aes(x=gpa,fill=admit)) +
  geom_density(alpha=0.8,color='black') +
  ggtitle('Density Plot')

# Data Normalization
# Using Min max
df <- data.frame(ctl,trt)
df$ctl <- (df$ctl - min(df$ctl))/(max(df$ctl)-min(df$ctl))
df$trt <- (df$trt - min(df$trt))/(max(df$trt)-min(df$trt))
df

# Using Z-score
df1 <- data.frame(ctl,trt)
df1

df1$ctl <- (df1$ctl - mean(df1$ctl))/sd(df1$ctl)
df1$trt <- (df1$trt - mean(df1$trt))/sd(df1$trt)
df1

# Using Decimal Scaling

df2 <- data.frame(ctl,trt)
df2$ctl <- (df2$ctl)/(10^2)
df2$trt <- (df2$trt)/(10^2)
df2
# Data Partition
set.seed(1234)
ind = sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train <- data[ind == 1,] 
test <- data[ind == 2, ]
# Build a Model
library(naivebayes)
model <- naive_bayes(admit ~., data = train)
model

train %>%
  filter(admit == "0") %>%
  summarise(mean(gre),sd(gre))

plot(model)

# Predict Model
p <- predict(model,train,type = 'prob')
head(cbind(p,train))
# Confusion Matrix - train Data
p1 <- predict(model,train)
tab1 <- table(p1,train$admit)
tab1
1-sum(diag(tab1))/sum(tab1)
# Confusion Matrix -- Test Data
p2 <- predict(model,test)
(tab2 <- table(p2,test$admit))
1-sum(diag(tab2))/sum(tab2)

#########################P PROBLEM NO 1 ###################################
# OverFitting and Under Fitting
# HOW TO DETECT OVER FITTING AND UNDER FITTING PROBLEM
# Overfitting: Good performance on the training data and poor generliazation to other data.
# Underfitting: Poor performance on the training data and poor generalization to other data.
# Split Data training and testing and check if the model performance
# Confusion Matrix - train Data
p1 <- predict(model,train)
tab1 <- table(p1,train$admit)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix -- Test Data
p2 <- predict(model,test)
(tab2 <- table(p2,test$admit))
1-sum(diag(tab2))/sum(tab2)

# Handling Overfitting and Underfitting
# Overfitting and UnderFitting -> use Regularization or Cross validation Methods
# Regularization
library(caret)
library(glmnet)
library(mlbench)
library(psych)
data("BostonHousing")
data <- BostonHousing
str(data)
pairs.panels(data[c(-4,-14)])

set.seed(222)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Cross validation
custom <- trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = T)

set.seed(1234)
lm <- train(medv ~., train,method="lm",trControl=custom)

#results
lm$results
lm
summary(lm)
plot(lm$finalModel)

# Ridge Regression
set.seed(1234)
ridge <- train(medv ~., train,method='glmnet',tuneGrid = expand.grid(alpha = 0,
                                                                     lambda = seq(0.0001,1,length=5)),
               trControl= custom)
# Plot results
plot(ridge)
ridge
plot(ridge$finalModel,xvar = "lambda",label = T)
plot(ridge$finalModel,xvar = "dev",label = T)
plot(varImp(ridge,scale = F))
plot(varImp(ridge,scale = T))

# Lasso Regression
set.seed(1234)
lasso <- train(medv ~., train,method='glmnet',tuneGrid=expand.grid(alpha=1,
                                                                   lambda=seq(0.0001,0.2,length=5)),
               trControl=custom)
plot(lasso)
lasso
plot(lasso$finalModel,xvar = "lambda",label = T)
plot(lasso$finalModel,xvar = "dev",label = T)
plot(varImp(lasso,scale = F))
plot(varImp(lasso,scale = T))

# Elastic Net Regression
set.seed(1234)
en <- train(medv ~., train,method='glmnet',tuneGrid=expand.grid(alpha=seq(0,1,length=10),
                                                                   lambda=seq(0.0001,0.2,length=5)),
               trControl=custom)
plot(en)
en
plot(en$finalModel,xvar = "lambda",label = T)
plot(en$finalModel,xvar = "dev",label = T)
plot(varImp(en,scale = F))
plot(varImp(en,scale = T))

# Compare Models

model_list <- list(LinearModle = lm,Ridge=ridge,Lasso=lasso,ElasticNet=en)

res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res,metric = 'RMSE')

# Best Model
en$bestTune
best <- en$finalModel
coef(best,s=en$bestTune$lambda)

# Save Final Model for Later Use
saveRDS(en,"Final_model.rds")

# Read Model
fm <- readRDS("Final_model.rds")
print(fm)

# Prediction

p1 <- predict(fm,train)
sqrt(mean((train$medv-p1)^2))

p2 <- predict(fm,test)
sqrt(mean((test$medv-p2)^2))
#########################P PROBLEM NO 2 ###################################
# Class Imbalance 
data <- read.csv("binary.csv",header = T,sep = ",")
str(data)
data$admit <- as.factor(data$admit)
summary(data)
prop.table(table(data$admit))
barplot(prop.table(table(data$admit)),col=rainbow(2),ylim = c(0,0.7),main = "Class Distribution")
set.seed(123)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
train <- data[ind == 1,]
test <- data[ind == 2,]
table(train$admit)
prop.table(table(train$admit))
library(randomForest)
rftrain <- randomForest(admit ~., data = train)
library(caret)
confusionMatrix(predict(rftrain,test),test$admit, positive = '1')

# Handling Class Imbalance
# Class Imbalance -- Use
# Under Sampling
# Over Sampling
# Both under and Over sampling
# generate Synthetic Data
# check which model is performing best and keep that for model building

# Over Sampling for best sensitivity

library(ROSE)
over <- ovun.sample(admit ~.,data=train,method = "over",N=376)$data # -- Over sampling
table(over$admit)

rfover <- randomForest(admit~.,data = over)
confusionMatrix(predict(rfover,test),test$admit, positive = '1')


under <- ovun.sample(admit~.,data=train,method = "under",N=194)$data #-- Under sampling
table(under$admit)
rfunder <- randomForest(admit ~.,data = under)
confusionMatrix(predict(rfunder,test),test$admit, positive = '1')


both <- ovun.sample(admit~.,data=train,method = "both",p=0.5,seed = 222,N=285)$data # both under and over
table(both$admit)
rfboth <- randomForest(admit ~.,data = both)
confusionMatrix(predict(rfboth,test),test$admit, positive = '1')

rose <- ROSE(admit ~., data=train,N=500,seed = 111)$data #-- This synthetic Data
table(rose$admit)
rfrose <- randomForest(admit~.,data = rose)
confusionMatrix(predict(rfrose,test),test$admit, positive = '1')

#########################P PROBLEM NO 3 ###################################
# Detect Multicollinarity
library(faraway)
data("divusa")
head(divusa)
mydata <- data.frame(divusa[,-1])
head(mydata)
round(cor(mydata),2)
mymodel <- lm(divorce ~., mydata)
mymodel
summary(mymodel)
vif(mymodel)

# Handling Multicollinarity
# Multicollinarity - FEATURE SELECTION, REGUALARIATION SPECIALLY LASSO REGRESSION

library(Boruta) 
library(mlbench)
library(caret)
library(randomForest)
data("Sonar")
str(Sonar)
dim(Sonar)

# Feature Selection
set.seed(111)
boruta <- Boruta(Class ~., data = Sonar,doTrace = 2,maxRuns = 500)
print(boruta)

plot(boruta,las=2,cex.axis = 0.7)
plotImpHistory(boruta)

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

# Data Partition
set.seed(222)
ind <- sample(2,nrow(Sonar),replace = T,prob = c(0.6,0.4))
train <- Sonar[ind == 1,]
test <- Sonar[ind == 2,]

# RandomForest
set.seed(333)
rf60 <- randomForest(Class ~., data = train)
rf60

# Predict Test Data
p <- predict(rf60,test)
confusionMatrix(p,test$Class)

# FROM Boruta
getNonRejectedFormula(boruta)

rf41 <- randomForest(Class ~ V1 + V2 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + 
                       V26 + V27 + V28 + V29 + V30 + V31 + V32 + V35 + V36 + V37 + 
                       V39 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + V52 + 
                       V54 + V59,data=train)

p <- predict(rf41,test)
confusionMatrix(p,test$Class)

getConfirmedFormula(boruta)

rf33 <- randomForest(Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
                       V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
                       V31 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + V48 + 
                       V49 + V51 + V52,data = train)

p <- predict(rf33,test)
confusionMatrix(p,test$Class)

# PRINCIPAL COMPONENT ANALYSIS
data("iris")
str(iris)
summary(iris)

# Data partition
set.seed(111)
ind <- sample(2,nrow(iris),replace = T,prob = c(0.8,0.2))
training <- iris[ind == 1,]
testing <- iris[ind ==2,]

library(psych)
pairs.panels(training[,-5],gap=0,bg=c('red','yellow','blue')[training$Species],pch=21)
# if independent variables are highly correlated then it creates multicollinearity problem.
# so, we need to bring them on to common ground
# PCA

pc <- prcomp(training[,-5],center = TRUE,scale. = TRUE)

attributes(pc)
pc$center  # -- center is mean of each varaibles
mean(training$Sepal.Length)
pc$scale  # -- scale is standard deviation for each variables
sd(training$Sepal.Length)

print(pc)
summary(pc)

# Orthogonality of PCs
pairs.panels(pc$x,gap=0,bg=c('red','yellow','blue')[training$Species],pch=21)

# Bi-plot
library(devtools)
library(ggplot2)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pc,obs.scale = 1,var.scale = 1,groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name= '')
g <- g + theme(legend.direction = 'horizontal',legend.position = 'top')
print(g)

# Prediction with PC
trg <- predict(pc,training)
trg <- data.frame(trg,training[5])

tst <- predict(pc,testing)
tst <- data.frame(tst,testing$Species)

# Multinomial Logistic Regression with First Two PC's
library(nnet)
trg$Species <- relevel(trg$Species,ref="setosa")
mymodel <- multinom(Species ~PC1 + PC2,data = trg)
summary(mymodel)

# Confusion matrix - training
p <- predict(mymodel,trg)
tab <- table(p,trg$Species)
tab
1 - sum(diag(tab))/sum(tab)
#########################P PROBLEM NO 4 ###################################
# Detect Heteroscadasticity
library(lmtest)
bptest(mymodel)
library(car)
ncvTest(mymodel)

# Handling Heterocadasticity
# HETEROSCADASTICITY -- USE
# BOX COX TRANSFROMATION, PCA
library(faraway)
data("butterfat")
str(butterfat)
fullmodel <- lm(Butterfat ~ Breed  + Age ,data = butterfat)
plot(fullmodel)

library(MASS)
bc <- boxcox(fullmodel,lambda = seq(-3,3))
# Extract best lambda
best.lam <- bc$x[which(bc$y==max(bc$y))]
# Transform the data using inverse
fullmodel.inv <- lm((Butterfat)^-1 ~ Breed + Age,data = butterfat)
plot(fullmodel.inv)
# NORMALIZATION
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
weight <- cbind(ctl, trt)
class(weight)
print(weight)

#########################P PROBLEM NO 5 ###################################
# Detect Non-Linearity
crPlots(mymodel)
ceresPlots(mymodel)

# Handling Non-Linearity
setwd("D:\\Economatrics")
# Read Data
LungCapData <- read.csv("LungCapData2.csv",header = T,sep = ",")
attach(LungCapData)
summary(LungCapData)
# make a plot of LungCap vs. Height

plot(Height,LungCap,main = "Polynomial Regression",las=1)

# now, let's fit a linear Regression
model1 <- lm(LungCap ~ Height)

# and add the line to the plot... make it thick and red...
abline(model1,lwd=3,col="red")

# We can also use Residual plots to help with assessing linearity and checking
# other model assumptions

# first, the WRONG WAY ....
model2 <- lm(LungCap~Height+Height^2)
summary(model2)

# first, the RIGTH WAY
model3 <- lm(LungCap~Height+I(Height^2))
summary(model3)

# Alternative ways to include Height:
# 1: Create a new variable called Height^2 and include
# this variable into the model

#Or, Create Height^2, and then include this in model...it's the same!
HeightSquare <- Height^2
model3again <- lm(LungCap~Height+HeightSquare)
summary(model3again)

# Or, use the "poly" command...it's the same!
model3againagain <- lm(LungCap~poly(Height,degree = 2,raw = T))
summary(model3againagain)

# now,let's add this to the plot, using a thick blue line
lines(smooth.spline(Height,predict(model3)),col='blue',lwd=3)

# test if the model including Height^2 i signif. better than one without
# using the partial F-Test
anova(model2,model3)

# from the F-test we can see such a small p-value we will reject the null
# hypothesis and conclude that we have evidence to believe the model
# including height squared provides a Statistically significantly better fit
# than the model without Height squared.

# try fitting a model that includes Height^3 as well
model4 <- lm(LungCap ~Height+I(Height^2)+I(Height^3))
summary(model4)

# Now,let's add this model to the plot, using a thick dashed green line
lines(smooth.spline(Height,predict(model4)),col="green",lwd=3,lty=3)

# and, let's add a lenged to clarify the lines
legend(46,15,legend = c("model1:linear","model3:poly x^2","model:poly"),
       col=c("red","blue","green"),lty=c(1,1,3),lwd=3,bty="n",cex=0.9)

#########################P PROBLEM NO 6 ###################################
# Detect Auto Correlation
durbinWatsonTest(mymodel)
dwtest(mymodel)
bgtest(mymodel)
library(sandwich)
# Handling AutoCorrelation
# PRINCIPAL COMPONENT ANALYSIS
data("iris")
str(iris)
summary(iris)

# Data partition
set.seed(111)
ind <- sample(2,nrow(iris),replace = T,prob = c(0.8,0.2))
training <- iris[ind == 1,]
testing <- iris[ind ==2,]

library(psych)
pairs.panels(training[,-5],gap=0,bg=c('red','yellow','blue')[training$Species],pch=21)
# if independent variables are highly correlated then it creates multicollinearity problem.
# so, we need to bring them on to common ground
# PCA

pc <- prcomp(training[,-5],center = TRUE,scale. = TRUE)

attributes(pc)
pc$center  # -- center is mean of each varaibles
mean(training$Sepal.Length)
pc$scale  # -- scale is standard deviation for each variables
sd(training$Sepal.Length)

print(pc)
summary(pc)

# Orthogonality of PCs
pairs.panels(pc$x,gap=0,bg=c('red','yellow','blue')[training$Species],pch=21)

# Bi-plot
library(devtools)
library(ggplot2)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pc,obs.scale = 1,var.scale = 1,groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name= '')
g <- g + theme(legend.direction = 'horizontal',legend.position = 'top')
print(g)

# Prediction with PC
trg <- predict(pc,training)
trg <- data.frame(trg,training[5])

tst <- predict(pc,testing)
tst <- data.frame(tst,testing$Species)

# Multinomial Logistic Regression with First Two PC's
library(nnet)
trg$Species <- relevel(trg$Species,ref="setosa")
mymodel <- multinom(Species ~PC1 + PC2,data = trg)
summary(mymodel)

# Confusion matrix - training
p <- predict(mymodel,trg)
tab <- table(p,trg$Species)
tab
1 - sum(diag(tab))/sum(tab)
#########################P PROBLEM NO 7 ###################################
# Vanishing Gradient -- Dropout, Activation Function, increase and decrease hidden layer
#########################P PROBLEM NO 8 ###################################
# Exploding Gradient -- Dropout, Activation Function, increase and decrease hidden layer

