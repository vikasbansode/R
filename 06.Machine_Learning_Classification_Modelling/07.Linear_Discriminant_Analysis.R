###########################################################
#          LINER DISCRIMINANT ANALYSIS                    #
# Originally developed by R.A.Fisher in 1936.             #
# to classify subjects into one of the two clearly        #
# defined groups.                                         #
# It was later expanded to classify subjects              #
# into more than two groups.                              #
# It helps to find liner combination of original          #
# varialbes that provide the best possible seperation     #
# between the groups.                                     #
# The basic purpose is to estimate relationship           #
# between a single categorical dependent variable         #
# and a set of quantitative independent variables.        #  
###########################################################

#####################################################
#                   Examples                        #
# - Predicting success/failures of new products     #
# - accepting/rejecting admission to an applicant   #
# - predicting credit risk category for a person    #
# - Classifying patients into differnt categories   #
#####################################################

# 1. Set working Directory

setwd("D:\\from E\\ML")

# 2. Read Data

data("iris")
str(iris)

# 3.Data Preprocessing if required
# whitespaces,punctuation marks,duplicates,outlier,missing values

# 4. Exploratory Data Analysis
library(psych)
pairs.panels(iris[1:4],gap=0,bg=c("red","green","blue")[iris$Species],
            pch = 21
             )

# 5. Normalization/convert variable to to facter or numeric if required
 
# 6. Data Partion

set.seed(555)
ind <- sample(2,nrow(iris),replace = TRUE,prob = c(0.6,0.4))
train <- iris[ind==1,]
test <- iris[ind==2,]

# 5. Build a required model(in this case we are using LDA)

library(MASS)
linear <- lda(Species~.,train)
linear
attributes(linear)
linear$prior
linear$counts
linear$scaling

# 6. plot the model if required 
# here using histogram

p <- predict(linear,train)
ldahist(data = p$x[,1],g=train$Species)
ldahist(data = p$x[,2],g=train$Species)

# Bi-plot

install.packages('devtools')
library(devtools)
install_github('fawda123/ggord')
library(ggord)

#ggord(linear,train$species,ylim=c(-10,10))

# Partition Plot
#library(klaR)
#partimat(Species~.,data=traing,method='lda')
#partimat(Species~.,data=traing,method='qda')
# 7. P-value calculation if required

# 8. Prediction - train Data
p1 <- predict(linear,train)$class

# 9. Confusion Matrix & Error for Train Data
tab <- table(Predicted=p1,Actual=train$Species)
tab
1-sum(diag(tab))/sum(tab)

# 10. Prediction - test Data
p2  <- predict(linear,test)$class

# 11. Confusion Matrix & Error for test Data
tab1 <- table(Predicted=p2,Actual=test$Species)
tab1
1-sum(diag(tab1))/sum(tab1)


# 12. Compare training and Testing errors

0.984375- 0.9767442
0.02325581-0.015625

# 13. make a decision if model is good or bad

# There is no major significant difference in training & testing
# hence model looks good.

# 14. If model is good end the task here

# End of The Task

# 15. if model is still not good then, fine tune the model


#####################################################################
#              LDA ADAVANTAGE                                       #
# -1. Histrogram and bi-plot provide useful insight and are         #
# helpful for interpretation of the Analysis.                       #
# -2. If there is not a great difference in the group covariance    #
# matrices, then linear Discriminant Analysis will perform as well  #
# as quadratic.                                                     #
#               LDA DISADVANTAGES                                   #
# 1. It is not useful for non-linear problem                        #
# 2. not meeting multivariate normality assumption of the the       #
# independent variables can cause problems.                         #  
# 3. not meeting assuption of equal covariance matrices can         #
# adversely affect the classification process.                      #
#####################################################################   



