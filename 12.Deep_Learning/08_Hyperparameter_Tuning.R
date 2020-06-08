###################################################################
#                       HYPERPARAMTER TUNING                      #
# In machine Learning, Hyperparameter optimization or tunning is  #
# the problem of choosing a set of optimal hyperparameters for a  #
# learning algorithm.                                             #
#                                                                 #
#   A hyperparameter is a parameter whose value is used to control#
# the learning process. By contrast, the value of other parameters#
# are learned.                                                    #
###################################################################

# import libraries
library(keras)
library(tensorflow)
library(tfruns)

# 1.Set working Directory
setwd("D:\\from E\\Deep_Learning")

# 2. Read Data

data <- read.csv("CTG.csv",header = TRUE,sep = ",")
str(data)

# 3. Matrix

data <- as.matrix(data)
dimnames(data) <- NULL

# 4. Normalize
data[,1:21] <- normalize(data[,1:21])
data[,22] <- as.numeric(data[,22])-1

# 5. Data Partition
set.seed(1234)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
training <- data[ind==1,1:21]
test <- data[ind==2,1:21]

trainingtarget <- data[ind==1,22]
testtarget <- data[ind==2,22]

# 6. One Hot Encoding

trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)

# 7. Hyperparameter tuning

runs <- tuning_run("experiment.R",flags = list(dense_units1=c(32,64),
                                               dense_units2=c(16,32),
                                               dropout1=c(0.1,0.2),
                                               dropout2=c(0.1,0.2),
                                               batch_size=c(32,64)))

# 8. Best Hyperparameter valus

head(runs)
results <- runs[,c(3,5:10)]
