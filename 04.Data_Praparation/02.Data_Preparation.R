                                        #############################################################
                                        #                                                           #
                                        #           Data Preparation                                #
                                        # Below steps are neccessary for Deep Learning              #
                                        #                                                           #
                                        #   1. Derived Columns if required                          #
                                        #   2. Convert Data to matrix                               #
                                        #   3. Normalize Data                                       #
                                        #   4. Data Partion for independent and Dependent variables #
                                        #   5. One Hot Encoding                                     #
                                        #############################################################


# Set working Directory

setwd("D:\\from E\\ML")                                        

# Get working Directory

getwd()                                      

# Read Data

data <- read.csv("CTG.csv",header = TRUE,sep = ",")
str(data)

# Change Data to Matrix

data <- as.matrix(data)

# Remove the Dimension names

dimnames(data) <- NULL

# Normalize data using normalize function  is part of keras package

library(keras)
data[,1:21] <- normalize(data[,1:21])
data[,22] <- as.numeric(data[,22])-1
summary(data)

# Data Partition for independent variabel

set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training <- data[ind==1,1:21]
testing <- data[ind==2,1:21]

# Data Partition for dependent or target variable

trainingtarget <- data[ind==1,22]
testingtarget <- data[ind==2,22]

# One Hot Encoding

trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testingtarget)

# Derived Column

df <- data.frame(a=1:5,b=2:6)
df

with(df, {c <- a + b; df;} )

within(df, {c <- a + b; df;} )

# OR

aa <- data.frame(x=c(2,4,6),y=c(3,5,7))
aa$z <- (aa$x + aa$y)
aa

# Convert categorical levels to factor
df <- data.frame(sex=c("male","female","female","female","male"),pclass=c(3,1,3,1,3))
df

levels(df$sex) <- c(1,0)
df

names(df)[2] <- "class"
df

# copying Data structure for ML

df <- data.frame(a=1:5,b=2:6)
df

names(df)
noquote(names(df))

paste(colnames(df),collapse = "+")

noquote(paste(colnames(df),collapse = "+"))

# Now Data is ready for model building
