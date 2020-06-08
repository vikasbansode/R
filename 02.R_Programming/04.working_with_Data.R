# Set working Directory

setwd("D:\\from E\\R")

# working with Data

LungCapData <- read.csv("LungCapData.txt",header = T,sep = "\t")

# Explore Data

attach(LungCapData)
dim(LungCapData)
names(LungCapData)
str(LungCapData)
head(LungCapData)
tail(LungCapData)

LungCapData[c(5,6,7,8,9),]

LungCapData[5:9,]

head(LungCapData[-c(4:722)])

class(LungCapData$Gender)
levels(LungCapData$Gender)
length(LungCapData$Gender)
summary(LungCapData)


# Logics Statement, Cbind, Rbind

Age[1:5]

temp <- Age>15

temp2 <- as.numeric(Age>15)

FemSmoke <- Gender == "female" & Smoke == "yes"
FemSmoke[1:5]

# Attach deta to original Data frame using cbind

MoreData <- cbind(LungCapData,FemSmoke)
MoreData[1:5,]
