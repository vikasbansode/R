                      ###############################################
                      #                                             #
                      #           Data Cleaning                     #
                      # 1. Removing Trailing and Leading Spaces     #
                      # 2. Removing Punctuation marks               #
                      # 3. Removing Outliers                        #
                      # 4. Removing Missing Values                  #
                      #                                             #
                      ############################################### 

# Set working Directory

setwd("D:\\from E\\ML")

# get working Directory

getwd()

# Read Data

data <- read.csv("Data_Preparation.txt",header = TRUE,sep = "\t")

# Check if there are any 0's present in data
xtabs(~admit+rank,data=data) 

# Explore Data

str(data)
dim(data)
length(data)
head(data)
tail(data)
names(data)
levels(data)
class(data$LungCap)
class(data$Age)
class(data$Height)
class(data$Smoke)
class(data$Gender)
class(data$Caesarean)
summary(data)
                    
                    #########################################
                    #                                       #
                    #     Trailing and Leading Spaces       #
                    #                                       #
                    #########################################

# Here we observed trailing and Leading spaces for Gender attribute, lets clean them
data$Gender <- trimws(data$Gender,which = c('right'))
data$Gender <- trimws(data$Gender,which = c('left'))
summary(data)
# we see these are clean now
                    
                    #########################################
                    #                                       #
                    #           Punctuation Marks           #
                    #                                       #
                    #########################################

summary(data)
#Here we see some punctuation marks for Smoke Atrribute, lets clean them
data$smoke <- gsub("[[:punct:]]|[[:digit:]]|(http[[:alpha:]]*:\\/\\/)","",data$Smoke)
library(dplyr)
data1 <- select(data,c(1,2,3,7,5,6))
rm(data)
data <- data1
rm(data1)
                      #########################################
                      #                                       #
                      #           OUTLIERS                    #
                      #                                       #
                      #########################################

library(dplyr)
library(tidyr)
library(purrr)
outlierremoval <- function(dataframe){
  dataframe %>%
    select_if(is.numeric) %>%         #selects on the numeric columns
    map(~ .x[!.x %in% boxplot.stats(.)$out]) #%>%
  # not clear whether we need to output as a list or data.frame
  # if it is the latter, the columns could be of different length
  # so we may use cbind.fill
  # { do.call(rowr::cbind.fill, c(., list(fill = NA)))}
  
}

outlierremoval(data)
 ##################################################################################
x <- c(1, 60, 2, 1, 4, 4, 1, 1, 6, -30, 70)
x[which(x %in% boxplot.stats(x)$out)]  # to detect outliers
##################################################################################
boxplot(data$LungCap) # No outliers
boxplot(data$Age)   # In age we see lot of outliers
boxplot(data$Height) # No outliers
summary(data$Age)
##################################################################################
# Set benchmarks to handle the outliers

IQR <- 15.00 - 9.00
bench <- 15.00 + 1.5*IQR
bench

# Display outliers

data$Age[data$Age > bench]

# Replace outliers with NA values
# NOTE : You can replace , omit, remove outliers from data according to your requirment

data$Age[data$Age > bench]  <- NA

                        #########################################
                        #                                       #
                        #           MISSING VALUES              #
                        #                                       #
                        #########################################

# Install pre-requisite R-packages

install.packages("mice")
install.packages('VIM')

# Import Packages

library(mice)
library(VIM)

# Explore Data

str(data)

# Lets calculate what percentage data is missing

p <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,p)

# Plot the Data for Missing values

md.pattern(data)
md.pairs(data)

# Sum the missing values

sum(is.na(data$Age))
is.na(data)
which(is.na(data))

# Check exact rows of missing values

apply(is.na(data),2,which)

# we are replacing Height missing values with mean

mean(data$Height[!is.na(data$Height)])
mean(data$Height,na.rm = TRUE)
round(mean(data$Height,na.rm = TRUE),2)
data$Height[which(is.na(data$Height))] <- round(mean(data$Height,na.rm = TRUE),2)

# you can omit or replace the missing values according to your requirement 
# here I m replacing with mean

round(mean(data$Age,na.rm = TRUE))
data$Age[which(is.na(data$Age))] <- round(mean(data$Age,na.rm = TRUE))

######################## Data is Cleaned now ################
