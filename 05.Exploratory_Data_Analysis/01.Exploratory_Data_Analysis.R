################# Exploratory Data Analysis #################

# Understand what is Data first?
# Data is collection of facts,such as numbers, words,measurements,Observations or even just Description of things.

# There two types of Data
# 1. Qualitative Data --> is descriptive information.
# 2. Quantitative Data -> is numerical information (numbers).

# Quantitative Data Again have two types
# 1. Discret Data -> can only take certain values(like whole number).
# 2. Continuous Data -> can take only value (within the range).

# Exploratory Data Analysis is very important part of machine Learning it is used to get rough idea about data before
# building machine learning models

# EDA mainly consists of 3 major parts which are as below
# 1. Data Summarization -> when we summarize the essence of particulare Dataset using statistical measurements such as
# Mean,Median,Mode,variance,standard Deviation,IQR,min,max etc..
# 2. Data Visualization -> when we visually exploring the data by plotting various types of graphs such as histogram,barplot.
# 3. Data Normalization -> When we adjust the scale of the data i.e lets say we have weight and weight measurement in kg's 
# and we have age & age is measured in No of years. if you want to get weight & age on same scale. how would you do that?
# We will see it in step by step.


##################### We are doing this in practically on iris Dataset ###############################

# install pre-requisite packages
install.packages('stats')
install.packages("dplyr")
#Load libraries
library(stats)
library(dplyr)
# The IRIS data is availabe by default on R-studio
#Loading data onto mydata object
mydata <- iris
str(mydata)

################### 1. Data Summarization ################

###############################################################################################################
# The best way to summarize a particular dataset is report both the central tendency along with the           #
# appropriate measure of variability. for instance, the best way to summarize the dataset is to report        #
# the mean value and the standard deviation.However, in the second Dataset both the mean and SD are affected  #
# by the outlier value in this case we report the median and Inter quartile Range.                            #
###############################################################################################################

# take sepal Length as an Example as below

SL <- mydata$Sepal.Length

######## Central tendencies #####

# Calculate mean

mean(SL)

# Calculate median

median(SL)

# calculate Mode

mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

mode(SL)

################# Measure of Variability #####################

# 1. Calculate standard Deviation

sd(SL)

# 2.  Calculate Variance

var(SL)

# 3. Calculate IQR

quantile(SL)


##################### 2. Data Visualization  ############################################
#  when we visually exploring the data by plotting various types of graphs such as      #
# 1. Bar plot -> Bar plots are good when your data is in categories                     #
# such as ("Comedy","Drama", Iris Dataset has Species to plot                           #
# onlye species bar plots are useful) It is best to leave gaps                          #
# between the bars of a Bar Graph,so It doesn't look like a Histogram                   #
# 2. Histogram -> Histograms are useful when you have continuous                        #
# Data (such as person's height) then use a histogram                                   #
# 3. Bar charts & Pie Charts -> are appropriate for summarizing                         #
# the distribution of a categorical variables.                                          #
# 4. Box plot and Grouped Boxplots -> are appropriate for summarizing                   #
# the distribution of a numerica variables.                                             #
# 5. Stratified Boxplot -> are useful for examing the relationship between a categorical#
# variable and a numerica variable, within the strata or groups defined by              #  
# a third categorical variables.                                                        #
# 6. Stem and Leaf plots -> are appropriate for summarizing the distribution of         #
# a numeric variables and are most appropriate for smaller datasets.                    #  
# 7. Line Graphs -> are appropriate that shows information that is connected in some way#
# (such as change over time)                                                            #
# 8. Scatter plots -> are appropriate to show the relationship between two sets of data.#
#########################################################################################


str(mydata)

# for categorical data we are choosing species data and for 
# continuous data we have already taken Sepal length.

# Numeric Data - Histogram

hist(SL)

# Categorical Data - Barplot
Species <- mydata$Species
table(Species)
barplot(table(Species))

####################### 3. Data Normalization ###############

str(mydata)
# we only need numeric data to Normalize the data.
mydata_numeric <- select(mydata,c(1,2,3,4))
str(mydata_numeric)

# Normalize Dataset using (Z-score) - All varialble on the same scale
zscore_mydata <- scale(mydata_numeric)
View(zscore_mydata)

## We can normalize Data in R using below equations as well
# 1. (x -min(x))/(max(a)-min(x)) --> by using min max
# 2. z_core <- (x - min(x))/sd(x) --> by using z score
# 3. (x)/(10^j) --> (j is digits of greater value) by using Decimal Scaling
