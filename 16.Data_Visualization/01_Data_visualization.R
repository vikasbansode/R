# Visualization

setwd("D:\\from E\\ML")

vehicle <- read.csv("vehicle.csv",header = T,sep = ",")

str(vehicle)
head(vehicle)
tail(vehicle)

#Histogram
hist(vehicle$lc)

hist(vehicle$lc,
     breaks = 30,
     xlim = c(0,1500),
     ylim = c(0,1000),
     col = 'green',
     xlab = 'Labor cost',
     ylab = 'Frequency',
     main = 'Histogram of Labor Cost'
     )

# Bar plot

plot(vehicle$State)

# pie chart

pie(table(vehicle$State))

# Box plot

boxplot(Mileage ~ State,data = vehicle)

# scatter plot

plot(vehicle$lh,vehicle$lc)

# Multi-plot

pairs(vehicle[2:6])
