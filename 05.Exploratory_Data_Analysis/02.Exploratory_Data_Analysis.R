# Create dummy variables

df <- data.frame('A'=c('high','medium','low'),
                 'B'=c(10,20,30)
                 )
df

for(i in unique(df$A)){df[paste("A",i,sep = "_")] <- ifelse(df$A==i,1,0)}
df

# EDA

iris <- iris
summary(iris)

levels(iris$Species)

table(iris$Species)

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)


# univariate Analysis

summary(iris)
table(iris$Species)

# Bivariable Analysis

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


# Pair plot
library(psych)
pairs.panels(iris[-5],gap=0,bg=c("red","yellow","blue")[iris$Species],pch=21)

pairs(iris)
plot(iris[,1:4],
     main="Relationships between characteristics of iris flowers",
     pch=19,
     col="blue",
     cex=0.9)

# Polynomial Regression

x <- runif(1000,min = -3,max = 3)
plot(x,main = 'linear',col='blue') 
plot(x,x**2,main = "degree 2",col='blue')
plot(x,x**3,main = "degree 3",col='blue')
plot(x,x**4,main = "degree 4",col='blue')
plot(x,x**5,main = "degree 5",col='blue')
plot(x,x**6,main = "degree 6",col='blue')
plot(x,x**7,main = "degree 7",col='blue')


p <- 0.5
q <- seq(0,100,1)
y <- p*q
plot(q,y,type='l',col='red',main='Linear relationship')

y <- 450 + p*(q-10)^3
plot(q,y,type='l',col='navy',main='Nonlinear relationship',lwd=3)

set.seed(20)
q <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (q-10)^3
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)

model <- lm(noisy.y ~ poly(q,3))
model <- lm(noisy.y ~ x + I(X^2) + I(X^3))
