
# Create a vector

x <- 1:5
x

y <- 6:10
y

plot(x,y)

abline(lm(y~x),col='red')

x1 <- c(1,3,5,7,9)
x1

gender <- c("male","female")
gender

# Creating vectors using seq function

seq(from=1,to=7,by=1)

seq(from=1,to=7,by=1/3)

# Creat a list

list_data <- list("Red","Green",c(21,32,11),TRUE,51.23,119.1)
list_data

# Create Matrices

m <- matrix(c(3:14),nrow = 4,byrow = FALSE) # Column wise

n <- matrix(c(3:14),nrow = 4,byrow = TRUE) # row wise

matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow = TRUE) #Row wise fashion
matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow = FALSE) # Columnwise fashion

# Create Arrays

v1 <- c(5,93)
v2 <- c(10,11,12,13,14,15)
result <- array(c(v1,v2),dim = c(3,3,2))
result

# Create factors

data <- c("east","west","south","North")
data
print(is.factor(data))

# Create Data.frame

emp_data <- data.frame(
  emp_id = c(1:5),
  emp_name = c("Rick","Dan","Mick","Ryan","Gary"),
  salary = c(625,515,611,720,825),
  join_date = as.Date(c("2012-01-01","2013-09-23","2015-05-08",
                        "2016-08-25","2017-02-28")),stringsAsFactors = FALSE)
emp_data

# Variables assignment
x <- 11

# Here x is variable holding value of 11
print(x)

y <- 7
y


# Arithmatic Operations
10+14
7+9

a <- 10  # "a" variable holding value 10
b <- 12  # "b" variable holding value 12

# Arithmatic operations on variables
a + b
c <- a + b
a/b
a^2
sqrt(a)
log(a)
log2(a)
exp(b)
abs(-14)

x <- 1:5
y <- c(1,3,5,7,9)
x + 10
x - 10
x * 10
x/2
x
y

# Repete function

rep(1,times=5)

rep("vikas",times=5)

rep(1:3,times=5)

rep(seq(from=2,to=5,by=0.25),times=5)

rep(c("m","f"),times=5)


# Slicing and Dicing
# Vector

y[3]

y[-3]

y[c(1,5)]

y[-c(1,5)]

y[y>6]
y[y<6]


# List

x <- list(a=1, b=2, c=3)

x[1]
x[-1]
x[x<2]
x[x>2]

# Matrices

mat <- matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow = FALSE)
mat

mat[c(1,3),2] # row 1 and 3 column 2
mat[c(2,3),3]
mat[,1]
mat[1,]
mat[-1,]

# Arrays
v1 <- c(5,93)
v2 <- c(10,11,12,13,14,15)
result <- array(c(v1,v2),dim = c(3,3,2))
result

result[2]
result[2,1,1]
result[2,3,1]
result[2,,]
result[1,3,]

# Factors
data <- c("east","west","south","North")
data

data[1]
data[2]

for(i in data){
  print(i)
}

# Data Frame
df <- data.frame(
  emp_id = c(1:5),
  emp_name = c("Rick","Dan","Mick","Ryan","Gary"),
  salary = c(625,515,611,720,825),
  join_date = as.Date(c("2012-01-01","2013-09-23","2015-05-08",
                        "2016-08-25","2017-02-28")),stringsAsFactors = FALSE)
df
names(df)
dim(df)
head(df)
tail(df)
str(df)
levels(df$emp_id)

# All rows and All columns

df[,]

# First row and all columns

df[1,]

# First two rows and all columns

df[1:2,]

# First and third row and all columns

df[c(1,3),]

# First row and second and third column

df[1,2:3]

# First, Second row and second and Third columns

df[1:2,2:3]

# Just first column with All rows

df[,1]

# First and Third column with All rows

df[,c(1,3)]

# First column as data frame

as.data.frame(df[,1],drop=FALSE)

# Element at 2nd row, third column

df[2,3]
