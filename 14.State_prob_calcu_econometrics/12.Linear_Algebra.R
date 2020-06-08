
                # Introduction
# Linear algebra is a branch of mathematics that is widely 
# used throughout data science. Yet because linear algebra 
# is a form of continuous rather than discrete mathematics,
# many data scientists have little experience with it. 
# A good understanding of linear algebra is essential for understanding
# and working with many machine learning algorithms and especially deep 
# learning algorithms. Let us dive into the world of
# linear algebra with R:

# Creating Matrix

A <- matrix(data = 1:25, nrow = 5, ncol = 5, byrow = TRUE)
A

B <- matrix(data = 25:49, nrow = 5, ncol = 5, byrow = FALSE)
B

# Element-wise operations
# Addition
A + B

# substraction
A - B

# Multiplication
A * B

# Division
A / B

# Exponentiation
A ^ B

################### Basic Matrix Operations ############
# Transpose
t(A)
t(B)
# multiplication
A %*% B
B %*% A

# Outer Product
X <- matrix(data = 1:10, nrow = 10)
Y <- matrix(data = 21:30, nrow = 10)
X %o% Y

X %*% t(Y)

# Cross Product
crossprod(A)
t(A) %*% A
crossprod(A, B)
t(A) %*% B
t(A) %*% B

# Diagonal
diag(A)
diag(B)
diag(c(1, 2, 3))
diag(5)
diag(3)

#################### Combine Matrices #######################
# Horizontal Combine
cbind(A, B)
# Vertical
rbind(A, B)

# Column Means
colMeans(A)
colMeans(B)

# Row Means
rowMeans(A)
rowMeans(B)

# Column Sum
colSums(A)
colSums(B)

# Row sum
rowSums(A)
rowSums(B)

################## Advanced Matrix Operations ###################
# First let us try Cholesky Factorization in R, 
# which demands that Matrix should be positive definite:
S <- matrix(data = c(5, 1, 1, 3), nrow = 2)
R <- chol(S)
t(R) %*% R

# Now let us deal with Eigenvalues in R
y <- eigen(A)
y$values
y$vectors

# Let us move over to Moore-Pernrose Generalized Inverse in R:
library(MASS)
ginv(A)
A %*% ginv(A) %*% A

# QR Decomposition 
y <- qr(A)
y$qr

y$rank

y$qraux
y$pivot

# Making Inverse
solve(S)
S %*% solve(S)

#  Solve Linear Equation
y = Mx
y <- c(1, 2, 3)
M <- matrix(data = c(1, 3, 2, 5, 4, 5, 2, 1, 4), nrow = 3)
x <- solve(M, y)
x

# make Singular Value Decomposition
y <- svd(A)
y$d

y$u
y$v

# make a first order derivative and second derivative hessian matrix
library(numDeriv)
sc2.f <- function(x){
  n <- length(x)
  sum((1:n) * (exp(x) - x)) / n
}
sc2.g <- function(x){
  n <- length(x)
  (1:n) * (exp(x) - 1) / n
}

x0 <- rnorm(5)
hess <- hessian(func=sc2.f, x=x0)
hessc <- hessian(func=sc2.f, x=x0, "complex")
# Hessian = Jacobian of the gradient
jac <- jacobian(func=sc2.g, x=x0)
jacc <- jacobian(func=sc2.g, x=x0, "complex")

# Let us display the first order derivative matrix with complex method
jacc
hess
