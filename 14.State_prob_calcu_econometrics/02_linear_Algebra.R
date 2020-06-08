
########################## LINEAR ALGEBRA #####################
####### VECTORS ###########

# Creating vetors
a <- c(1, 3, 2)
a
# Transpose of vectors
t(a)

# Multiplying a vector by a number
7*a

# Sum of vectors

a <- c(1, 3, 2)
b <- c(2, 8, 9)

a + b

# (Inner) product of vectors

sum(a * b)

# The length (norm) of a vector
sqrt(sum(a * a))

# The 0-vector and 1-vector
rep(0, 5)

rep(1,5)

# Orthogonal (perpendicular) vectors
# Two vectors v1 and v2 are orthogonal if their inner product is zero
v1 <- c(1, 1)
v2 <- c(-1, 1)
sum(v1 * v2)

######## Matrices ###########

A <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 3)
A

A2 <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 3, byrow = T)
A2

# Multiplying a matrix with a number
7 * A
# Transpose of matrices
t(A)
# Sum of matrices
B <- matrix(c(5, 8, 3, 4, 2, 7), ncol = 3, byrow = T)
A+B
# Multiplication of a matrix and a vector
A %*% a

# Note difference
A * a

# Multiplication of matrices
A <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 2)
B <- matrix(c(5, 8, 4, 2), ncol = 2)
A %*% B

# Vectors as matrices
# Some special matrices
# - An n × n matrix is a square matrix
# - A matrix A is symmetric if A = A>.
# - A matrix with 0 on all entries is the 0-matrix and
# is often written simply as 0.
# - A matrix consisting of 1s in all entries is of written J.
# - A square matrix with 0 on all off-diagonal entries and elements d1, d2, . . . , dn
# on the diagonal a diagonal matrix and is often written diag{d1, d2, . . . , dn}
# - A diagonal matrix with 1s on the diagonal is called the identity matrix
# and is denoted I. The identity matrix satisfies that IA = AI = A.

# 1. 0-matrix and 1-matrix
matrix(0, nrow = 2, ncol = 3)
# 2. 1-matrix
matrix(1, nrow = 2, ncol = 3)

# 3. Diagonal matrix
diag(c(1, 2, 3))
# 4. identity matrix
diag(1, 3)
# Note what happens when diag is applied to a matrix:
diag(diag(c(1, 2, 3)))
diag(A)

# Inverse of matrices
# Some facts about inverse matrices are:
# - Only square matrices can have an inverse, but not all square
# matrices have an inverse.
# - When the inverse exists, it is unique.
# - Finding the inverse of a large matrix A is numerically 
# complicated (but computers do it for us).

# Finding the inverse of a matrix in R is done using the solve() function:
A <- matrix(c(1, 3, 2, 4), ncol = 2, byrow = T)
A
B <- solve(A)
B
A %*% B

# Solving systems of linear equations

A <- matrix(c(1, 2, 3, 4), ncol = 2)
b <- c(7, 10)
x <- solve(A) %*% b
x


# Trace

# Determinant

# Some additional rules for matrix operations

# Inverting an n × n matrix*
A <- matrix(c(2, 2, 3, 3, 5, 9, 5, 6, 7), ncol = 3)
A
AB <- cbind(A, diag(c(1, 1, 1)))
AB
# First, we make sure that AB[1,1]=1. 
# Then we subtract a constant times the
# first row from the second to obtain that AB[2,1]=0, 
# and similarly for the third row:
AB[1, ] <- AB[1, ]/AB[1, 1]
AB[2, ] <- AB[2, ] - 2 * AB[1,]
AB[3, ] <- AB[3, ] - 3 * AB[1, ]
AB                         

# Next we ensure that AB[2,2]=1. Afterwards we subtract 
# a constant times the second row from the third to obtain
# that AB[3,2]=0:

AB[2, ] <- AB[2, ]/AB[2, 2]
AB[3, ] <- AB[3, ] - 4.5 * AB[2, ]

# Now we rescale the third row such that AB[3,3]=1:
AB[3, ] <- AB[3, ]/AB[3, 3]
AB

# We then work our way up to obtain that 
# AB has zeros above the main diagonal:
AB[2, ] <- AB[2, ] - 0.5 * AB[3, ]
AB[1, ] <- AB[1, ] - 2.5 * AB[3, ]
AB

# Now we extract the three rightmost columns of AB into the matrix B. 
# We claim that B is the inverse of A, and this can be verified by 
# a simple matrix multiplication

B <- AB[, 4:6]
A %*% B
