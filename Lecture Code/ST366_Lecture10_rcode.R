# matrix calculations

a <- matrix(1:9, nrow=3)
b <- rbind(a, 1:3)
# b <- rbind(a, 1)

nrow(b)
ncol(b)
dim(b)

det(a) # determinant
diag(a) # diagonal elements
diag(1:4) # constructs a diagonal matrix
diag(4) # constructs a 4x4 identity matrix

t(b) # transpose

d <- rbind(1:3, a)
d
b - d  # subtract elements
b / d
b * d # element-wise multiplication
b %*% d

t(b) %*% d # matrix multiplication

crossprod(b, d)  # t(b) %*% d
tcrossprod(b, d) # b %*% t(d)

# solving
a[3,3] <- 20
a
b <- c(2,3,1)
x <- solve(a, b)
x
a %*% x # check the result. Notice x is interpreted as a column vector.

ai <- solve(a)
ai %*% a # should be the identity

all.equal(ai %*% a, diag(3))
# all.equal(ai %*% a, diag(rep(1,3)))

# example: markov chains
p <- matrix(c(.1,.2,.1,.4,.2,.2,.4,.4,.3,.4,.4,.2,.4,.2,.1,0), nrow=4)
p
apply(p,1,sum)
rowSums(p)

p %*% p        ## two-step transition probabilities
p %*% p  %*% p ## three-step transition probabilities

### Note that p %*% p is the correct way to multiply p by itself:
### p^2 would just square every element of p.

# property 1
pi <- p
for(i in 1:8) pi <- pi %*% p
pi

# property 2
q <- rep(1,4)/4
qmat <- matrix(0,ncol=4,nrow=9)
qmat[1,] <- q
for(i in 1:8) {
  q <- q %*% p
  qmat[i+1,] <- q
}
qmat

barplot(t(qmat), col=2:5)

# property 3
solve(diag(4) - t(p), rep(0,4)) # gives an error

A <- diag(4) - t(p)
A[4,] <- 1
A
solve(A, c(0,0,0,1))

# eigenvalues and eigenvectors
a
eigen(a)

eig <- eigen(a) # this is a list of two components
val1 <- eig$values[1]
vec1 <- eig$vectors[,1]
val1
vec1
a %*% vec1
val1 * vec1
a %*% vec1 - val1 * vec1

val2 <- eig$values[2]
vec2 <- eig$vectors[,2]
a %*% vec2
val2 * vec2
a %*% vec2 - val2 * vec2

val2 <- eig$values[3]
vec2 <- eig$vectors[,3]
a %*% vec3
val3 * vec3
a %*% vec3 - val3 * vec3

# example: Markov chains (cont'd)
eig <- eigen(t(p))
eig
v <- eig$vectors[,1]
(pi <- v/sum(v))

## example: least squares estimation
n <- 80
set.seed(123); x <- rnorm(n)
true.beta <- c(2, .5)
true.sigma2 <- 3
set.seed(12); y <- rnorm(n, true.beta[1] + true.beta[2]*x, sqrt(true.sigma2))

X <- model.matrix(~ x)
# same as cbind(1, x)

(beta.hat <- solve(crossprod(X, X)) %*% crossprod(X, y))
(beta.hat2 <- solve(t(X) %*%X ) %*% t(X) %*% y)

res <- lm(y ~ x)
coef(res)
#