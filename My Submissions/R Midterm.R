# Ciarán Ó hAoláin - 17309103

# Q1

install.packages("MoEClust")
library("MoEClust")

# (a)
ais
data <- ais
plot(RCC ~ BMI, data = ais, col = sex)

# (b)
mean(ais$RCC)
# 4.718614

# (c)
mean(ais$RCC[ais$sex == "male"])
# 5.026569

# (d)
mean(ais$RCC[ais$sport == "Tennis"])
# 4.790909

# (e)
mean(ais$RCC[ais$sport == "Tennis" & ais$sex == "male"])
# 5.26

# (f)
boxplot(RCC ~ sex, data = ais)


# Q2
# (a)
dgumbel <- function(x, beta, mu) {
  z <- (x - mu) / beta
  return((1 / beta) * exp(-(z + exp(-z))))
}
dgumbel(seq(-1, 1), 0.5, -1)
# 0.73575888 0.23640990 0.03596646

# (b)
dgumbel2 <- function(x, mu = 0, beta = 1) {
  z <- (x - mu) / beta
  return((1 / beta) * exp(-(z + exp(-z))))
}

# (c)
dgumbel3 <- function(x, mu = 0, beta = 1) {
  if (beta <= 0) {
    return(stop("beta must be greater than zero"))
  }
  z <- (x - mu) / beta
  return((1 / beta) * exp(-(z + exp(-z))))
}

# (d)
## (i)
x <- seq(-5, 20, 0.01)

## (ii)
dg1 <- dgumbel3(x, beta = 4, mu = 2)

## (iii)
dg2 <- dgumbel3(x, beta = 2, mu = 1)

## (iv)
dg3 <- dgumbel3(x)

## (v)
plot(
  dg1 ~ x,
  ylim = c(0, 0.2),
  type = "l",
  col = "red",
  ylab = "Densities"
)

## (vi)
lines(dg2 ~ x, col = "green")
lines(dg3 ~ x, col = "blue")


# Q3
# (a)
N <- rep(NA, 20)
r <- 2.5
K <- 25
N[1] <- 6
while (sum(is.na(N)) != 0) {
  i <- sum(!is.na(N))
  N[i + 1] <- N[i] * exp(r * (1 - N[i] / K))
}
N

# (b)
Ricker <- function(N1, r, K, time) {
  N <- rep(NA, time)
  N[1] <- N1
  while (sum(is.na(N)) != 0) {
    i <- sum(!is.na(N))
    N[i + 1] <- N[i] * exp(r * (1 - N[i] / K))
  }
  return(N)
}
# Test
Ricker(6, 2.5, 25, 20)

# (c)
r1 <- Ricker(6, 2.5, 25, 20)
r2 <- Ricker(6, 3, 25, 20)
r3 <- Ricker(6, 3.5, 25, 20)
plot(
  r1,
  type = "l",
  col = "red",
  ylab = "Ricker value",
  xlab = "Time",
  ylim = c(min(c(r1, r2, r3)), max(r1, r2, r3))
)
lines(r2, col = "blue", lwd = 0.75)
lines(r3, col = "green", lwd = 0.5)

# (d)
# N is not a vector, or data type intended to store a list of values.
# Thus we get a warning every time we try to "index" an element of it.
# So the initial 6 just keeps being replicated due to incompatible types.
