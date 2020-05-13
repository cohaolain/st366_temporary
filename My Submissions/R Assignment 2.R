# Ciarán Ó hAoláin - 17309103

## Q1
# (a)
our_subset <- cars[cars[, "dist"] >= 20 & cars[, "dist"] <= 25,]
our_subset
#     speed dist
# 4      7   22
# 13    12   20
# 14    12   24
# 24    15   20

# (b)
mean(our_subset[, "speed"])
# 11.5

# (c)
subset(cars, dist >= 20 & dist <= 25)
#     speed dist
# 4      7   22
# 13    12   20
# 14    12   24
# 24    15   20


## Q2
barplot(
  VADeaths,
  beside = TRUE,
  legend = TRUE,
  main = "Death rates in Virginia",
  ylab = "Deaths per 1000",
  ylim = c(0, 80)
)

## Q3
# (a)
setwd("/home/ciaran/st366/")
eupop <- read.table("eupop.txt", header = TRUE, row.names = 1)
head(eupop)
#            p014 p1544 p4564 p65.     pop
# Austria    17.0  44.2  23.4 15.5  8082.8
# Belgium    17.7  42.2  23.5 16.6 10213.8
# Denmark    18.2  41.6  25.3 14.9  5313.6
# Finland    18.4  40.8  26.1 14.7  5159.6
# France     19.0  42.5  22.8 15.8 58973.2
# Luxembourg 18.8  43.7  23.2 14.3   429.2

# (b)
ie_uk <- as.matrix(eupop[c("Ireland", "UK"),-5])

barplot(
  ie_uk,
  legend.text = c("Ireland", "UK"),
  main = "Population Breakdown (by Age) of Ireland vs UK",
  beside = TRUE
)

# (c)
barplot(ie_uk["Ireland",],
        main = "Population Breakdown (by Age) of Ireland vs UK",
        ylim = c(-50, 50),
        col = "green")

barplot(-ie_uk["UK",],
        col = "blue",
        add = TRUE)

# (d)
barplot(t(eupop[,-5]),
        main = "Population Breakdown (by Age) of Ireland vs UK",
        las = 2,
        legend.text = TRUE)

## Q4
# (a)
gen_normal <- rnorm(100)
# This wasn't quite clear.
# The question specifies:
#   size = 100
#   n    = 20
#   p    = .25
# However, the arguments n, size in the function rbinom
# have the opposite meaning to what seems to be suggested in the question.
# We are asked for a sample of size = 100, with n = 20.
# Confusingly, this is done correctly by writing:
#   rbinom(n = 100, size = 20, p = .25)
# Since n is the number of observarions in rbinom
# I will henceforth assume we want a sample of size 100,
# i.e. .100 observations, with n =20 trials each.
gen_binom <- rbinom(n = 100, size = 20, p = .25)

# (b)
qqplot(gen_normal, gen_binom)
qqnorm(gen_binom)
# The binomial distribution appears to approximate that of the normal,
# albeit with a very discrete nature (i.e. with a low resolution).

# (c)
gen_binom_c <- rbinom(n = 100, size = 200, p = 5)
qqplot(gen_normal, gen_binom_c)
qqnorm(gen_binom_c)
# The binomial distribution once again appears to approximate that of the normal,
# this time with a higher resolution / higher degree of granularity.

## Q5
# (a)
fbinom <-
  function(k, n, p)
    sum(sapply(0:k, function(i)
      choose(n, i) * p ** i * (1 - p) ** (n - i)))

# (b)
fbinom(5, 10, 0.3)
# 0.952651
fbinom(100, 30, 0.3)
# 1

# (c)
binom_compare <-
  function(k, n, p)
    all.equal(fbinom(k, n, p), pbinom(k, n, p))

# (d)
binom_compare(5, 10, 0.3)
# TRUE (both not equal with standard == operator)
binom_compare(100, 30, 0.3)
# TRUE (both not equal with standard == operator)


## Q4
# (a)
geo_mean <- function(x)
  prod(x) ** (1 / length(x))
prod(rexp(500, rate = 1 / 100))

# (b)
geo_mean_alt <- function(x)
  exp((1 / length(x)) * sum(sapply(x, log)))

# (c)
exp_dist_sample <- rexp(1000, rate = 1 / 100)
ans_a <- geo_mean(exp_dist_sample)
ans_b <- geo_mean_alt(exp_dist_sample)
ans_a - ans_b
# ans_a is Inf, because the product inside the geo_mean function
# can evaluate to Inf, because the product becomes a number too large
# for R to store. This renders the computation useless.

# (d)
differences <-
  replicate(1000, (function(sample)
    geo_mean_alt(sample) - mean(sample))(rexp(100)))

# (e)
hist(differences)
mean(differences)
# -0.4334288
