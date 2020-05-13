d <- 1:4
names(d) <- LETTERS[1:4]
barplot(d, main = "Made up data",
        ylab = "Count", col = 1:4)
barplot(d, main = "Made up data",
        ylab = "Count", col = c("orange", "yellow", "purple", "brown"))
palette()
palette(c("orange", "yellow", "purple", "brown"))
barplot(d, main = "Made up data",
        ylab = "Count", col = 1:4)
palette("default")

barplot(d, main = "Made up data",
        ylab = expression(y^2), col = 1:4)
barplot(d, main = "Made up data",
        ylab = expression(y[2]), col = 1:4)
barplot(d, main = "Made up data",
        ylab = expression(mu), col = 1:4)

d1 <- cbind(d, rpois(4,7))
colnames(d1) <- c("var1","var2")
d1

par(mfrow = c(1,3))
barplot(d1, main = "Made up data", ylab = "Count",
        col = 1:4)
barplot(d1, main = "Made up data", ylab = "Count",
        col = 1:4, beside = TRUE, legend = TRUE)
barplot(t(d1), main = "Made up data", ylab = "Count",
        col = 5:6, beside = TRUE, legend = TRUE)
dev.off()

## not setting ylim
barplot(d1[,1], main = "Made up data",
        ylab="Count", col = 5)
barplot(-d1[,2], col = 6, add = TRUE)

## not using add = TRUE
barplot(d1[,1], main = "Made up data", ylab="Count",
        col = 5, ylim = c(-11,4))
barplot(-d1[,2], col = 6)

## putting it all together
barplot(d1[,1], main = "Made up data", ylab="Count",
        col = 5, ylim = c(-11,4))
barplot(-d1[,2], col = 6, add = TRUE)

# getwd()
# note the use of the forward slash in the call to setwd()
setwd("D:/Google Drive/Maynooth/ST366 Statistical Computing with R/2019-2020/Data/")
#eupop <- read.table("eupop.txt", header = TRUE)
eupop <- read.table("eupop.txt", header = TRUE,
                    row.names = 1)
head(eupop)

eupop <- as.matrix(eupop)
barplot(eupop["Ireland",-5], main = "Population of Ireland")

barplot(t(eupop[c("UK","Ireland"),-5]),
        main = "Population of UK and Ireland")
barplot(t(eupop[c("UK","Ireland"),-5]),
        main = "Population of UK and Ireland",
        beside = TRUE, legend = TRUE)

eusmall <- eupop[1:4,-5]
dotchart(eusmall)
dotchart(t(eusmall))

dotchart(t(eusmall), xlim = c(0,50),
         pch = 1:4, col = 1:4)

dev.off()
par(mfrow = c(1,2)); par(mar = c(1,1,1,1))
pie(eupop["Ireland",-5],
    main = "Population of Ireland")
pie(eupop["UK",-5],
    main = "Population of UK")
dev.off()

x <- rnorm(1000)
par(mfrow = c(1,2))
hist(x, breaks = "Sturges", main = "Sturges breaks") # the default
hist(x, breaks = 35, main = "35 breaks")

boxplot(iris[1:50,1:4])

boxplot(iris[,"Sepal.Length"] ~ iris[,"Species"])
boxplot(Sepal.Length ~ Species, data = iris)


#cols <- rep(c("magenta","black","yellow"), each=50)
cols  <- c("magenta", "black", "yellow")
cols  <- cols[as.numeric(iris$Species)]
plot(Sepal.Width ~ Sepal.Length, data = iris,
     col = cols)
plot(Sepal.Width ~ Sepal.Length, data = iris,
     pch = 21, col = 1, cex = 1.5, bg = cols)

plot(iris[,-5], pch = 21, bg = cols, col = 1, cex = 1.2)

### extra -- using lattice
library(lattice)
xyplot(Sepal.Width ~ Sepal.Length, data = iris,
       groups = Species)

iris2 <- iris[sample(1:nrow(iris), 50),]
iris2

require(lattice)
trellis.par.set(superpose.symbol = list(fill = c("magenta","black","yellow")))
xyplot(Sepal.Width ~ Sepal.Length, data = iris, pch = 21,
       col = 1, cex = 1.5, groups = Species,
       xlim = c(4,8.5), ylim = c(1.75,4.75))
xyplot(Sepal.Width ~ Sepal.Length, data = iris2, pch = 21,
       col = 1, cex = 1.5, groups = Species,
       xlim = c(4,8.5), ylim = c(1.75,4.75))
###

### extra - GGally
require(GGally)
ggpairs(data = iris, 
        mapping = aes(colour = Species, alpha = .4))
###

require(MASS)
parcoord(iris[,-5])
parcoord(iris[,-5], col = iris[,5], var.label = TRUE)
### variables on columns
parcoord(iris[,-5], col = cols)

setosa <- colMeans(iris[iris$Species == "setosa",-5]) # subset(iris, Species == "setosa")[,-5]
versicolor <- colMeans(iris[iris$Species =="versicolor",-5])
virginica <- colMeans(iris[iris$Species =="virginica",-5])
irism <- rbind(setosa, versicolor, virginica)
#parcoord(irism, col = 1:3, lwd = 3, var.label = TRUE)
parcoord(irism, col = unique(cols), lwd = 3, var.label = TRUE)

x <- rnorm(50); y <- rnorm(50)
qqplot(x, y) # plot(sort(x), sort(y))

par(mfrow = c(1,2))
qqnorm(x); qqline(x)
qqnorm(y); qqline(y)

dev.off()
ppoints(50)
(0:49 + .5)/(50 - 1 + 2*.5)
qqnorm(x)
plot(qnorm(ppoints(50)), sort(x))

curve(dnorm, xlim = c(-4, 4))
curve(pnorm, xlim = c(-4, 4))
curve(qnorm, xlim = c(0, 1))

require(hnp)
hnp(x, half = FALSE, scale = TRUE)
hnp(y, half = FALSE, scale = TRUE)
#