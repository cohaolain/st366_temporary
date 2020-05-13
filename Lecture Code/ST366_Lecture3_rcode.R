rm(list = ls())

seq(0, 1, length.out=11)
seq(1, 9, by = 2)
?seq

c(1, c(2, c(3,4)))
# same as
c(1,2,3,4)

x <- c(0:3,NA)
x+1
x-x
1/x
x/x
?NaN

mean(x)
mean(x, na.rm=TRUE)

z <- rnorm(4)
typeof(z)
is.double(z)
is.numeric(z) # tests for doubles or integers

x <- 1:4
typeof(x)
is.integer(x)
is.double(x)
is.numeric(x)

y<- c(1,4,6)
typeof(y)
y_int<- c(1L,4L,6L)
typeof(y_int)

object.size(y)
object.size(y_int)

z
z1 <- as.integer(z)
z1
z2 <- trunc(z)
z2
typeof(z1)
typeof(z2)

x <- c(FALSE,TRUE,TRUE)
typeof(x)
is.numeric(x)
xn <- as.numeric(x) # coerces to numeric
xn
typeof(xn)

sum(x) # counts trues
mean(x) # proportion of trues

x <- 4:0
x > 2
x >= 2

#x = 0
x == 0

x0 <- x==0
x0
typeof(x0)
sum(x0) # coerces FALSE to 0 and TRUE to 1

x <- c(3,7,1,9,-1, 2, -3)
x > 1 & x < 9
x > 8 | x < 0
sum(x > 8 | x < 0) # does what?
sum(x[x > 8 | x < 0]) # does what? (see below!)
x != 3

myname <- c("Keefe","Murphy")
typeof(myname)

c(myname, 29)

x[3]
x[1:3]
x[c(2,4)]
x[-1] # all but the first element
myname[1]

x
x>2
x[x>2]
x[3] <- NA
## x[3] <- c(0,1) # warning!
x
x[!is.na(x)] # often useful

x <- c(6,8,12,2)
# x(1) # gives an error
x[5]
x[0]
x[c(TRUE,TRUE,FALSE,TRUE)]
x[c(TRUE,FALSE)]
x[FALSE]

marks <- c(Maths = 66, English = 88, Irish = 70)
names(marks)
marks[1]
marks["English"]
marks1 <- c(81, 40, 73)
names(marks1)
names(marks1) <- c("Maths", "English", "Irish")

edc <- c("JC","JC","AdvDeg","Deg","LC","Deg")
edn <- c(1,1,4,3,2,3)

ed <- factor(edc)
ed
levels(ed)
as.integer(ed)
ed1 <- factor(edc, levels=c("JC","LC","Deg","AdvDeg"))
ed1
as.numeric(ed1) # converts to numeric, ie a vector of doubles

dev.off() # clears device
par(mfrow=c(1,2)) # creates a panel of 1 row of 2 plots
plot(edn); plot(ed1)

# cex: "character expansion"
par(mfrow=c(1,2), cex.axis = 2, cex.lab = 2.2)
plot(edn, cex = 2); plot(ed1)

print(m1 <- matrix(1:6,nrow=2,ncol=3))
print(m2 <- matrix(1:6,nrow=2,ncol=3,byrow=TRUE))
is.matrix(m1)
is.matrix(1:4)
is.matrix(matrix(1:4, ncol = 1, nrow = 4))

m1[2,3]
m1[3]

m1[1,] #First row
m1[,2]  #Second column
m1[,c(1,3)]  #First and third column
m1[m1 > 2] # gives a vector

m1[,1:2] # first 2 columns
m1[,-(2:3)] # remove 2nd and 3rd columns
m1[,-(2:3), drop=FALSE] # keep the result as a matrix

cbind(rpois(5,5),1:5) # Binds columns

rownames(m1) <- LETTERS[1:2]
m1
colnames(m1) <- LETTERS[3:5]
m1
m1["A","D"]

nrow(m1)
ncol(m1)

dim(m1) # number of rows and columns

c(1:3,"abc")

?cars
cars[1:4,]
head(cars)
tail(cars)
is.matrix(cars)
is.data.frame(cars)

L3 <- LETTERS[1:6]
d <- data.frame(x = 1, y = 1:6, let = L3)
d
nrow(d)
ncol(d)

cbind(1,1:6,L3)

cars[1:4,]+1
cars[1:3,2]
cars[1:3,"dist"]
cars[cars[,2]>70,]
subset(cars, dist > 70) # alternative way of forming subsets

cars[cars[,2]>70,][1:5,]  # Explain this one!

attributes(cars)
str(cars) # structural summary
summary(cars) # statistical summary

dev.off()
X11()
plot(cars)

?chickwts
head(chickwts)
str(chickwts)
summary(chickwts)

chickwts[,2]
chickwts$feed # press TAB after typing chickwts$

levels(chickwts$feed)

par(mfrow = c(1,2))
plot(chickwts)
plot(weight ~ feed, data = chickwts)
## plot(response ~ factor, data = name.of.my.dataframe)
## plot(response ~ explanatory, data = name.of.my.dataframe)