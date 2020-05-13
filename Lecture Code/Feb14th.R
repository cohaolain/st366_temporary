dat   <- data.frame(t=seq(0, 2 * pi, by=0.1))
dat$x <- 16 * sin(dat$t)^3
dat$y <- {function(t) 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)}(dat$t)
par(mar=c(3,2,3,2))
with(dat, plot(x, y, type="l", col="hotpink", main="Happy Valentine's Day", 
               col.main="red", bty="n", axes=FALSE, xlab="", ylab="", lwd=2))
mtext(toupper("xoxo"), side=3, col="red")
mtext(paste0("From: ", "Micheál"), side=1, line=1, col="black", font=4)
mtext(paste0("To: ", "Leo"), side=1, line=2, col="black", font=4)