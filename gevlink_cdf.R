library(VGAM) #to use cloglog()
x <- seq(-8, 8, len = 200)
x_m <- matrix(x, length(x),1)
beta <- matrix(c(-2,1),2,1)
beta[1] <- 0
plot(x_m, inverse_gev(x_m, beta, xi = -1/2), xlim = c(-4,4),lty= 3, xlab="eta", ylab = "Cumulative Distribution Function", type="l")
lines(x_m, inverse_gev(x_m, beta, xi = 1/2), lty = 2, add = TRUE)
lines(x_m, cloglog(beta[1]+beta[2]*x_m, inverse = TRUE), lty = 1, add = TRUE)
