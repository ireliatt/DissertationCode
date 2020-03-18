library(VGAM)

x <- seq(-5, 8, len = 200)
x_m <- matrix(x, length(x),1) #convert the list of x to a matrix
beta <- c(0,1)
curve(logit(x, inverse = TRUE), -5, 5, ylab = "Cumulative Distribution Function",xlab = "eta", lty = 1)
curve(probit(x, inverse = TRUE), -5, 5, lty = 2, add = TRUE)
curve(cloglog(x, inverse = TRUE), -5, 5, lty = 3, add = TRUE)
lines(x_m, inverse_gev(x_m, beta, xi = -1/2), lty = 4, add = TRUE)
