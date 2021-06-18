rm(list = ls())

source("StepWise_v5.R")

## Nonparametric function estimation

par(mfrow = c(1, 3), mar = c(5.1, 5.1, 1.1, 2.1))

x <- seq(0,1,length.out = 500)
y <- sin(2 * pi * x)^3 + rnorm(500, sd = 0.35)

dimension = 10000
degree = 3
knots = knots_quantile(x, dimension = dimension, order = degree + 1, type = "bs")
B = bsplines(x, knots = knots, order = degree + 1, derivative = 0)

fit_lm = lm(y ~ B - 1)

plot(x, y, cex.lab = 2, cex.axis = 2)
lines(x, fit_lm$fitted.values, lwd = 2)
for(i in 1 : (length(knots) - 2 * (degree + 1)))
{
   abline(v = knots[i + 4], col = "grey", lty = 2)
}

############################################################
library(MASS)
data("Boston")
plot(Boston$lstat, Boston$medv, xlab = "lower status of the population (%)",
     ylab = "median value of owner-occupied homes",
     cex.lab = 2, cex.axis = 2)

dimension = 5
degree = 3
knots = knots_quantile(Boston$lstat, dimension = dimension, order = degree + 1, type = "bs")
B = bsplines(Boston$lstat, knots = knots, order = degree + 1, derivative = 0)

fit_lm = lm(Boston$medv ~ B - 1)
lines(sort(Boston$lstat), fit_lm$fitted.values[order(Boston$lstat)], lwd = 2)

############################################################
library(loon.data)
data("bone")

x = bone$age
y = bone$rspnbmd
z = bone$sex
tz = transform(z,
               class1 = ifelse(z == "male", 1, 0),
               class2 = ifelse(z == "female", 1, 0))[,2:3]
x_m = x[z == "male"]
x_f = x[z == "female"]

fit = StepWise(x, tz, y, max_knots = 50, order = 4,
               criterion = "BIC", max_iter = 1000, epsilon = 1e-5)
fit_m = fit$fitted_values[z == "male"]
fit_f = fit$fitted_values[z == "female"]

plot(x, y, type = "n", xlab = "age",
     ylab = "Relative spinal bone mineral density",
     cex.lab = 2, cex.axis = 2)
points(x, y, col = "grey")
lines(sort(x_m), fit_m[order(x_m)], lty = 1, lwd = 2)
lines(sort(x_f), fit_f[order(x_f)], lty = 3, lwd = 2)
abline(v = fit$knots, col = "grey", lty = 2)
legend("topright", c("male", "female"),
       col = "black",
       lty = c(1, 3), lwd = 2,
       bty = "n", cex = 1.5)

## Splines
par(mfrow = c(1, 2), mar = c(3.1, 3.1, 1.1, 3.1))

x <- seq(0,1,length.out = 500)
y <- sin(4 * pi * x) + cos(4 * pi * x) + 5 * sin(5 * pi * x) + rnorm(500, sd = 0.35)

dimension = 10
degree = 1
knots = knots_quantile(x, dimension = dimension, order = degree + 1, type = "bs")
B = bsplines(x, knots = knots, order = degree + 1, derivative = 0)

fit_lm = lm(y ~ B - 1)

plot(x, y, cex.lab = 2, cex.axis = 2, type = "n",
     bty = "n", yaxt = "n", ann = FALSE)
lines(x, fit_lm$fitted.values, lwd = 2)
for(i in 1 : (length(knots) - 2 * (degree + 1) + 2))
{
   abline(v = knots[i + degree], col = "grey", lty = 2)
}
###########################################################
y <-sqrt(x + 1) + 2 * sin(2 * pi * (x-2))^3 + cos(2 * pi * (x-3)^2) + rnorm(500, sd = 0.35)

dimension = 10
degree = 3
knots = knots_quantile(x, dimension = dimension, order = degree + 1, type = "bs")
B = bsplines(x, knots = knots, order = degree + 1, derivative = 0)

fit_lm = lm(y ~ B - 1)

plot(x, y, cex.lab = 2, cex.axis = 2, type = "n",
     bty = "n", yaxt = "n", ann = FALSE)
lines(x, fit_lm$fitted.values, lwd = 2)
for(i in 1 : (length(knots) - 2 * (degree + 1) + 2))
{
   abline(v = knots[i + degree], col = "grey", lty = 2)
}

## splines basis
source("PS_function.R")
source("PS_function_B.R")

par(mfrow = c(1, 2), mar = c(3.1, 3.1, 1.1, 3.1))

N = 500
x = seq(0, 1, length = N)
X = Psplines(x, num_knots = 16, degree = 3)
matplot(x, X, type = "l", cex.lab = 2, cex.axis = 2, xlab = "")

dimension = 16 # 1, x, x^2, x^3, number of knots, 1 + degree + number of knots
degree = 3
knots = knots_quantile(x, dimension = dimension, order = degree + 1, type = "bs")

B = bsplines(x, knots = knots, order = degree + 1, derivative = 0)
matplot(x, B, type = "l", cex.lab = 2, cex.axis = 2, xlab = "")


## contour
par(mar = c(5.1,5.1,1.1,1.1))
x <- seq(-10, 10, length.out = 200)
y <- x
f <- function(x,y) 1000*(((cos(pi/4) * (x-6))+sin(pi/4) * (y-7))/4)^2 + 1000 * (((sin(pi/4) * (x-6))-(cos(pi/4) * (y-7)))/2)^2 
contour(x, y, outer(x, y, f), nlevels = 100, bty = "n",
        xlab = expression(beta[1]), ylab = expression(beta[2]),
        cex.lab = 2, cex.axis = 2, col = "grey")

xx <- c(-8, -2, -2, 2, 2, 5, 5, 5.7, 5.7, 5.9, 5.9)
yy <- c(-8, -8,  1, 1, 5, 5, 6,   6, 6.8, 6.8, 6.8)

for(i in 1 : length(xx))
{
   lines(xx[i:(i+1)], yy[i:(i+1)])
   points(xx[(i+1)], yy[(i+1)], pch = 16, cex = 1.5)
   if(i == 10) break
}

points(6, 7, pch = 13, cex = 2, col = "red")
points(-8, -8, pch = 10, cex = 2, col = "blue")
legend(4.9, -6.9, c("Initial value", "CDA solution", "Global minimizer"),
       pch = c(10, 16, 13), col = c("blue", "black", "red"), bty = "n",
       cex = 1.3)

