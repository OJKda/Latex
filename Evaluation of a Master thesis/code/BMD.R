rm(list = ls())

source("StepWise_v5.R")

library(loon.data)
data("bone")

# BME_scatterplot 5.5 x 10
# par(mfrow = c(1,2), mar = c(5.1, 5.1, 2.1, 2.1))
# x = bone$age
# y = bone$rspnbmd
# z = bone$sex
# 
# plot(x, y, bty = "n", type = "n", xlab = "age",
#      ylab = "Relative spinal bone mineral density",
#      cex.lab = 1.3, cex.axis = 1.2)
# points(x, y, col = "grey")
# points(x[z == "male"], y[z == "male"], pch = 2, col = "grey")
# points(x[z == "female"], y[z == "female"], col = "grey")
# legend("topright", c("male", "fmale"), col = "grey",
#        pch = c(1, 2), bty = "n", cex = 1.1)
# 
# 
# bone_ethnic <- na.omit(bone)
# x <- bone_ethnic$age
# y <- bone_ethnic$rspnbmd
# z <- bone_ethnic$ethnic
# 
# plot(x, y, bty = "n", type = "n", xlab = "age",
#      ylab = "Relative spinal bone mineral density",
#      cex.lab = 1.3, cex.axis = 1.2)
# points(x, y, col = "grey")
# points(x[z == "White"], y[z == "White"], col = "grey")
# points(x[z == "Hispanic"], y[z == "Hispanic"], pch = 0, col = "grey")
# points(x[z == "Asian"], y[z == "Asian"], pch = 2, col = "grey")
# points(x[z == "Black"], y[z == "Black"], pch = 3, col = "grey")
# legend("topright", c("White", "Hispanic", "Asian", "Black"),
#        pch = c(1,0,2,3), col = "grey",
#        bty = "n", cex = 1.1)

# fit 5.5 x 10
par(mfrow = c(1,2))
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

plot(x, y, bty = "n", type = "n", xlab = "age",
     ylab = "Relative spinal bone mineral density",
     cex.lab = 1.3, cex.axis = 1.2)
points(x, y, col = "grey")
lines(sort(x_m), fit_m[order(x_m)], lty = 1, lwd = 2)
lines(sort(x_f), fit_f[order(x_f)], lty = 3, lwd = 2)
abline(v = fit$knots, col = "grey", lty = 2)
legend("topright", c("male", "female"),
       col = "black",
       lty = c(1, 3), lwd = 2,
       bty = "n", cex = 1.1)


bone_ethnic <- na.omit(bone)
x <- bone_ethnic$age
y <- bone_ethnic$rspnbmd
z <- bone_ethnic$ethnic

tz = transform(z,
               class1 = ifelse(z == "White", 1, 0),
               class2 = ifelse(z == "Hispanic", 1, 0),
               class3 = ifelse(z == "Asian", 1, 0),
               class4 = ifelse(z == "Black", 1, 0))[, 2:5]
x_w = x[z == "White"]
x_h = x[z == "Hispanic"]
x_a = x[z == "Asian"]
x_b = x[z == "Black"]

fit = StepWise(x, tz, y, max_knots = 50, order = 4,
               criterion = "BIC", max_iter = 1000, epsilon = 1e-5)

fit_w = fit$fitted_values[z == "White"]
fit_h = fit$fitted_values[z == "Hispanic"]
fit_a = fit$fitted_values[z == "Asian"]
fit_b = fit$fitted_values[z == "Black"]

plot(x, y, bty = "n", type = "n", xlab = "age",
     ylab = "Relative spinal bone mineral density",
     cex.lab = 1.3, cex.axis = 1.2)
points(x, y, col = "grey")
lines(sort(x_w), fit_w[order(x_w)], lty = 1, lwd = 2)
lines(sort(x_h), fit_h[order(x_h)], lty = 3, lwd = 2)
lines(sort(x_a), fit_a[order(x_a)], lty = 4, lwd = 2)
lines(sort(x_b), fit_b[order(x_b)], lty = 5, lwd = 2)
abline(v = fit$knots, col = "grey", lty = 2)
legend("topright", c("White", "Hispanic", "Asian", "Black"),
       col = "black",
       bty = "n", cex = 1.1, lwd = 2,
       lty = c(1,3,4,5))

# sex x ethnic 11 x 10
# bone_data <- na.omit(bone)
# 
# bone_data = transform(bone_data,
#                       ethnic_Asian = ifelse(ethnic == "Asian", 1, 0),
#                       ethnic_Black = ifelse(ethnic == "Black", 1, 0),
#                       ethnic_Hispanic = ifelse(ethnic == "Hispanic", 1, 0),
#                       ethnic_White = ifelse(ethnic == "White", 1, 0),
#                       male = ifelse(sex == "male", 1, 0),
#                       female = ifelse(sex == "female", 1, 0))
# 
# s = as.matrix(bone_data[,10:11])
# z = as.matrix(bone_data[,6:9])
# x = bone_data$age
# y = bone_data$rspnbmd
# 
# x_A = x[bone_data$ethnic == "Asian"]
# x_B = x[bone_data$ethnic == "Black"]
# x_H = x[bone_data$ethnic == "Hispanic"]
# x_W = x[bone_data$ethnic == "White"]
# x_m = x[bone_data$sex == "male"]
# x_f = x[bone_data$sex == "female"]
# 
# # z = [male, female, asian, black, hispanic, white]
# tz <- cbind(s,z)
# 
# fit = StepWise(x, tz, y, max_knots = 50, order = 4,
#                criterion = "BIC", max_iter = 1000, epsilon = 1e-5)
# 
# par(mfrow = c(4,2), mar = c(5.1, 4.5, 3.1, 2.1))
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # Asian male
# index = which((bone_data$sex == "male") & (bone_data$ethnic == "Asian"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Male & Asian", cex.main = 1.7)
# 
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # Asian female
# index = which((bone_data$sex == "female") & (bone_data$ethnic == "Asian"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Female & Asian", cex.main = 1.7)
# 
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # Black male
# index = which((bone_data$sex == "male") & (bone_data$ethnic == "Black"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Male & Black", cex.main = 1.7)
# 
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # Black female
# index = which((bone_data$sex == "female") & (bone_data$ethnic == "Black"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Female & Black", cex.main = 1.7)
# 
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # Hispanic male
# index = which((bone_data$sex == "male") & (bone_data$ethnic == "Hispanic"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Male & Hispanic", cex.main = 1.7)
# 
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # Hispanic female
# index = which((bone_data$sex == "female") & (bone_data$ethnic == "Hispanic"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Female & Hispanic", cex.main = 1.7)
# 
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # White male
# index = which((bone_data$sex == "male") & (bone_data$ethnic == "White"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Male & White", cex.main = 1.7)
# 
# plot(x, y, col = "gray", cex = 1.5, type = "n", bty = "n",
#      xlab = "age", ylab = "BMD", cex.lab = 1.7, cex.axis = 1.6)
# points(x, y, col = "gray")
# # White female
# index = which((bone_data$sex == "female") & (bone_data$ethnic == "White"))
# lines(sort(x[index]), fit$fitted_values[index][order(x[index])], col = "black",
#       lwd = 1.5)
# points(x[index], y[index], col = "black")
# title(main = "Female & White", cex.main = 1.7)
