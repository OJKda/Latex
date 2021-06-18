source("splineBox1.9.R")

Plus = function(x)
{
   x_plus = x
   x_plus[x_plus < 0] = 0
   return(x_plus)
}

Psplines = function(x, num_knots = NULL, knots = NULL, degree = 3)
{
   N = length(x)
   if(is.null(num_knots))
      num_knots = length(knots)
   else if(is.null(knots))
      knots = seq(min(x), max(x), length = num_knots)
   else
      stop("Both num_knots and knots are NULL!!")
   dimension = num_knots - 2 + degree + 1
   X = matrix(0, N, dimension)
   X[, 1] = 1
   for(i in 1 : degree)
      X[, i + 1] = x^i
   for(j in 1 : (dimension - degree - 1))
      X[, (j + degree + 1)] = Plus(x - knots[j + 1])^degree
   return(X)
}

PS_Reg_B = function(x, z, y, knots = knots, degree = 3, max_iter = 1000)
{
   n = length(x)
   X = bsplines(x, knots, order = degree + 1, derivative = 0)
   #X = Psplines(x, num_knots = num_knots, degree = degree)
   dimension = ncol(X)
   beta = theta = rep(0, dimension)
   BB = colSums(X^2)
   residuals_old = y
   residuals = y
   RSS_old = sum(residuals^2) / (2)
   for(iter in 1 : max_iter)
   {
      # update beta
      for(j in 1 : dimension)
      {
         partial_residuals = residuals + beta[j] * X[, j]
         beta[j] = sum(partial_residuals * X[, j]) / sum(X[, j]^2)
         residuals = partial_residuals - beta[j] * X[, j]
      }
      # update theta
      for(j in 1 : dimension)
      {
         partial_residuals = residuals + z * theta[j] * X[, j]
         theta[j] = sum(partial_residuals * z * X[, j]) / (sum(z^2 * X[, j]^2))
         residuals = partial_residuals - z * theta[j] * X[, j]
      }
      RSS = sum(residuals^2) / (2)
      if(abs(RSS_old - RSS) <= 1e-10)
         break
      else
         RSS_old = RSS
   }
   fitted_values = y - residuals
   results = list()
   results$beta = beta
   results$theta = theta
   results$residuals = residuals
   results$fitted_values = fitted_values
   results$iteration = iter
   results$X = X
   return(results)
}








