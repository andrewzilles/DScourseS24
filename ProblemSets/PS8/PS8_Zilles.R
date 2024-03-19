#3
install.packages("nloptr")
library(nloptr)

#4
# Set the seed
set.seed(100)

# Number of observations and variables
N <- 100000
K <- 10

# Generate X matrix with first column of 1s
X <- cbind(1, matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1))

# Generate error term (eps)
sigma_sq <- 0.25
eps <- rnorm(N, mean = 0, sd = sqrt(sigma_sq))

# Set true values of beta
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
print(beta)

# Generate Y
Y <- X %*% beta + eps


#5
# OLS closed-form solution
beta_ols_closed <- solve(t(X) %*% X) %*% t(X) %*% Y

# Compare with true beta
print(beta_ols_closed)
print(beta)

#These are all pretty close. Looks like the most it is off is by 0.0035ish.
print(beta_ols_closed)

#6
# Gradient function
gradient_ols <- function(beta, Y, X) {
  return(-t(X) %*% (Y - X %*% beta))
}

# Initial guess for beta
beta_init <- rep(0, K)

# Gradient descent
learning_rate <- 0.0000003
beta_ols_gd <- beta_init
iterations <- 10000
for (i in 1:iterations) {
  beta_ols_gd <- beta_ols_gd - learning_rate * gradient_ols(beta_ols_gd, Y, X)
}

print(beta_ols_gd)


#7
library(nloptr)

# Objective function for OLS
ols_objective <- function(beta, Y, X) {
  return(sum((Y - X %*% beta)^2))
}

# Gradient function for OLS objective
ols_gradient <- function(beta, Y, X) {
  return(-2 * t(X) %*% (Y - X %*% beta))
}

# Using L-BFGS algorithm
beta_ols_lbfgs <- nloptr(x0 = rep(0, K), 
                         eval_f = ols_objective, 
                         eval_grad_f = ols_gradient,
                         lb = rep(-Inf, K), 
                         ub = rep(Inf, K), 
                         opts = list(algorithm = "NLOPT_LD_LBFGS"), 
                         Y = Y, 
                         X = X)$solution

# Using Nelder-Mead algorithm
beta_ols_neldermead <- nloptr(x0 = rep(0, K), eval_f = ols_objective, lb = rep(-Inf, K), ub = rep(Inf, K), opts = list(algorithm = "NLOPT_LN_NELDERMEAD"), Y = Y, X = X)$solution

print(beta_ols_lbfgs)
print(beta_ols_neldermead)



#8
# Objective function for MLE
mle_objective <- function(theta, Y, X) {
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  return(sum((Y - X %*% beta)^2 / (2 * sig^2)) + N * log(sig))
}

gradient2 <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

# Initial guess for theta (beta and sigma)
theta_init <- c(rep(0, K), 1)

# Using L-BFGS algorithm
beta_mle_lbfgs <- nloptr(x0 = theta_init, eval_f = mle_objective, eval_grad_f = gradient2, lb = rep(-Inf, length(theta_init)), ub = rep(Inf, length(theta_init)), opts = list(algorithm = "NLOPT_LD_LBFGS"), Y = Y, X = X)$solution[1:(length(theta_init) - 1)]

print(beta_mle_lbfgs)




#9
# Using lm() function
model <- lm(Y ~ X - 1)

# Exporting to .tex file using modelsummary
library(modelsummary)
modelsummary(model, output = "ols_output.tex")

getwd()

