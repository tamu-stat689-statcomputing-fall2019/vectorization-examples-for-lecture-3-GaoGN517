# Classification rule in discriminant analysis
require(mnormt) # for multivariate normal data generation

# Functions for classification
##############################################
# INPUT
# beta - supplied discriminant vector
# xtrain, ytrain - training data
# xtest, ytest - testing data
#
# OUTPUT
# ypred - predicted class membership
# error - percent of misclassified observations

classify_for <- function(beta, xtrain, ytrain, xtest, ytest){
  # [ToDo] Code discriminant analysis classifier using for loop
  
  # Calculate sample means based on training data
  x_train_bar1 <- colMeans(xtrain[ytrain == 1, ]) ## sample mean for class1
  x_train_bar2 <- colMeans(xtrain[ytrain == 2, ]) ## sample mean for class2
  
  # Calculate class assignments for xtest in a for loop
  ntest <- nrow(xtest)
  ypred <- rep(1, ntest)
  for(i in 1:ntest) {
    h1 = as.numeric(crossprod(beta, xtest[i, ] - xbar1)^2)
    h2 = as.numeric(crossprod(beta, xtest[i, ] - xbar2)^2)
  }
  if (h1 > h2) {
    ypred[i] <- 2
  }
  
  # Calculate % error using ytest
  error <- (sum(ytest != ypred)/ntest) * 100
  
  # Return predictions and error
  return(list(ypred = ypred, error = error))
}

classify_vec <- function(beta, xtrain, ytrain, xtest, ytest){
  # [ToDo] Try to create vectorized version of classify_for
  
  # Calculate sample means based on training data
  x_train_bar1 <- colMeans(xtrain[ytrain == 1, ]) ## sample mean for class1
  x_train_bar2 <- colMeans(xtrain[ytrain == 2, ]) ## sample mean for class2
  # Calculate class assignments for xtest using matrix and vector algebra
  ntest <- nrow(xtest)
  xtestb <- xtest %*% beta
  m1b <- as.numeric(crossprod(x_train_bar1, beta)) 
  ## without as.numeric, the next step will report non-conformable.
  m2b <- as.numeric(crossprod(x_train_bar2, beta))
  h1 <- xtestb^2 - 2 * xtestb * m1b + m1b^2
  h2 <- xtestb^2 - 2 * xtestb * m2b + m2b^2
  ypred <- ifelse(h1 <= h2, 1, 2)
  # Calculate % error using ytest
  error <- (sum(ytest != ypred)/ntest) * 100
  # Return predictions and error
  return(list(ypred = ypred, error = error))
}

# Example 
##############################################
## dynamic memory allocation took a much long time. So define the space rather than cbind or append.
library(LearnBayes) # required to use rmnorm
# Create model parameters
p <- 10 # dimension
mu1 <- rep(0, p) # mean vector for class 1
mu2 <- rep(1, p) # mean vector for class 2
# equicorrelation covariance matrix with correlation rho
rho <- 0.4
Sigma <- matrix(rho, p, p) + diag(1-rho, p)

# Create training data
n1 <- 100 # number of samples in class 1
n2 <- 100 # number of samples in class 2
ytrain <- c(rep(1, n1), rep(2, n2)) # class assignment
xtrain <- matrix(0, n1 + n2, p)
xtrain[ytrain == 1, ] <- rmnorm(n1, mean = mu1, varcov = Sigma)
xtrain[ytrain == 2, ] <- rmnorm(n2, mean = mu2, varcov = Sigma)

# Create testing data of the same size for simplicity
ytest<- c(rep(1, n1), rep(2, n2)) # class assignment
xtest <- matrix(0, n1 + n2, p)

xtest[ytest == 1, ] <- rmnorm(n1, mean = mu1, varcov = Sigma)
xtest[ytest == 2, ] <- rmnorm(n2, mean = mu2, varcov = Sigma)

# Calculate sample means and within class sample covariance on training data
xbar1 <- colMeans(xtrain[ytrain == 1, ])
xbar2 <- colMeans(xtrain[ytrain == 2, ])
W <- ((n1 - 1) * cov(xtrain[ytrain == 1, ]) + (n2 - 1) * cov(xtrain[ytrain == 2, ]))/(n1 + n2 - 2)

# Calculate the discriminant vector
beta <- solve(W, xbar1 - xbar2)

# Calculate test assignments based on each function

out1 = classify_for(beta, xtrain, ytrain, xtest, ytest)

out2 = classify_vec(beta, xtrain, ytrain, xtest, ytest)

# [ToDo] Verify the assignments agree with each other
sum((out1$pred - out2$pred))^2

# [ToDo] Use microbenchmark package to compare the timing

library(microbenchmark)
microbenchmark(
  classify_for(beta, xtrain, ytrain, xtest, ytest), #668.0300
  classify_vec(beta, xtrain, ytrain, xtest, ytest) #65.2765
)
