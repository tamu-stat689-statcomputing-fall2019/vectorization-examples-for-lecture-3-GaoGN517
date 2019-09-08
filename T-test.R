# Exp4. Multiple t-test
# Lecture examples are lacking enough comments.
p <- 1000
n <- 50

set.seed(03875)
X <- matrix(rnorm(n*p, mean = 10, sd = 3), n, p)
group <- rep(1:2, each = n/2)

# Function that returns t-test statistic for each gene
computeT_for <- function(X, group){
  Tstats <- rep(0, p)
  for (j in 1:p){
    Tstats[j] <- t.test(X[, j] ~ group)$stat
  }
  return(Tstats)
}

library(microbenchmark)
microbenchmark(
  computeT_for(X, group),
  times = 10
)
## too slow
## change the interface.
computeT_for2 <- function(X, group){
  Tstats <- rep(0, p)
  for (j in 1:p){
    Tstats[j] <- t.test(X[group == 1, j], X[group == 2, j])$stat
  }
  return(Tstats)
}

## 4 fold difference. Use the non-formula interface instead of the formula interface. 
microbenchmark(
  computeT_for(X, group),
  computeT_for2(X, group),
  
  times = 10
)


## Function that returns t-test statistic for each gene. 
## We don't need all other statistics in this situation.
computeT_for3 <- function(X, group){
  n1 = sum(group == 1)
  n2 = sum(group == 2)
  
  Tstats <- rep(0, p)
  for (j in 1:p){
    m1 = mean(X[group == 1, j])
    m2 = mean(X[group == 2, j])
    var1 = var(X[group == 1, j])
    var2 = var(X[group == 2, j])
    Tstats[j] = (m1 - m2) /sqrt(var1/n1 + var2/n2)
  }
  return(Tstats)
}

microbenchmark(
  computeT_for(X, group),
  computeT_for2(X, group),
  computeT_for3(X, group),
  times = 10
)

## Get rid of the for loops
computeT_vec0 <- function(X, group) {
  n1 = sum(group == 1)
  n2 = sum(group == 2)
  
  #Means
  m1 = colMeans(X[group == 1, ])
  m2 = colMeans(X[group == 2, ])
  
  #Variances
  var1 = sum(diag(var(X[group == 1, ])))
  var2 = sum(diag(var(X[group == 2, ]))) #right way but not good enough.
 
  
  #Get T statistics
  Tstats = (m1 - m2) /sqrt(var1/n1 + var2/n2)
  return(Tstats)
}

computeT_vec <- function(X, group) {
  n1 = sum(group == 1)
  n2 = sum(group == 2)
  
  #Means
  m1 = colMeans(X[group == 1, ])
  m2 = colMeans(X[group == 2, ])
  
  #Variances
  #var1 = sum(diag(var(X[group == 1, ])))
  #var2 = sum(diag(var(X[group == 2, ]))) #right way but not good enough.
  p = ncol(X)
  var1 = colSums((X[group == 1, ] - matrix(m1, n1, p, byrow = T))^2)/(n1 - 1)
  var2 = colSums((X[group == 2, ] - matrix(m2, n2, p, byrow = T))^2)/(n2 - 1)
  
  #Get T statistics
  Tstats = (m1 - m2) /sqrt(var1/n1 + var2/n2)
  return(Tstats)
}

#identical(computeT_for3(X, group), computeT_vec(X, group))

microbenchmark(
  computeT_for3(X, group),
  computeT_vec0(X, group),
  computeT_vec(X, group),
  times = 10
)
