# Exp4. Multiple t-test
p <- 1000
n <- 50

set.seed(03875)
X <- matrix(rnorm(n*p, mean = 10, sd = 3), n, p)
group <- rep(1:2, each = n/2)

# Function that returns t-test statistic for each gene
computeT_for <- function(X, group){
  Tstats <- rep(0, p)
  for (j in 1:p){
    Tstats[j] <- t.test(X[, j] - group)$stat
  }
  return(Tstats)
}

library(microbenchmark)
microbenchmark(
  computeT_for(X, group),
  times = 10
)