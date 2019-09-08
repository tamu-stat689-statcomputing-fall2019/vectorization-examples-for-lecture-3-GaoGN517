## Given a vector x, want to calculate matrix with x, x^2, x^3 up to specified degree.
library(microbenchmark)
library(profvis)

## dg - integer, maximal power
powers1 <- function(x, dg) {
  pw <- matrix(x, nrow = length(x))
  prod <- x # current product
  for (i in 2:dg) {
    prod <- prod * x
    pw <- cbind(pw, prod)
  }
  return(pw)
}


x <- runif(1000000)

microbenchmark(
  powers1(x, 8), #283.1857
  times = 10 # I dont want the default 100 runs because it's too long. 
)

Rprof() ## gives a report of (approximately) hwo much time each function/operation with 
## the code takes. 
# start monitoring
invisible(powers1(x, 8))
Rprof(NULL) ## stop monitoring
summaryRprof() ## See the report.
## Supperise: cbind is taking a lot of time! This is because of dynamic allocation
## as we did not specify the size that pw will take before the loop. 

profvis(
  {
    dg <- 8
    pw <- matrix(x, nrow = length(x))
    prod <- x
    for (i in 2:dg){
      prod <- prod * x
      pw <- cbind(pw, prod)
    }}
  
)
# This needs to supply code line by line rather than the function call for best results. 