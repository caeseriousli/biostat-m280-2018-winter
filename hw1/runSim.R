## Creating these dummy objects for passing arguments from autoSim.R, because
## I could not pass a double quotation mark and a single quotation mark within
## the argument string, so they had to be passed as R objects

gaussian = "gaussian"
t1 = "t1"
t5 = "t5"

## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}

# Set which distribution we are supposed to use to generate samples
if (dist == "gaussian") {
  both_means = function() {
    x = rnorm(n)
    return(c(estMeanPrimes(x), mean(x)))
  }
} else if (dist == "t1") {
  both_means = function() {
    x = rt(n, df = 1)
    return(c(estMeanPrimes(x), mean(x)))
  }
} else if (dist == "t5") {
  both_means = function() {
    x = rt(n, df = 5)
    return(c(estMeanPrimes(x), mean(x)))
  }
}
# All mean squared errors stored in a variable
set.seed(seed)
all = replicate(rep, both_means())

# Print average MSE's for both prime-indexed and classic simulations
## Note that since the true mean is zero for all distributions, 
##   we simply square the vector
cat(c("PrimeAvg: ", round(sum((all[1, ])^2)/rep, 5)), "\n")
cat(c("SampAvg: ", round(sum((all[2, ])^2)/rep, 5)), "\n")
