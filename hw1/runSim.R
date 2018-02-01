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
  sim_prime = function() {
    x = rnorm(n)
    return(estMeanPrimes(x))
  }
  sim_mean = function() {
    x = rnorm(n)
    return(mean(x))
  } 
} else if (dist == "t1") {
  sim_prime = function() {
    x = rt(n, df = 1)
    return(estMeanPrimes(x))
  }
  sim_mean = function() {
    x = rt(n, df = 1)
    return(mean(x))
  } 
} else if (dist == "t5") {
  sim_prime = function() {
    x = rt(n, df = 5)
    return(estMeanPrimes(x))
  }
  sim_mean = function() {
    x = rt(n, df = 5)
    return(mean(x))
  }
}
# All mean squared errors stored in a variable
set.seed(seed)
all_prime = replicate(rep, sim_prime())
all_classic = replicate(rep, sim_mean())

# Print average MSE's for both prime-indexed and classic simulations
## Note that since the true mean is zero for all distributions, 
##   we simply square the vector
cat(c("PrimeAvg: ", round(sum((all_prime)^2)/rep, 5)), "\n")
cat(c("SampAvg: ", round(sum((all_classic)^2)/rep, 5)), "\n")
