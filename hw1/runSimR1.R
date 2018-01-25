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

# Check which distribution we are supposed to use and generate sample
set.seed(seed)
if (dist == "gaussian")
  x = rnorm(n)
if (dist == "t1")
  x = rt(n, df = 1)
if (dist == "t5")
  x = rt(n, df = 5)

# All mean squared errors stored in a variable
all_prime = replicate(rep, estMeanPrimes(x))
all_classic = replicate(rep, mean(x))

# Print average MSE's for both prime-indexed and classic simulations
noquote(c("Prime-index Average MSE", sum(all_prime)/rep))
noquote(c("Prime-index Average MSE", sum(all_classic)/rep))
