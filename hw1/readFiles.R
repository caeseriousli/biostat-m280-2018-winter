# Read txt files and output average MSE table

# Read txt files in the current working directory
nVals = seq(100, 500, by=100)
distTypes = c("gaussian", "t1", "t5")

# Create a data frame to store all data
finalTable = matrix(data = NA, nrow = 0, ncol = 5)
colnames(finalTable) = c("n", "Method", "t1", "t5", "Gaussian")
finalTable = as.data.frame(finalTable)

for (n in nVals) {
  # create a temperary table for each sample size n to store values read from files
  tempTable = matrix(data = NA, nrow = 2, ncol = 5)
  colnames(tempTable) = c("n", "Method", "t1", "t5", "Gaussian")
  # set up a column index counter (iterator) to help input value to the temp table
  # it iterates from 3 to 5 and back to 3, to fill in values from 3 distributions
  counter = 3
  for (distr in distTypes) {
    # "Borrowing" code from autoSim.R to read in relevant files
    oFile = paste("n", n, "_", distr, ".txt", sep="")
    reading = read.table(oFile, header=FALSE, 
                          stringsAsFactors=FALSE)
    # 
    tempTable[, 1] = n
    tempTable[, 2] = reading[, 1]
    tempTable[, counter] = reading[, 2]
    # counter +1 so the next file it reads, values will be stored in next column
    counter = counter + 1
  }
  finalTable = rbind(finalTable, tempTable)
}
library(knitr)
kable(finalTable, row.names = FALSE)


