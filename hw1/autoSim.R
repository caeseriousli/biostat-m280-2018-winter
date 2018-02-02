# autoSim.R
nVals = seq(100, 500, by=100)
distTypes = c("t1", "t5", "gaussian")
for (n in nVals) {
  # add another for loop for distribution types
  for (distr in distTypes) {
    # use both n and distribution types to output to files
    oFile = paste("n", n, "_", distr, ".txt", sep="")
    arg = paste("seed=280 ", "n=", n, " dist=", distr, " rep=50", sep = "")
    sysCall = paste("nohup Rscript runSim.R ", arg, " > ", oFile, sep = "")
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
}