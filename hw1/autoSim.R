# autoSim.R
nVals = seq(100, 500, by=100)
distTypes = c("gaussian", "t1", "t5")
for (n in nVals) {
  for (distr in distTypes) {
    oFile = paste("n", n, "_", distr, ".txt", sep="")
    arg = paste("seed=280 ", "n=", n, " dist=", distr, " rep=50", sep = "")
    sysCall = paste("nohup Rscript runSim.R ", arg, " > ", oFile, sep = "")
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
}