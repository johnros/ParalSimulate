

configurations.fix.kappa <- rbind(
  configurations.00,
  configurations.10,
  configurations.30,
  configurations.40)


cl <- makeCluster(37, type='FORK')
clusterEvalQ(cl, library(InformationAndInference))
MSEs.fix.kappa <- parApply(cl, configurations.fix.kappa, 1, replicateMSE)
save(MSEs.fix.kappa, configurations.fix.kappa, file='RData/MSEs_fix_kappa.7.RData')

stopCluster(cl)
############################################
