# To be run from fix_p_combined_simulations.Rmd

configurations <- rbind(
  configurations.0, configurations.1, 
  configurations.3, configurations.4)




# cl <- makeCluster(3, useXDR=TRUE, homogeneous=FALSE, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
cl <- makeCluster(35, type="FORK", rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
clusterEvalQ(cl, library(InformationAndInference))

MSEs <- parApply(cl, configurations, 1, replicateMSE)
attr(MSEs, "createdAt") <- Sys.time()
save(MSEs, configurations, file='RData/MSEs_fix_p.9.RData')

stopCluster(cl)




