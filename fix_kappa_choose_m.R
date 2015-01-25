library(InformationAndInference)
.m <- seq(1e1, 1e2, length.out=10)
.p <- 5e1
.N <- 1e4
.n <- ceiling(.N/.m)
.sigma <- 1e1


## OLS
configurations.000 <- makeConfiguration(
  reps = 1e2, 
  m = .m, p = .p, n = .n, lambda = NA, 
  model = my.ols, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarIdentity,
  data.maker=makeRegressionData,
  name='ols') 

nrow(configurations.000)

cl <- makeCluster(35)
clusterEvalQ(cl, library(InformationAndInference))
MSEs.000 <- parApply(cl, configurations.000, 1, replicateMSE)
stopCluster(cl)
save(MSEs.000, configurations.000, file='RData/MSEs_choose_m.1.RData')


##TODO: adapt analysis to unified MSEs object.
# MSEs.framed.0 <- frameMSEs(MSEs.0, configurations.0)
# plotMSEs(MSEs.framed.0, 'test', robust=TRUE, legend.position='right')
# 
# 
# MSEs.framed.1 <- frameMSEs(MSEs.1, configurations.1)
# plotMSEs(MSEs.framed.1, 'test', robust=TRUE, legend.position='right')
# 
# MSEs.framed.3 <- frameMSEs(MSEs.3, configurations.3)
# MSEs.framed.3 %>% select(n, average, std.dev, median, mad) %>% head
# plotMSEs(MSEs.framed.3, 'test', robust=TRUE, legend.position='right')
# 
# MSEs.framed.4 <- frameMSEs(MSEs.4, configurations.4)
# plotMSEs(MSEs.framed.4, 'test', robust=TRUE, legend.position='right')

