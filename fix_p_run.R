library(InformationAndInference)
.m <- c(1e1, 2e1, 4e1)
.p <- 2e1
.n <- seq(4e1, 4e3, length.out=6)
.sigma <- 1e1



## OLS
configurations.0 <- makeConfiguration(
  reps = 5e1, m = .m, p = .p, n = .n, lambda = NA, 
  model = my.ols, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarIdentity,
  data.maker=makeRegressionData) 

cl <- makeCluster(min(30, nrow(configurations.0)))
clusterEvalQ(cl, library(InformationAndInference))
MSEs.0 <- parApply(cl, configurations.0, 1, replicateMSE)

MSEs.framed.0 <- frameMSEs(MSEs.0, configurations.0)
plotMSEs(MSEs.framed.0, 'test', robust=TRUE, legend.position='right')



save(MSEs.0, configurations.0, file='RData/MSEs.0.RData')




## Ridge
configurations.1 <- makeConfiguration(
  reps = 5e1,
  m = .m, 
  p = .p, 
  n = .n, 
  lambda=2,
  model = my.ridge, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarRidge,
  data.maker=makeRegressionData) 

MSEs.1 <- parApply(cl, configurations.1, 1, replicateMSE)


MSEs.framed.1 <- frameMSEs(MSEs.1, configurations.1)
plotMSEs(MSEs.framed.1, 'test', robust=TRUE, legend.position='right')


save(MSEs.1, configurations.1, file='RData/MSEs.1.RData')



## Non Linear
configurations.3 <- makeConfiguration(
  reps = 5e1, 
  m = .m, 
  p = .p, 
  n = .n, 
  lambda = NA, 
  model = my.log.link, link = exp, sigma = 1, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker=makeRegressionData) 

MSEs.3 <- parApply(cl, configurations.3, 1, replicateMSE)


MSEs.framed.3 <- frameMSEs(MSEs.3, configurations.3)
MSEs.framed.3 %>% select(n, average, std.dev, median, mad) %>% head
plotMSEs(MSEs.framed.3, 'test', robust=TRUE, legend.position='right')

save(MSEs.3, configurations.3, file='RData/MSEs.3.RData')




## Logistic Regression
configurations.4<- makeConfiguration(
  reps = 1e2, 
  m = .m, 
  p = .p, 
  n = .n, 
  model = my.logistic, link = sigmoid, sigma = 1e1, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker = makeClassificationData) 

MSEs.4 <- parApply(cl, configurations.4, 1, replicateMSE)

MSEs.framed.4 <- frameMSEs(MSEs.4, configurations.4)
plotMSEs(MSEs.framed.4, 'test', robust=TRUE, legend.position='right')

save(MSEs.4, configurations.4, file='RData/MSEs.4.RData')


stopCluster(cl)


