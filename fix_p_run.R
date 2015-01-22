library(InformationAndInference)
.m <- c(1e1, 2e1, 4e1, 1e2)
.p <- 5e1
.n <- seq(4e1, 1e3, length.out=6)
.sigma <- 1e1


## OLS
configurations.0 <- makeConfiguration(
  reps = 2e1, 
  m = .m, p = .p, n = .n, lambda = NA, 
  model = my.ols, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarIdentity,
  data.maker=makeRegressionData) 
## Ridge
configurations.1 <- makeConfiguration(
  reps = 1e2,
  m = .m, 
  p = .p, 
  n = .n, 
  lambda=2,
  model = my.ridge, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarRidge,
  data.maker=makeRegressionData) 
## Non Linear
configurations.3 <- makeConfiguration(
  reps = 5e2, 
  m = .m, 
  p = .p, 
  n = .n, 
  lambda = NA, 
  model = my.log.link, link = exp, sigma = 1, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker=makeRegressionData) 
## Logistic Regression
configurations.4<- makeConfiguration(
  reps = 2e2, 
  m = .m, 
  p = .p, 
  n = .n, 
  model = my.logistic, link = sigmoid, sigma = 1e1, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker = makeClassificationData) 

configurations <- rbind(
  configurations.0, configurations.1, 
  configurations.3, configurations.4)




cl <- makeCluster(35)
clusterEvalQ(cl, library(InformationAndInference))
MSEs <- parApply(cl, configurations, 1, replicateMSE)
stopCluster(cl)
save(MSEs, configurations, file='RData/MSEs_fix_p.1.RData')


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

