library(InformationAndInference)
.m <- c(1e1, 2e1, 4e1, 1e2)
.kappa <- 0.2
.n <- seq(4e1, 1e3, length.out=6)
.sigma <- 1e1
file.ending <- 2


## OLS
configurations.00 <- makeConfiguration_fixKappa(
  reps = 1e2, 
  m = .m, 
  n = .n , 
  kappa = .kappa, 
  lambda = NA, 
  sigma = .sigma, 
  model = my.ols, link = identity, beta.maker = makeBetasDeterministic, 
  beta.star.maker = BetaStarIdentity, data.maker = makeRegressionData,
  truth.fun=truthOLS) 
## Ridge
configurations.10 <- makeConfiguration_fixKappa(
  reps = 1e2,
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  lambda=2,
  model = my.ridge, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarRidge,
  data.maker=makeRegressionData) 
## Non Linear
configurations.30 <- makeConfiguration_fixKappa(
  reps = 5e2, 
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  lambda = NA, 
  model = my.log.link, link = exp, sigma = 1, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker=makeRegressionData) 
## Logistic Regression
configurations.40<- makeConfiguration_fixKappa(
  reps = 2e2, 
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  model = my.logistic, link = sigmoid, sigma = .sigma, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker = makeClassificationData) 


configurations.fix.kappa <- rbind(
  configurations.00,
  configurations.10,
  configurations.30,
  configurations.40)


cl <- makeCluster(35)
clusterEvalQ(cl, library(InformationAndInference))
MSEs.fix.kappa <- parApply(cl, configurations.fix.kappa, 1, replicateMSE)
stopCluster(cl)

file.name <- paste('RData/MSEs_fix_kappa.',file.ending,'.RData', sep='')
save(MSEs.fix.kappa, configurations.fix.kappa, file='RData/MSEs_fix_kappa.1.RData')
############################################
