library(InformationAndInference)
.m <- c(1e1, 2e1, 4e1, 1e2)
.kappa <- 0.2
.n <- seq(4e1, 4e3, length.out=6)
.sigma <- 1e1
.beta.norm <- 1e0

## OLS
configurations.00 <- makeConfiguration(
  reps = 1e2, 
  m = .m, 
  n = .n , 
  kappa = .kappa, 
  lambda = NA, 
  sigma = .sigma, 
  model = my.ols, link = identity, beta.maker = makeBetasDeterministic, 
  beta.norm = .beta.norm,
  beta.star.maker = BetaStarIdentity, data.maker = makeRegressionData,
  name='ols') 
## Ridge
configurations.10 <- makeConfiguration(
  reps = 1e2,
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  lambda=2,
  model = my.ridge, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, 
  beta.norm=.beta.norm, beta.star.maker = BetaStarRidge,
  data.maker=makeRegressionData, name='ridge') 
## Non Linear
configurations.30 <- makeConfiguration(
  reps = 5e2, 
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  lambda = NA, 
  model = my.log.link, link = exp, sigma = 1, 
  beta.maker = makeBetasDeterministic, beta.norm=.beta.norm, 
  beta.star.maker = BetaStarIdentity, 
  data.maker=makeRegressionData, name='nls') 
## Logistic Regression
configurations.40<- makeConfiguration(
  reps = 2e2, 
  m = .m, 
  kappa = .kappa, 
  n = .n,
  lambda=NA,
  model = my.logistic, link = sigmoid, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.norm=.beta.norm,
  beta.star.maker = BetaStarIdentity, 
  data.maker = makeClassificationData, name='logistic') 


configurations.fix.kappa <- rbind(
  configurations.00,
  configurations.10,
  configurations.30,
  configurations.40)


cl <- makeCluster(15, type='FORK')
clusterEvalQ(cl, library(InformationAndInference))
MSEs.fix.kappa <- parApply(cl, configurations.fix.kappa, 1, replicateMSE)
save(MSEs.fix.kappa, configurations.fix.kappa, file='RData/MSEs_fix_kappa.5.RData')

stopCluster(cl)
############################################
