library(InformationAndInference)
.m <- c(1e1, 2e1, 4e1, 1e2)
.p <- 5e1
.n <- seq(4e1, 1e3, length.out=6)
.sigma <- 1e1
.lambda <- 1e-1

## OLS
configurations.0 <- makeConfiguration(
  reps = 1e2, 
  m = .m, p = .p, n = .n, lambda = NA, 
  model = my.ols, link = identity, sigma = .sigma, beta.norm = 1e1,
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarIdentity,
  data.maker=makeRegressionData,
  name='ols') 
## Ridge
configurations.1 <- makeConfiguration(
  reps = 2e1,
  m = .m, 
  p = .p, 
  n = .n, 
  lambda=.lambda,
  model = my.ridge, link = identity, sigma = .sigma, beta.norm = 1e1,
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarRidge,
  data.maker=makeRegressionData,
  name='ridge') 
## Non Linear
configurations.3 <- makeConfiguration(
  reps = 5e2, 
  m = .m, 
  p = .p, 
  n = .n, 
  lambda = NA, 
  model = my.log.link, link = exp, sigma = 1, beta.norm = 1e1,
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker=makeRegressionData,
  name='non-linear') 
## Logistic Regression
configurations.4<- makeConfiguration(
  reps = 2e2, 
  m = .m, 
  p = .p, 
  n = .n, 
  lambda = NA, 
  model = my.logistic, link = sigmoid, sigma = 1e1, beta.norm = 1e1,
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker = makeClassificationData,
  name='logistic') 

configurations <- rbind(
  configurations.0, configurations.1, 
  configurations.3, configurations.4)




# cl <- makeCluster(3, useXDR=TRUE, homogeneous=FALSE, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
cl <- makeCluster(15, type="FORK", rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
clusterEvalQ(cl, library(InformationAndInference))

MSEs <- parApply(cl, configurations, 1, replicateMSE)
attr(MSEs, "createdAt") <- Sys.time()
save(MSEs, configurations, file='RData/MSEs_fix_p.5.RData')

stopCluster(cl)




