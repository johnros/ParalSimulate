# Vary N configuration:
# .m <- seq(1e1, 1e2, length.out=10)
# .p <- 5e1
# .N <- c(1e4, 1e5, 1e6)
# .n <- expand.grid(.m,.N) %>% apply(1, function(x) ceiling(x[2]/x[1]))
# .sigma <- 1e1

# TODO: simulate with large p for high-dim scenario.
# Possible a single plot, Fixing N, m on the x, and different p. 
# For large p, MSE should be linear. 
# For small p, MSE can be non linear.
.sigma <- 1e1
.N <- 5e4
(.m <- seq.int(1e1, 1e2, by=5) )
(.n <- (.N/.m))
.kappa <- 0.9
(.p <- seq(5e1, min(.n)*.kappa, length.out=4) %>% round(-1))



## OLS
configurations.000 <- makeConfiguration(
  reps = 1e2, 
  m = .m, p = .p, n = .n, lambda = NA, 
  model = my.ols, 
  link = identity, 
  sigma = .sigma, 
  beta.maker = makeBetasDeterministic, 
  beta.star.maker = BetaStarIdentity,
  data.maker=makeRegressionData,
  name='ols') 
configurations.000 %>% select(N) %>% round(-3) %>% table
configurations.000 %<>% filter(round(N,-2) ==.N)
nrow(configurations.000)
configurations.000 %>% select(m,p,N) %>% table 

cl <- makeCluster(35)
clusterEvalQ(cl, library(InformationAndInference))

MSEs.000 <- parApply(cl, configurations.000, 1, replicateMSE)
attr(MSEs.000, "createdAt") <- Sys.time()

save(MSEs.000, configurations.000, file='RData/MSEs_choose_m.2.RData')



## Ridge
.lambda <- 2
configurations.001 <- makeConfiguration(
  reps = 1e2, 
  m = .m, p = .p, n = .n, lambda = .lambda, 
  model = my.ridge, 
  link = identity, 
  sigma = .sigma, 
  beta.maker = makeBetasDeterministic, 
  beta.star.maker = BetaStarRidge,
  data.maker=makeRegressionData,
  name='ridge') 
configurations.001 %<>% filter(round(N,-2) ==.N)

MSEs.001 <- parApply(cl, configurations.001, 1, replicateMSE)
attr(MSEs.001, "createdAt") <- Sys.time()
save(MSEs.001, configurations.001, file='RData/MSEs_choose_m_ridge.2.RData')

stopCluster(cl)





