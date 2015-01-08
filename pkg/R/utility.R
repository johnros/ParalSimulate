# Make fitting and coef extracting function
my.ols <-  list(fitter=function(y, x,...) lm(y~x-1,...),
                coefs= function(fit) coef(fit),
                getTrueBeta=function(x) x)

my.huber <- list(fitter=my.huber <- function(y, x,...) rlm(y~x-1,...), 
                 coefs= function(fit) coef(fit),
                 getTrueBeta=function(x) x)

my.absolute <- list(fitter=function(y, x,...) rq(y~x-1, method="fn",...),
                    coefs=function(fit) coef(fit),
                    getTrueBeta=function(x) x)

my.logistic <- list(fitter=function(y, x,...) glm(y~x-1, family = binomial,...),
                    coefs= function(fit) coef(fit),
                    getTrueBeta=function(x) x)




# Generate true parameters
makeBetas <- function(p){
  beta <- rnorm(p)
  beta <- beta / sqrt(beta %*% beta)
  return(beta)
}
## Testing:
#makeBetas(100)



makeTest <- function(reps=1e1, 
                     m=c(1e1,1e2), 
                     p=5e1, 
                     n=c(1e2,1e3), 
                     kappa=0.1, 
                     model=my.ols,
                     beta=makeBetas(p),
                     beta.star=beta){
  .reps <<- reps
  .m <<- m
  .p <<- p
  .n <<- n
  .kappa <<- kappa
  .model <<- model
  .beta<<- beta
  .beta.star<<- beta.star
}
## Testing
# makeTest()
# .p  





# Return a frame with all simulation configurations
makeConfiguration <- function(reps, 
                              m, 
                              p, 
                              n, 
                              kappa, 
                              model, 
                              parameters, 
                              link=identity, 
                              sigma=1, 
                              beta,
                              beta.star){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      p=p, 
                                      n=n, 
                                      kappa=kappa, 
                                      model=list(model), 
                                      params=c(makeBetas),
                                      link=c(link),
                                      sigma=sigma,
                                      beta=list(beta),
                                      beta.star=list(beta.star))
  configurations.frame <- configurations.frame %>% mutate(N=n*m)
  configurations.frame <- configurations.frame %>% 
    filter(p/n < kappa)
 
  return(configurations.frame)
}
## Testing:
# makeTest()
# .configurations <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params, identity, 1, .beta,.beta.star)
# .configurations %>% dim
# .configurations %>% names





# Take betas and make sample:
## betas are assumed to have unit variance.
makeRegressionData <- function(p, N, beta, link, sigma,...){
  
  # Deal with call from do.call where link is a list:
  if(is.list(link)) link <- link[[1]]
  if(is.list(beta)) beta <- beta[[1]]
  
  # Generate data:
  X <-  matrix(rnorm(p*N), nrow=N, ncol=p)
  linear.effect <- X %*% beta
  y <- link(linear.effect) + rnorm(N, 0, sd=sigma)
  result <- list(y=y, X=X)
  return(result)
}
## Testing:
# .p <- 1e2
# .N <- 1e4
# .betas <- makeBetas(p = .p)
# makeRegressionData(.p, .N, .betas, identity, 1)


analyzeParallel <- function(data, m, model, N, ...){
  y <- data$y
  X <- data$X
  FITTER <- model[[1]]$fitter
  COEFS <- model[[1]]$coefs
  
  ## Centralized Solution:
  the.fit <- FITTER(y=y,x=X)
  center.coefs <- COEFS(the.fit)
  
  ## Parallelized solution:
  machine.ind <- sample(1:m, size = N, replace = TRUE)
  
  # Check if there are enough observations per machine
  .min <- machine.ind %>% table %>% min
  if(.min < p) stop('Not enough observations per machine')
  
  
  machine.wise <-  matrix(NA, ncol = m, nrow = ncol(X))
  for(i in seq_len(m)){
    .the.fit <- FITTER(y=y[machine.ind==i], x=X[machine.ind==i,])
    .coefs<- COEFS(.the.fit)
    machine.wise[,i] <- .coefs 
  }
  averaged.coefs <- rowMeans(machine.wise, na.rm=TRUE)
  
  result <- list(averaged=averaged.coefs, 
                 centralized=center.coefs)
  return(result)
}
## Testing:




# Get single configuration, make data and return errors for parallelized and distributed:
getErrors <- function(configuration){
  # Sketch: 
  ## Generate parameters
  ## Generate data
  ## Estimate parameters
  ## Compute ground true
  ## Compute errors
  p <- configuration[['p']]
  data <- do.call(makeRegressionData, configuration)
  coefs <- do.call(analyzeParallel, c(list(data),configuration))
  errors <- list(
    averaged= coefs$averaged - configuration$beta.star[[1]],
    centralized= coefs$centralized - configuration$beta.star[[1]])  
  return(errors)      
}
## Testing:
# .beta <- makeBetas(.p)
# .beta.star <- .beta
# makeTest(n=c(1e4))
# .configurations <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params, beta=.beta, beta.star=.beta.star)
# .errors <- getErrors( .configurations[1,])
# plot(averaged~centralized, data=.errors)


# Compute sum of squares 
SSQ <- function(x) x^2 %>% sum

# Compute the squared error of estimates
errorFun <- function(errors){
  lapply(errors, SSQ)
}
## Testing:
# errorFun(.errors)



# Repeat estimation for a given configuration
replicateMSE <- function(configuration){
  MSEs <- replicate(configuration$replications,{
    errors <- getErrors(configuration)
    errorFun(errors)
  })
  return(MSEs)
}
## Testing:
# configuration <- .configurations[1,]
# replicateMSE(configuration)


