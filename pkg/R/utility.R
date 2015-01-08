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
  betas <- rnorm(p)
  betas <- betas / sqrt(betas %*% betas)
  return(betas)
}
## Testing:
#makeBetas(100)



makeTest <- function(reps=1e1, 
                     m=c(1e1,1e2), 
                     p=5e1, 
                     n=c(1e2,1e3), 
                     kappa=0.1, 
                     model=my.ols){
  .reps <<- reps
  .m <<- m
  .p <<- p
  .n <<- n
  .kappa <<- kappa
  .model <<- model
}
## Testing
# makeTest()
# .p  





# Return a frame with all simulation configurations
makeConfiguration <- function(reps, m, p, n, kappa, model, parameters, link=identity, sigma=1){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      p=p, 
                                      n=n, 
                                      kappa=kappa, 
                                      model=list(model), 
                                      params=c(makeBetas),
                                      link=c(link),
                                      sigma=sigma)
  configurations.frame <- configurations.frame %>% mutate(N=n*m)
  return(configurations.frame)
}
## Testing:
# makeTest()
# .configuration <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params)
# .configuration %>% dim
# .configuration %>% names
# .configuration$model 


# Which configurations not to run
excludeConfigurations <- function(configurations){
  result <- configurations %>% 
    filter(p/n < kappa)
  return(result)
}
## Testing:
# makeTest(p=1e1)
# .configuration <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params)
# excludeConfigurations(.configuration) 


# Take betas and make sample:
## betas are assumed to have unit variance.
makeRegressionData <- function(p, N, betas, link, sigma,...){
  
  # Deal with call from do.call where link is a list:
  if(is.list(link)) link <- link[[1]]
  
  # Generate data:
  X <-  matrix(rnorm(p*N), nrow=N, ncol=p)
  linear.effect <- X %*% betas
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
getErrors <- function(configuration, beta, beta.star){
  # Sketch: 
  ## Generate parameters
  ## Generate data
  ## Estimate parameters
  ## Compute ground true
  ## Compute errors
  p <- configuration[['p']]
  data <- do.call(makeRegressionData, c(list(betas=beta), configuration))
  coefs <- do.call(analyzeParallel, c(list(data), configuration))
  errors <- list(
    averaged= coefs$averaged-beta.star,
    centralized= coefs$centralized-beta.star)  
  #TODO: dump errors to file.
  return(errors)      
}
## Testing:
# .beta <- makeBetas(.p)
# .beta.star <- .beta
# makeTest(n=c(1e3))
# .configurations <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params)
# .errors <- getErrors( .configurations[1,], .beta, .beta.star)
# plot(averaged~centralized, data=.errors)
# plot(.errors$averaged);abline(0,0)
# plot(.errors$centralized);abline(0,0)


# Compute sum of squares 
SSQ <- function(x) x^2 %>% sum

# Compute the squared error of estimates
errorFun <- function(errors){
  lapply(errors, SSQ)
}
## Testing:
# .beta <- makeBetas(.p)
# .beta.star <- .beta
# makeTest(n=c(1e4))
# .configurations <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params)
# .errors <- getErrors( .configurations[1,], .beta, .beta.star)
# errorFun(.errors)



