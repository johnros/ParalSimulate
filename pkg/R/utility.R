# Make fitting and coef extracting function
my.ols <-  list(fitter=function(y, x,...) lm(y~x-1,...),
                coefs= function(fit) coef(fit))

my.ridge <-  list(fitter=function(y, x,...) lm.ridge(y~x-1,...),
                coefs= function(fit) coef(fit))

my.log.link <- list(fitter=function(y, x,...) {
  .start <- coef(lm(y~x-1))
  .control <- glm.control(epsilon=1e-6, maxit = 1e2 )
  glm(formula = y~x-1, family=gaussian(link='log'),  start=.start,
      control = .control)
  },
  coefs= function(fit) coef(fit))

my.huber <- list(fitter=my.huber <- function(y, x,...) rlm(y~x-1,...), 
                 coefs= function(fit) coef(fit))

my.absolute <- list(fitter=function(y, x,...) rq(y~x-1, method="fn",...),
                    coefs=function(fit) coef(fit))

my.logistic <- list(fitter=function(y, x,...) glm(y~x-1, family = binomial,...),
                    coefs= function(fit) coef(fit))


# Convert a list of lists to a matrix.
extractor <- function(x) apply(x, 1, unlist)



# Generate true parameters
makeBetasRandom <- function(p){
  beta <- rnorm(p)
  beta <- beta / sqrt(beta %*% beta)
  return(beta)
}
## Testing:
# makeBetasRandom(100) 


makeBetasDeterministic <- function(p){
  beta <- 1:p
  beta <- beta / sqrt(beta %*% beta)
  return(beta)
}
## Testing:
# makeBetasDeterministic(100) 



makeTest <- function(reps=1e1, 
                     m=c(1e1,1e2), 
                     p=5e1, 
                     n=c(1e2,1e3), 
                     kappa=0.1, 
                     lambda=2,
                     model=my.ols){
  .reps <<- reps
  .m <<- m
  .p <<- p
  .n <<- n
  .lambda<<- lambda
  .kappa <<- kappa
  .model <<- model
}
## Testing
# makeTest()
# .lambda  





# Return a frame with all simulation configurations
makeConfiguration <- function(reps, 
                              m, 
                              p, 
                              n, 
                              kappa, 
                              lambda,
                              model, 
                              link=identity, 
                              sigma=1, 
                              beta.maker,
                              beta.star.maker){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      p=p, 
                                      n=n, 
                                      kappa=kappa, 
                                      model=list(model),
                                      link=c(link),
                                      sigma=sigma)
  
  configurations.frame <- configurations.frame %>% 
    filter(p<n & p/n < kappa)
  
  configurations.frame %<>% mutate(N=n*m)    
  configurations.frame$beta <- lapply(configurations.frame$p, beta.maker)
  configurations.frame$beta.star <- lapply(configurations.frame$beta, beta.star.maker, lambda=lambda)
  
  return(configurations.frame)
}
## Testing:
#   OLS:
# makeTest()
# .configurations <- makeConfiguration(reps = .reps, m = .m, p = .p, n = .n, kappa = .kappa, lambda = 2, model = .model, link = identity, sigma = 1, beta.maker = makeBetasRandom, beta.star.maker = identity)
# .configurations %>% dim
# .configurations %>% names
# str(.configurations)
# .configurations$beta
#   Ridge:
# makeTest()
# .configurations <- makeConfiguration(reps = .reps, m = .m, p = .p, n = .n, kappa = .kappa, lambda = 2, model = .model, link = identity, sigma = 1, beta.maker = makeBetasRandom, beta.star.maker = identity)
# .configurations %>% dim
# .configurations %>% names
# str(.configurations)
# .configurations$beta




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
# do.call(makeRegressionData, configurations[1,])


analyzeParallel <- function(data, m, model, N, p,...){
  y <- data$y
  X <- data$X
  
  FITTER <- model$fitter
  COEFS <- model$coefs
  
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
#   p <- configuration[['p']]
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
  MSEs <- lapply(errors, SSQ)
  ratio <- with(MSEs, averaged/centralized)
  result <- c(list(ratio=ratio), MSEs )
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


## Get MSE results and configuration and return data.frame for plotting.
frameMSEs <- function(MSEs, configurations){
  
  getRatio <- function(x) x[,'ratio']
  
  MSEs.list <- lapply(MSEs, extractor)
  MSEs.frame <- MSEs.list %>% 
    lapply(getRatio) %>% {
      average <- sapply(.,mean, na.rm=TRUE)
      std.dev <- sapply(.,sd, na.rm=TRUE)
      cbind(average=average, std.dev=std.dev)
    } %>% 
    as.data.frame
  
  MSEs.framed <- data.frame(configurations, MSEs.frame) 
  MSEs.framed %<>% mutate(arm=2*std.dev/sqrt(n))
  
  return(MSEs.framed)
}
## Testing:
# .configurations <- makeConfiguration(
#   reps = 1e1, m = 1e1, p = 5e1, 
#   n = seq(2e2,5e2,length.out=3) , 
#   kappa = 5e-1, model = my.ols, link = identity, sigma = 1e1 ) 
# .MSEs <- apply(.configurations, 1, replicateMSE)
# frameMSEs(.MSEs, .configurations)



# Plot results of fixed p regime:
plotMSEs <- function(MSEs.framed, 
                     the.title, 
                     y.lab= '', 
                     y.lim=c(1,2)){
  
  plot.1 <- ggplot(data = MSEs.framed, aes(x=n, y=average, colour=m, group=m))+
    geom_point()+
    geom_segment(aes(xend=n, y=average+std.dev, yend=average-std.dev))
  
  plot.1 <- plot.1 +
    labs(title = the.title)+
    ylab(y.lab)+
    ylim(y.lim) +
    xlab(expression(n))+
    #scale_x_continuous(trans=log_trans(base = 10), breaks=c(5e2, 1e3, 5e3))+
    theme_bw()+
    theme(text = element_text(size=20), legend.position = "none") 
  return(plot.1)  
}
## Testing
# .configurations <- makeConfiguration(
#   reps = 1e1, m = 1e1, p = 5e1, 
#   n = seq(1.1e2,3e2,length.out=3) , 
#   kappa = 5e-1, model = my.ols, link = identity, sigma = 1e1 ) 
# .MSEs <- apply(.configurations, 1, replicateMSE)
# .MSEs.framed <- frameMSEs(.MSEs, .configurations)
# plotMSEs(.MSEs.framed, 'test')



## Compute risk minimizer for Ridge problem
identityBeta <- function(beta, ...){
  return(beta)
}
## Testing:
# identityBeta(rep(1,10), 2)
# identityBeta(makeBetasDeterministic(10), 2)
# identityBeta(makeBetasDeterministic(2), 2, matrix(c(10,3,3,2),2,2))



## Compute risk minimizer for Ridge problem
ridgeBeta <- function(beta, lambda, Sigma){
  if(missing(Sigma)){
    beta.star <- beta/(1+lambda)
  }
  else{
    p <- length(beta)
    beta.star <- solve(Sigma+lambda*diag(p)) %*% Sigma %*% beta
  }
  return(beta.star)
}
## Testing:
# ridgeBeta(rep(1,10), 2)
# ridgeBeta(makeBetasDeterministic(10), 2)
# ridgeBeta(makeBetasDeterministic(2), 2, matrix(c(10,3,3,2),2,2))
