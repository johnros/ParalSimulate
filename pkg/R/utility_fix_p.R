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
                              lambda,
                              model, 
                              link=identity, 
                              sigma=1, 
                              beta.maker,
                              beta.star.maker, 
                              data.maker,
                              name){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      p=p, 
                                      n=n, 
                                      model=list(model),
                                      link=c(link),
                                      sigma=sigma, 
                                      data.maker=c(data.maker),
                                      name)
  
  configurations.frame %<>% filter(p<n)
  
  configurations.frame %<>% mutate(N=n*m)    
  configurations.frame$beta <- lapply(configurations.frame$p, beta.maker)
  configurations.frame$beta.star <- lapply(configurations.frame$beta, beta.star.maker, lambda=lambda)
  
  return(configurations.frame)
}
## Testing:
#   OLS:
# makeTest()
# .configurations <- makeConfiguration(reps = .reps, m = .m, p = .p, n = .n, kappa = .kappa, lambda = 2, model = .model, link = identity, sigma = 1, beta.maker = makeBetasRandom, beta.star.maker = identityBeta, data.maker = makeRegressionData)
# .configurations %>% dim
# .configurations %>% names
# str(.configurations)
# .configurations$beta
#   Ridge:
# makeTest()
# .configurations <- makeConfiguration(reps = .reps, m = .m, p = .p, n = .n, kappa = .kappa, lambda = 2, model = .model, link = identity, sigma = 1, beta.maker = makeBetasRandom, beta.star.maker = identityBeta, data.maker = makeRegressionData)
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


analyzeParallel <- function(data, m, model, N, p, beta.star, ...){
  y <- data$y
  X <- data$X
  
  FITTER <- model$fitter
  COEFS <- model$coefs
  
  ## Centralized Solution:
  the.fit <- FITTER(y=y, x=X, beta.star=beta.star)
  center.coefs <- COEFS(the.fit)
  
  ## Parallelized solution:
  machine.ind <- rep(1:m, times = N/m)[sample(1:N)]
    
  # Check if there are enough observations per machine
  .min <- machine.ind %>% table %>% min
  if(.min < p) stop('Not enough observations per machine')
  
  
  machine.wise <-  matrix(NA, ncol = m, nrow = ncol(X))
  for(i in seq_len(m)){
    .the.fit <- FITTER(y=y[machine.ind==i], x=X[machine.ind==i,],beta.star=beta.star)
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
  data.maker <- configuration[['data.maker']]
  data <- do.call(data.maker, configuration)
  coefs <- do.call(analyzeParallel, c(list(data=data), configuration))
  errors <- list(
    averaged= coefs$averaged - configuration$beta.star,
    centralized= coefs$centralized - configuration$beta.star)  
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



# Get configuration and return MSE and ratio of each replication
replicateMSE <- function(configuration){
  MSEs <- replicate(configuration$replications,{
    errors <- getErrors(configuration)
    errorFun(errors)
  })
  return(MSEs)
}
## Testing:
# .configuration <- .configurations[1,,drop=FALSE]
# apply(.configuration, 1, replicateMSE)


## Get MSE results and configuration and return data.frame for plotting.
frameMSEs <- function(MSEs, configurations){
  
  getRatio <- function(x) x[,'ratio']
  
  MSEs.list <- lapply(MSEs, extractor)
  MSEs.frame <- MSEs.list %>% 
    lapply(getRatio) %>% {
      average <- sapply(.,mean, na.rm=TRUE)
      std.dev <- sapply(.,sd, na.rm=TRUE)
      median <- sapply(.,median, na.rm=TRUE)
      mad <- sapply(.,mad, na.rm=TRUE)
      cbind(average=average, std.dev=std.dev, median=median, mad=mad)
    } %>% 
    as.data.frame
  
  MSEs.frame.parallel <- MSEs.list %>% 
    sapply(getMSEParallel) %>%
    as.data.frame %>%
    setNames('parallel')
  
  MSEs.framed <- data.frame(configurations, MSEs.frame, MSEs.frame.parallel) 
    
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
                     y.lim=c(1,2), 
                     robust=FALSE, 
                     legend.position="none",
                     jitter=0, 
                     line=TRUE){
  
  if(robust){
    MSEs.framed %<>% mutate(center=median, arm=mad)
  }  else{
    MSEs.framed %<>% mutate(center=average, arm=std.dev)
  }
  
    MSEs.framed %<>% mutate(n=n+runif(nrow(MSEs.framed),-jitter,jitter),
                            m=as.factor(m))
  
  plot.1 <- ggplot(data = MSEs.framed, aes(x=n, y=center, colour=m, group=m))+
    geom_point()+
    geom_segment(aes(xend=n, y=center+arm, yend=center-arm))  
  
  plot.1 <- plot.1 +
    labs(title = the.title)+
    ylab(y.lab)+
    ylim(y.lim) +
    xlab(expression(n))+
    #scale_x_continuous(trans=log_trans(base = 10), breaks=c(5e2, 1e3, 5e3))+
    theme_bw()+
    theme(text = element_text(size=20), legend.position = legend.position) 
  
  if(line){
    plot.1 <- plot.1 + geom_line()
  }
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





# Make data for classification problems:
makeClassificationData <- function(p, N, betas, link,...){
  X <-  matrix(rnorm(p*N), nrow=N, ncol=p)
  linear.effect <- X %*% betas
  probs <- link(linear.effect)
  y <- rbinom(N, 1, probs) %>% factor
  result <- list(y=y,X=X)
  return(result)
}
## Testing:
# .p <- 1e2
# .N <- 1e4
# .betas <- makeBetasRandom(.p)
# .classification.data <- makeClassificationData(.p, .N, .betas, sigmoid)
# .classification.data$y



# Plot results for choosing m:
plotMSEs2 <- function(MSEs.framed, 
                      the.title, 
                      y.lab= '', 
                      y.lim=c(1,2), 
                      robust=FALSE, 
                      legend.position="none",
                      jitter=0, 
                      line=TRUE, 
                      fix){
  
  MSEs.framed %<>% mutate(center=parallel, 
                          arm=0, 
                          n=as.factor(n),
                          N=as.factor(N),
                          p=as.factor(p))
  
  if(fix=='N'){
    plot.1 <- ggplot(data = MSEs.framed, aes(x=m, y=center, colour=N, group=N))
  }
  if(fix=='n'){
    plot.1 <- ggplot(data = MSEs.framed, aes(x=m, y=center, colour=n, group=n))
  }
  if(fix=='p'){
    plot.1 <- ggplot(data = MSEs.framed, aes(x=m, y=center, colour=p, group=p))
  }
  if(fix=='Np'){
    plot.1 <- ggplot(data = MSEs.framed, 
                     aes(x=m, y=center, 
                         colour=p,
                         shape = N,
                         group=interaction(N, p)))
  }
    
  plot.1 <- plot.1 + geom_line() + geom_point()
  
  plot.1 <- plot.1 +
    labs(title = the.title)+
    ylab(y.lab)+
    xlab(expression(m))+
    #scale_x_continuous(trans=log_trans(base = 10), breaks=c(5e2, 1e3, 5e3))+
    theme_bw()+
    theme(text = element_text(size=20), legend.position = legend.position) 
}
## Testing:

