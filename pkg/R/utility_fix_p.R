
# First order approximation of MSE for OLS in fix_p:
ApproxMSE_OLS_fixp <- function(n,p,m,sigma.sq){
  p/(n*m)*sigma.sq
}
## Testing:
# ApproxMSE_OLS_fixp(100,10,2, 1)


# Second order approximation of MSE for OLS in fix_p fix_N:
ApproxMSE_OLS_fixp2.matrix <- function(n, p, m, sigma.sq, Sigma=diag(p)){
  Sigma.inv <- solve(Sigma)
  I <- diag(p)
  
  zeta0 <- 0
  
  zeta1 <- sigma.sq * Sigma.inv
  
  zeta2 <- - sigma.sq * Sigma.inv * (1+p)
    
  zeta3 <- sigma.sq * Sigma.inv * (1+p)
    
  zeta4 <- sigma.sq * Sigma.inv * (1+p)
    
  N1 <- m*n
  N2 <- m*n^2
  M <-  (m-1)/(m*n^2) * zeta0 + 1/N1 * zeta1 + 1/N2 * (2* zeta2 + zeta3+ 2*zeta4) 

  return(M)
}
## Testing:
# ApproxMSE_OLS_fixp2.matrix(n=100, p=10, m=3, sigma.sq=1 )


ApproxMSE_OLS_fixp2 <- function(...){
  Trace(ApproxMSE_OLS_fixp2.matrix(...))
}
## Testing:
# ApproxMSE_OLS_fixp2(n=100, p=10, m=3, sigma.sq=1)


# First order approximation of MSE for Ridge in fix_p:
ApproxMSE_Ridge_fixp <- function(n, p, m, lambda, sigma.sq, beta){
  if(is.list(beta)) beta <- unlist(beta)
  
  ( (p+1) * (lambda)^2/(1+lambda)^4 * SSQ(beta) + (p * sigma.sq) ) / (n*m) 
}
## Testing:
# ..p <- 1e1
# ApproxMSE_Ridge_fixp(n = 1e3, p = ..p, lambda = 2, beta = runif(..p))




# Second order approximation of MSE for Ridge in fix_p:

ll <- function(lambda,x,y){
  lambda^x/(1+lambda)^y
}

ApproxMSE_Ridge_matrix <- function(n, p, m, lambda, sigma.sq, beta){
  
  B <- outer(beta, beta)
  I <- diag(p)
  A <- SSQ(beta) * I
  l <- function(x,y) ll(lambda,x,y)
  
  zeta0 <-  l(2,6) * (p+1)^2 * B
  
  zeta1 <- l(2,4)*(B+A) + l(0,2)*sigma.sq*I
  
  zeta2 <- - l(2,5)*(B*(p+4)+A*(p+2))- l(0,3)*(p+1)*sigma.sq*I
  
  zeta3 <-  l(2,6)*B*(p^2+p+5) + l(2,6)*A*(p+2)+ l(0,4)*sigma.sq*(p+1)*I
  
  zeta4 <- l(2,5)*B*(2*p+5) + l(2,5)*A*(2*p+3) + l(0,3)*sigma.sq*(p+1)*I
  
  N1 <- m*n
  N2 <- m*n^2
  M <-  (m-1)/(m*n^2) * zeta0 + 1/N1 * zeta1 + 1/N2 * (2* zeta2 + zeta3+ 2*zeta4) 
  
  return(M)
}
## Testing:
# sum(diag(ApproxMSE_Ridge_matrix(100,10,5,2,1,rnorm(10))))





ApproxMSE_Ridge_fixp2 <- function(n, p, m, lambda, sigma.sq, beta){
  if(is.list(beta)) beta <- unlist(beta)
  
  # Return trace of second order error matrix
  sum(diag(ApproxMSE_Ridge_matrix(n, p, m, lambda, sigma.sq, beta)))
}
## Testing:
# ApproxMSE_Ridge_fixp2(100,10,5,2,1,rnorm(10))





# Generate true parameters
makeBetasRandom <- function(p, beta.norm){
  beta <- rnorm(p)
  beta <- beta / sqrt(beta %*% beta) * beta.norm
  return(beta)
}
## Testing:
# makeBetasRandom(100) 


makeBetasDeterministic <- function(p, beta.norm){
  beta <-  1:p
  beta <- beta / sqrt(beta %*% beta) * beta.norm
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
                              p=NA, 
                              kappa=NA,
                              n, 
                              lambda=NA,
                              model, 
                              link=identity, 
                              sigma=1, 
                              beta.maker,
                              beta.norm,
                              beta.star.maker, 
                              data.maker,
                              name){
  
  if(is.na(p) && is.na(kappa)) stop('Either p or kappa are required!')
  if(!is.na(p) && !is.na(kappa)) stop('Only p or kappa are required!')
    
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      p=p, 
                                      kappa=kappa,
                                      n=n, 
                                      model=list(model),
                                      link=c(link),
                                      sigma=sigma, 
                                      data.maker=c(data.maker),
                                      lambda=lambda,
                                      name=name,
                                      beta.norm=beta.norm )
  
  if(is.na(p)) {
    configurations.frame %<>% mutate(p=n*kappa)    
  }
  
  configurations.frame %<>% filter(p<n)
  
  configurations.frame %<>% mutate(N=n*m)    
  configurations.frame$beta <- lapply(configurations.frame$p, 
                                      beta.maker, beta.norm=beta.norm)
  configurations.frame$beta.star <- lapply(configurations.frame$beta, 
                                           beta.star.maker, lambda=lambda)
  
  
  # Add theoretical performances
  
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


analyzeParallel <- function(data, m, model, N, p, ...){
  y <- data$y
  X <- data$X
  
  FITTER <- model$fitter
  COEFS <- model$coefs
  
  ## Centralized Solution:
  the.fit <- FITTER(y=y, x=X, ...)
  center.coefs <- COEFS(the.fit)
  
  ## Parallelized solution:
  machine.ind <- rep(1:m, times = N/m)[sample(1:N)]
    
  # Check if there are enough observations per machine
  .min <- machine.ind %>% table %>% min
  if(.min < p) stop('Not enough observations per machine')
  
  
  machine.wise <-  matrix(NA, ncol = m, nrow = ncol(X))
  for(i in seq_len(m)){
    .the.fit <- FITTER(y=y[machine.ind==i], x=X[machine.ind==i,],...)
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



# Compute the bias from the output ot replicateMSE (complicated structure!)
getBiasNorm <- function(x) {
  getBias(x) %>% 
    SSQ %>%
    sqrt
}
## Testing



# Compute the parallelizastion bias from many replications
getBias <- function(x){
  x['errors',] %>%
    lapply(function(x) x[['averaged']]) %>% 
    do.call(cbind,.)
}




## Get errors (MSE and bias) for each configuration and return data.frame.
frameMSEs <- function(MSEs, configurations, coordinate=50){
  
  # Compute norm of bias
  parallel.bias <- lapply(MSEs, getBias)
  
  bias.norm <- parallel.bias %>% sapply( function(x) {x %>% SSQ %>% sqrt})
      
  bias.mean <- parallel.bias %>% sapply( function(x) {x[length(x)]}) # Use the error in the last coordinate
  
  bias.single <- parallel.bias %>% sapply( function(x) {x[coordinate]})
      
  # Frame MSE of each configuration
  MSEs.list <- lapply(MSEs, cleanMSEs)
  
  ratios.frame <- MSEs.list %>% 
    lapply(function(x) x['ratio',]) %>% {
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
    setNames('parallel.MSE')

  #   configurations %<>% 
  #     mutate(
  #       mse.highdim=mse.fun.highdim(lambda=lambda, p=p, N=N, m=m, beta=beta.star),
  #       mse.fixp=mse.fun.fixp(lambda=lambda, p=p, N=N, m=m, beta=beta.star),
  #       bias.highdim=bias.fun.highdim(lambda=lambda, p=p, N=N, m=m, beta=beta.star),
  #       bias.fixp=bias.fun.fixp(lambda=lambda, p=p, N=N, m=m, beta=beta.star),
  #     )    
  
  
  
  MSEs.framed <- data.frame(configurations, 
                            ratios.frame, 
                            MSEs.frame.parallel,
                            bias.norm=bias.norm,
                            bias.mean=bias.mean,
                            bias.single=bias.single,
                            error.asympt=NA) 
    
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
                     y.lim, 
                     center, 
                     legend.position="none",
                     jitter=0, 
                     line=TRUE,
                     lty=3,
                     size=1){
  
  
  if(center=='MSE'){
    MSEs.framed %<>% mutate(center=parallel.MSE, arm=0)  
  }
  if(center=='bias.mean'){
    MSEs.framed %<>% mutate(center=bias.mean, arm=0)  
  }
  if(center=='bias.norm'){
    MSEs.framed %<>% mutate(center=bias.norm, arm=0)  
  }    
  if(center=='median'){
    MSEs.framed %<>% mutate(center=median, arm=mad)
  }
  if(center=='average'){
    MSEs.framed %<>% mutate(center=average, arm=std.dev)
  }
  
  MSEs.framed %<>% mutate(n=n+runif(nrow(MSEs.framed),-jitter,jitter),
                            m=as.factor(m))
  
  
  # Actual Plotting:
  plot.1 <- ggplot(data = MSEs.framed, aes(x=n, y=center, colour=m))+
    geom_point(aes(shape=m), size=size)+
    geom_segment(aes(xend=n, y=center+arm, yend=center-arm))  
  
  plot.1 <- plot.1 +
    labs(title = the.title)+
    ylab(y.lab)+
    xlab(expression(n))+
    #scale_x_continuous(trans=log_trans(base = 10), breaks=c(5e2, 1e3, 5e3))+
    theme_bw()+
    theme(text = element_text(size=20), legend.position = legend.position) 
  
  if(line){
    plot.1 <- plot.1 + geom_line(linetype=lty)
    }
  
  if(!missing(y.lim)){
    plot.1 <- plot.1 + ylim(y.lim) 
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

## Deprecated!
# For creating lty='b' type lines using package 'ggExtra'
# geom_barbed <- GeomBarbed$build_accessor()

# Plot results for choosing m:
plotMSEs2 <- function(MSEs.framed, 
                      the.title, 
                      y.lab= '', 
                      y.lim, 
                      robust=FALSE, 
                      legend.position="none",
                      jitter=0, 
                      line=TRUE, 
                      fix,
                      center,
                      rounding=-2,
                      lty=3,
                      lwd=1,
                      lwd.error=0.5,
                      lty.error=1,
                      point.size=1,
                      point.size.error=0,
                      scale.y=scale_y_continuous()){

  
  MSEs.framed %<>% mutate(arm=0, 
                          n=as.factor(n),
                          N=as.factor(round(N,rounding)),
                          p=as.factor(p))
  
  if(center=='MSE'){
    MSEs.framed %<>% mutate(center=parallel.MSE)  
  }
  if(center=='bias.mean'){
    MSEs.framed %<>% mutate(center=bias.mean)  
  }
  if(center=='bias.norm'){
    MSEs.framed %<>% mutate(center=bias.norm)  
  }
  if(center=='bias.single'){
    MSEs.framed %<>% mutate(center=bias.single)  
  }
  
      
  if(fix=='N'){
    plot.1 <- ggplot(data = MSEs.framed, 
                     aes(x=m, y=center, colour=N, group=N))
  }
  if(fix=='n'){
    plot.1 <- ggplot(data = MSEs.framed, 
                     aes(x=m, y=center, colour=n, group=n))
  }
  if(fix=='p'){
    plot.1 <- ggplot(data = MSEs.framed, 
                     aes(x=m, y=center, colour=p, group=p))
  }
  if(fix=='Np'){
    plot.1 <- ggplot(data = MSEs.framed, 
                     aes(x=m, y=center, 
                         shape=p,
                         colour=p,
                         #shape = N,
                         #group=interaction(N, p)))
                         group=p))
  }
  
  if(!missing(y.lim)) {
    plot.1 <- plot.1 + ylim(y.lim)
  }
  
  
  # Actual plotting
  plot.1 <- plot.1 + 
    # geom_line(lwd=lwd) + 
    geom_point(size=point.size) +
    geom_line(linetype=lty, lwd=lwd)+
    labs(title = the.title)+
    ylab(y.lab)+
    xlab(expression(m))+
    #scale_x_continuous(trans=log_trans(base = 10), breaks=c(5e2, 1e3, 5e3))+
    theme_bw()+
    theme(text = element_text(size=20), legend.position = legend.position)+
    scale.y
  
  if(!is.na(MSEs.framed$error.asympt[1]))  
    plot.1 <- plot.1 + 
    geom_line(aes(x=m, y=error.asympt), linetype=lty.error, lwd=lwd.error)+
    geom_point(aes(x=m, y=error.asympt, shape=p),  size=point.size.error)
    
    
  
  return(plot.1)  
}
## Testing:

