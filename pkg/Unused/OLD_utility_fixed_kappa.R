# Make the data for the n~p regime:
## Fix n and kappa
makeRegressionData2 <- function(n, kappa, m, betas, link, sigma){
  p <- floor(n*kappa)
  N <- n * m
  X <-  matrix(rnorm(p*N), nrow=N, ncol=p)
  linear.effect <- X %*% betas
  y <- link(linear.effect) + rnorm(N, 0, sigma)
  result <- list(y=y,X=X)
  return(result)
}
## Testing:
# n <- 1e3
# kappa <- 0.2
# m <- 1e2
# betas <- rnorm(p, 0, 1/sqrt(p))
# makeRegressionData2(n, kappa, m, betas, identity, 1)



# Fix n, kappa, m
## Compute p and N
compareParallelizeRegression2 <- function(
  n, kappa, m, FITTER, COEFS, plot.ind=TRUE, link=identity, sigma=1){
  
  p <- floor(n*kappa)
  N <- n * m
  
  betas <- rnorm(p, 0, 1/sqrt(p))
  betas <- betas/ sqrt(betas%*%betas)
  
  .data <- makeRegressionData2(n, kappa, m, betas, link, sigma)
  .estimates <- analyzeParallel(.data$y, .data$X, m, FITTER, COEFS)
  
  if(plot.ind) {
    plot(averaged~centralized, data=.estimates)
    abline(0, 1, col='grey')
    title(paste('m=',m,'p=',p, 'n=',N/m))
  }
  
  errorFun <- function(estim) (estim-betas)^2 %>% sum #%>% sqrt
  result <- lapply(.estimates, errorFun)
  invisible(result)
}
## Testing:
# n <- 1000
# kappa <- 1e-1
# m <- 10
# plot.ind <- TRUE
# FITTER <-  function(y, x,...) lm(y~x,...)
# COEFS <- function(x) coef(x)[-1]
# .test <- compareParallelizeRegression2(n, kappa, m, FITTER, COEFS, plot.ind)
# .test


getMSERegression2 <- function(replications, n, kappa, m, FITTER, COEFS){
  l2.distances <- replicate(replications,{
    estimates <- compareParallelizeRegression2(
      n, kappa, m, FITTER, COEFS, plot.ind = FALSE)
  })
  
  A <- mean(unlist(l2.distances['averaged',]))
  B <- mean(unlist(l2.distances['centralized',]))
  result <- A/B
  return(result)
}
## Testing:
# n <- 1e3
# replications <- 1e1
# FITTER <-  function(y, x,...) lm(y~x,...)
# COEFS <- function(x) coef(x)[-1]
# getMSERegression2(replications, n, kappa, m, FITTER, COEFS)


# Make classification data for high_dim regime.
makeClassificationData2 <- function(n, kappa, m, betas){
  p <- floor(n*kappa)
  N <- n * m
  X <-  matrix(rnorm(p*N), nrow=N, ncol=p)
  linear.effect <- X %*% betas
  #   probs <- pnorm(linear.effect, sd=sqrt(betas%*%betas)*4)
  probs <- plogis(linear.effect)
  y <- factor(rbinom(N, 1, probs))
  result <- list(y=y,X=X)
  return(result)
}
## Testing:
# m <- 10
# betas <- rnorm(p, 0, 1/sqrt(p))
# n <- 1000
# kappa <- 1e-1

# Return the MSE of the averaged and centralized estimators.
# Hold kappa,n,m fixed and adapt p and N.
# Average over parameters.
compareParallelizeClasification2 <- function(
  n, kappa, m, FITTER, COEFS, plot.ind=TRUE){
  
  p <- floor(n*kappa)
  N <- n * m
  
  betas <- rnorm(p)
  betas <- betas / sqrt(betas %*% betas)
  
  .data <- makeClassificationData2(n, kappa, m, betas)
  .estimates <- analyzeParallel(.data$y, .data$X, m, FITTER, COEFS)
  
  if(plot.ind) {
    plot(.estimates$averaged~betas, col='black')
    points(.estimates$centralized~betas, col='red')
    abline(0, 1, col='grey')
    #     plot(averaged~centralized, data=.estimates)
    #     abline(0, 1, col='grey')
    #     title(paste('m=',m,'p=',p, 'n=',N/m))
  }
  
  errorFun <- function(estim) (estim-betas)^2 %>% sum
  MSEs <- lapply(.estimates, errorFun)
  invisible(MSEs)
}
## Testing:
# n <- 1e3
# kappa <- 1e-1
# m <- 1e1
# plot.ind <- FALSE
# FITTER <- function(x,y,...) glm(y~x, family = binomial,...)
# COEFS <- function(fit) coef(fit)[-1] 
# .test <- compareParallelizeClasification2(n, kappa, m, FITTER, COEFS, plot.ind)
# .test


# Compute the MSE of the two estimators and average over runs.
getMSEClassification2 <- function(replications, n, kappa, m, FITTER, COEFS){
  l2.distances <- replicate(replications,{    
    compareParallelizeClasification2(n, kappa, m, FITTER, COEFS, FALSE)
  })
  
  A <- l2.distances['averaged',] %>% unlist %>% mean
  B <- l2.distances['centralized',] %>% unlist %>% mean
  result <- A/B
  return(result)
}
## Testing:
# replications <- 1e1
# n <- 1e3
# kappa <- 1e-1
# m <- 1e1
# FITTER <- function(x,y,...) glm(y~x, family = binomial,...)
# COEFS <- function(fit) coef(fit)[-1] 
# getMSEClassification2(replications, n, kappa, m, FITTER, COEFS)


frameMSEs2 <- function(MSEs, ns){
  MSEs.framed <- data.frame(MSEs=MSEs, ns=ns)
  return(MSEs.framed)
}
## Testing:


# Plot results of fixed p regime:
plotMSE2 <- function(MSEs, ns, truth,
                     the.title, 
                     ylab='', 
                     ylim=c(0,2)){
  
  MSEs.framed <- frameMSEs2(MSEs, ns)
  
  plot.1 <- ggplot(data = MSEs.framed, aes(x =ns, y=MSEs))+
    labs(title = the.title)+
    ylab(y.lab)+
    ylim(ylim)+
    xlab(expression(n))+
    geom_point(size=2)+
    stat_summary(fun.data=mean_cl_normal, 
                 colour="black", 
                 geom='line', 
                 size = 0.5)+
    scale_x_continuous(trans=log10_trans())+
    theme_bw()+
    theme(text = element_text(size=20))+
    geom_hline(yintercept=truth, lty=2)+
    geom_hline(yintercept=1, lty=1)
  return(plot.1)  
}
## Testing
# plotMSE(MSEs.0, p, kappas, replications , "test")
