# Fix n, kappa, m
## Compute p and N
compareParallelizeRegression2_ms <- function(
  n, kappa, ms, FITTER, COEFS, plot.ind=TRUE, link=identity, sigma=1){
  
  p <- floor(n*kappa)
  
  betas <- rnorm(p, 0, 1/sqrt(p))
  betas <- betas / sqrt(betas %*% betas)
  n <- floor(p/kappa)
    
  .data <- makeRegressionData(p, n* max(ms), betas, link, sigma )
  
  MSEs <- getMSEs(ms, n, .data, betas, FITTER, COEFS, plot.ind)
  
  invisible(MSEs)  
}
## Testing:
# n <- 1000
# kappa <- 1e-1
# plot.ind <- TRUE
# FITTER <-  my.ols
# COEFS <- coefs.ols
#  .test <- compareParallelizeRegression2_ms(n = 1e3, kappa = 1e-2, ms = c(1e1,1e2), FITTER = my.ols, COEFS = coefs.ols, plot.ind = FALSE, link = identity, sigma = 1)
# .test


getMSERegression2_ms <- function(replications, n, kappa, ms, FITTER, COEFS, link=identity, sigma=1){
  results <- replicate(replications,{
    compareParallelizeRegression2_ms(
      n, kappa, ms, FITTER, COEFS, plot=FALSE, link, sigma)
  })
  
  result <- matrix(NA, nrow=length(ms), ncol=2, dimnames=list(NULL, c("mean","se")))
  for(i in seq_along(ms)){
    m <- ms[i]
    MSE.averaged<- sapply(results[m,], function(x) x$averaged) %>% mean
    MSE.centralized<- sapply(results[m,], function(x) x$centralized) %>% mean
    result[i,1] <- MSE.averaged/MSE.centralized
    
    SE.averaged <- sapply(results[m,], function(x) x$averaged) %>% stderr
    SE.centralized <- sapply(results[m,], function(x) x$centralized) %>% stderr
    result[i,2] <- SE.averaged*SE.centralized
  }
  
  return(result)
}
## Testing:
# getMSERegression2_ms(1e1, 1e3, 1e-2, c(1e1,1e2), my.ols, coefs.ols)




rowSDs <- function(A){
  apply(A, 1, sd)
}
## Testing
# matrix(c(1,1,2,2), byrow = TRUE, ncol=2) %>%  rowSDs


frameMSE2_ms <- function(MSEs, ns, ms, kappa, truthFUN){
  MSEs.framed <- cbind(
      do.call(rbind, MSEs),
      ns=rep(ns, each = 2),
      m=rep(ms, times=length(ns)) )  %>% 
    as.data.frame %>%
    mutate(truth=truthFUN(kappa,m) ) %>%
    set_names(c('MSEs', 'SEs', 'ns', 'm','truth'))
  
  return(MSEs.framed)
}
## Testing:
# MSEs <- MSEs2_ms.0
# frameMSE2_ms(MSEs2_ms.0, ns, kappa, truthOLS )



# Plot results of fixed p regime:
plotMSE2_ms <- function(MSEs, 
                        ns,
                        ms,
                        the.title, 
                        kappa,
                        y.lab= '', 
                        y.lim=c(1,1.5),
                        x.lim=c(5e2, 3e3),
                        ...){
  
  MSEs.framed <- frameMSE2_ms(MSEs, ns, ms, kappa,...)
  
  plot.1 <- ggplot(data = MSEs.framed, 
                   aes(x=ns, y=MSEs, colour=as.factor(m), group=as.factor(m)))+
    geom_point()+
    geom_line(size=0.5) +
    geom_segment(aes(x=ns, xend=ns, y=MSEs+SEs, yend=MSEs-SEs))
  
  plot.1 <- plot.1 +
    labs(title = the.title)+
    ylab(y.lab)+
    geom_hline(aes(yintercept=truth), linetype=2)+
    ylim(y.lim)+
    xlab(expression(n))+
    scale_x_continuous(trans=log10_trans(), breaks=c(1e2, 5e2, 1e3, 2e3), limits=x.lim)+
    theme_bw()+
    theme(text = element_text(size=20), legend.position = "none") 
  return(plot.1)  
}
## Testing
# the.title <- 'test'
# y.lab= ''
# y.lim=c(1,1.3)
# MSEs <- MSEs_ms.0
# plotMSE2_ms(MSEs2_ms.0, ns, "test", kappa = 0.1, truthFUN=1)


compareParallelizeClassification2_ms <- function(
  n, kappa, ms, FITTER, COEFS, plot.ind=TRUE, link=plogis){
  
  p <- floor(n*kappa)
  
  betas <- rnorm(p, 0, 1/sqrt(p))
  betas <- betas / sqrt(betas %*% betas)
  n <- floor(p/kappa)
  
  .data <- makeClassificationData(p, n* max(ms), betas, link )
  
  MSEs <- getMSEs(ms, n, .data, betas, FITTER, COEFS, plot.ind)
  
  invisible(MSEs)  
}
## Testing:
# n <- 1000
# kappa <- 1e-1
# plot.ind <- TRUE
# FITTER <-  my.ols
# COEFS <- coefs.ols
# .test <- compareParallelizeRegression2_ms(n = 1e3, kappa = 1e-2, ms = c(1e1,1e2), FITTER = my.ols, COEFS = coefs.ols, plot.ind = FALSE, link = identity, sigma = 1)
# .test

getMSEClassification2_ms <- function(
  replications, n, kappa, ms, FITTER, COEFS, link=plogis){
  
  results <- replicate(replications,{
    compareParallelizeClassification2_ms(
      n, kappa, ms, FITTER, COEFS, plot=FALSE, link)
    })
    
    result <- matrix(NA, nrow=length(ms), ncol=2, dimnames=list(NULL, c("mean","se")))
    for(i in seq_along(ms)){
      m <- ms[i]
      MSE.averaged<- sapply(results[m,], function(x) x$averaged) %>% mean
      MSE.centralized<- sapply(results[m,], function(x) x$centralized) %>% mean
      result[i,1] <- MSE.averaged/MSE.centralized
      
      SE.averaged <- sapply(results[m,], function(x) x$averaged) %>% stderr
      SE.centralized <- sapply(results[m,], function(x) x$centralized) %>% stderr
      result[i,2] <- SE.averaged*SE.centralized
    }
    
    return(result)}
## Testing:
# getMSEClassification2_ms(1e1, 1e3, 1e-2, c(1e1,1e2), my.ols, coefs.ols)
