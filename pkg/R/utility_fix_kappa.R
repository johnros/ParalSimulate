truthNULL <- function(kappa,m) 100
truthOLS <- function(kappa,m) (1 - kappa/m) / (1-kappa)  
truthAbsolute <- function(kappa,m) 1 + 0.9 * kappa * (1-1/as.numeric(as.character(m)))


# Return a frame with all simulation configurations
makeConfiguration_fixKappa <- function(reps, 
                                       m, 
                                       n, 
                                       kappa, 
                                       lambda,
                                       model, 
                                       link=identity, 
                                       sigma=1, 
                                       beta.maker,
                                       beta.star.maker, 
                                       data.maker, 
                                       truth.fun){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      kappa=kappa, 
                                      n=n, 
                                      kappa=kappa, 
                                      model=list(model),
                                      link=c(link),
                                      sigma=sigma, 
                                      data.maker=c(data.maker),
                                      truth=NA)
  
  
  configurations.frame %<>% 
    mutate(p=floor(n*kappa), N=n*m) 
  
  if(!missing(truth.fun)) {
    configurations.frame %<>% 
      mutate(truth=truth.fun(kappa=kappa, m=m)) 
  }
    
  configurations.frame$beta <- lapply(configurations.frame$p, beta.maker)
  configurations.frame$beta.star <- lapply(configurations.frame$beta, beta.star.maker, lambda=lambda)
  
  return(configurations.frame)
}
## Testing:
#   OLS:
# makeTest()
# .configurations <- makeConfiguration_fixKappa(reps = .reps, m = .m, kappa=.kappa, n = .n, lambda = 2, model = .model, link = identity, sigma = 1, beta.maker = makeBetasRandom, beta.star.maker = identityBeta, data.maker = makeRegressionData)
# .configurations %>% dim
# .configurations %>% names
# str(.configurations)
# .configurations$beta
#   Ridge:
# makeTest()
# .configurations <- makeConfiguration_fixKappa(reps = .reps, m = .m, n = .n, kappa = .kappa, lambda = 2, model = .model, link = identity, sigma = 1, beta.maker = makeBetasRandom, beta.star.maker = identityBeta, data.maker = makeRegressionData)
# .configurations %>% dim
# .configurations %>% names
# str(.configurations)
# .configurations$beta



##TODO: write frameMSEs_fixKappa
## Get MSE results and configuration and return data.frame for plotting.
frameMSEs_fixKappa <- function(MSEs, configurations){
  
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
plotMSEs_fixKappa <- function(MSEs.framed, 
                     the.title, 
                     y.lab= '', 
                     y.lim=c(1,2), 
                     robust=FALSE){
  if(robust){
    MSEs.framed %<>% mutate(center=median, arm=0)
  }  else{
    MSEs.framed %<>% mutate(center=average, arm=0)
  }
  
  plot.1 <- ggplot(data = MSEs.framed, aes(x=n, y=center, colour=m, group=m))+
    geom_point()+
    geom_segment(aes(xend=n, y=center+arm, yend=center-arm))  
  
  if(!is.na(MSEs.framed$truth[1])){
    plot.1+ geom_hline(aes(yintercept=truth, col=m), linetype=2)
  }
  
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
