truthNULL <- function(kappa,m) 100
truthOLS <- function(kappa,m) (1 - kappa/m) / (1-kappa)  
truthAbsolute <- function(kappa,m) 1 + 0.9 * kappa * (1-1/as.numeric(as.character(m)))


# Return a frame with all simulation configurations
makeConfiguration_fixKappa <- function(reps, 
                                       m, 
                                       n, 
                                       kappa, 
                                       lambda=NA,
                                       model, 
                                       link=identity, 
                                       sigma=1, 
                                       beta.maker,
                                       beta.star.maker, 
                                       data.maker, 
                                       truth.fun,
                                       name){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      kappa=kappa, 
                                      n=n,
                                      lambda=lambda,
                                      model=list(model),
                                      link=c(link),
                                      sigma=sigma, 
                                      data.maker=c(data.maker),
                                      truth=NA,
                                      name)
  
  
  configurations.frame %<>% 
    mutate(p=ceiling(n*kappa), N=n*m) 
  
  if(!missing(truth.fun)) {
    configurations.frame %<>% 
      mutate(truth=truth.fun(kappa=kappa, m=m)) 
  }
    
  configurations.frame$beta <- lapply(configurations.frame$p, beta.maker)
  configurations.frame$beta.star <- beta.star.maker %>% 
    mapply(beta=configurations.frame$beta, 
           lambda=configurations.frame$lambda)
  
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










# For each configuration, average MSEs, and get ratio between algorithms
getMSERatio <- function(x){
  
  getRatio <- function(y) y[['averaged']]/y[['centralized']]
  ratio <- x %>% colMeans %>% getRatio
  return(ratio)
}
## Testing:
# .configurations <- makeConfiguration_fixKappa(reps=2e1, m=c(5e0, 1e1), n=seq(1e2,2e2,length.out=2) , kappa=0.5, model=my.ols, link=identity, sigma=1, beta.maker=makeBetasRandom, beta.star.maker=BetaStarIdentity,  data.maker=makeRegressionData, truth.fun=truthOLS) 
# .MSEs <- apply(.configurations, 1, replicateMSE)




## Get MSE results and configuration and return data.frame for plotting.
frameMSEs_fixKappa <- function(MSEs, configurations){
  
  MSEs.list <- lapply(MSEs, extractor)
  MSEs.frame <- MSEs.list %>% 
    sapply(getMSERatio) %>%
    as.data.frame %>%
    setNames('average')
  
  MSEs.framed <- data.frame(configurations, MSEs.frame) 
  MSEs.framed %<>% mutate(m=as.factor(m))
  
  return(MSEs.framed)
}
## Testing:
# .configurations <- makeConfiguration_fixKappa(reps=2e1, m=5, n=seq(2e2, 3e2,length.out=2) , kappa=0.5, model=my.ols, link=identity, sigma=1, beta.maker=makeBetasRandom, beta.star.maker=BetaStarIdentity,  data.maker=makeRegressionData, truth.fun=truthOLS) 
# .MSEs <- apply(.configurations, 1, replicateMSE)
# # save(.MSEs, .configurations, file='RData/18.1.2015.RData')
# # load(file='RData/18.1.2015.RData')
# .MSEs.framed <- frameMSEs_fixKappa(.MSEs, .configurations)
# .MSEs.framed %>% select(kappa, n, m, p, truth, average)






# Plot results of fixed p regime:
plotMSEs_fixKappa <- function(MSEs.framed, 
                              the.title, 
                              y.lab='', 
                              y.lim=c(1,2), 
                              robust=FALSE,
                              legend.position='none',
                              line=TRUE){
  if(robust){
    MSEs.framed %<>% mutate(center=median, arm=0)
  }  else{
    MSEs.framed %<>% mutate(center=average, arm=0)
  }
  
  plot.1 <- ggplot(data = MSEs.framed, aes(x=n, y=center, colour=m, group=m))+
    geom_point()  
  if(!is.na(MSEs.framed$truth[1])){
    plot.1 <- plot.1 + geom_hline(aes(yintercept=truth, col=m), linetype=2)
  }
  
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
# .configurations <- makeConfiguration_fixKappa(reps=2e1, m=c(5e0, 1e1), n=seq(2e2, 3e2, length.out=2) , kappa=0.5, model=my.ols, link=identity, sigma=1, beta.maker=makeBetasRandom, beta.star.maker=BetaStarIdentity,  data.maker=makeRegressionData, truth.fun=truthOLS) 
# .MSEs <- apply(.configurations, 1, replicateMSE)
# # save(.MSEs, .configurations, file='RData/18.1.2015.RData')
# # load(file='RData/18.1.2015.RData')
# .MSEs.framed <- frameMSEs_fixKappa(.MSEs, .configurations)
# .MSEs.framed %>% select(kappa, n, m, p, truth, average)
# plotMSEs_fixKappa(.MSEs.framed, 'test', robust = FALSE)
