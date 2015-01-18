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
  
  return(MSEs.framed)
}
## Testing:
.configurations <- makeConfiguration_fixKappa(reps=2e1, m=5, n=seq(2e2, 3e2,length.out=2) , kappa=0.5, model=my.ols, link=identity, sigma=1, beta.maker=makeBetasRandom, beta.star.maker=BetaStarIdentity,  data.maker=makeRegressionData, truth.fun=truthOLS) 
.MSEs <- apply(.configurations, 1, replicateMSE)
# save(.MSEs, .configurations, file='RData/18.1.2015.RData')
# load(file='RData/18.1.2015.RData')
.MSEs.framed <- frameMSEs_fixKappa(.MSEs, .configurations)
.MSEs.framed %>% select(kappa, n, m, p, truth, average)






# Plot results of fixed p regime:
plotMSEs_fixKappa <- function(MSEs.framed, 
                              the.title, 
                              y.lab='', 
                              y.lim=c(1,2), 
                              robust=FALSE){
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
    theme(text = element_text(size=20), legend.position = "none") 
  return(plot.1)  
}
## Testing
## TODO: why loss approximation inexact?
.configurations <- makeConfiguration_fixKappa(reps=2e1, m=c(5e0, 1e1), n=seq(2e2, 3e2, length.out=2) , kappa=0.5, model=my.ols, link=identity, sigma=1, beta.maker=makeBetasRandom, beta.star.maker=BetaStarIdentity,  data.maker=makeRegressionData, truth.fun=truthOLS) 
.MSEs <- apply(.configurations, 1, replicateMSE)
# save(.MSEs, .configurations, file='RData/18.1.2015.RData')
# load(file='RData/18.1.2015.RData')
.MSEs.framed <- frameMSEs_fixKappa(.MSEs, .configurations)
.MSEs.framed %>% select(kappa, n, m, p, truth, average)
plotMSEs_fixKappa(.MSEs.framed, 'test', robust = FALSE)
