frameMSEs <- function(MSEs, configurations){
  
  MSEs.list <- lapply(MSEs, extractor)
  
  getRatio <- function(x) x[,'ratio']
    
  ## TODO: make plot
  MSEs.list %>% 
    lapply(getRatio) %>% {
      average <- lapply(., mean)
      std.dev <- lapply(., sd)
      c(average, std.dev)
    }
  
  
  
  
  ########
  ms <- dimnames(MSEs [[1]])[[1]]
  
  MSEs.framed <- NULL
  for(i in seq_along(kappas)){
    .temp <- MSEs[[i]] %>% 
      t %>% 
      as.data.frame %>%
      gather_(key='m' , value='MSEs', ms) %>% 
      mutate(kappa=kappas[i], 
             ns= (p/kappa) %>% floor)
    MSEs.framed <- rbind(MSEs.framed, .temp)
  }
  return(MSEs.framed)
}
## Testing:
.configurations <- makeConfiguration(
  reps = 1e1, m = 1e1, p = 5e1, 
  n = seq(2e2,5e2,length.out=3) , 
  kappa = 5e-1, model = my.ols, link = identity, sigma = 1e1 ) 
.MSEs <- apply(.configurations, 1, replicateMSE)
MSEs <- .MSEs
configurations <- .configurations

frameMSE_ms(.MSEs, p, kappas)





# Plot results of fixed p regime:
plotMSE_ms <- function(MSEs, p, kappas,
                       the.title, 
                       y.lab= '', 
                       y.lim=c(1,2)){
  
  MSEs.framed <- frameMSE_ms(MSEs, p, kappas)
  
  plot.1 <- ggplot(data = MSEs.framed, aes(x=ns, y=MSEs, colour=m, group=m))+
    stat_summary(fun.data=mean_cl_normal, 
                 geom='errorbar', 
                 size = 1)+
    stat_summary(fun.data=mean_cl_normal, 
                 geom='line', 
                 size = 0.5)
  plot.1 <- plot.1 +
    labs(title = the.title)+
    ylab(y.lab)+
    ylim(y.lim)+
    xlab(expression(n))+
    scale_x_continuous(trans=log10_trans(), breaks=c(5e2, 1e3, 5e3))+
    theme_bw()+
    theme(text = element_text(size=20), legend.position = "none") 
  return(plot.1)  
}
## Testing

plotMSE_ms(.MSEs, p, kappas, replications , the.title)
