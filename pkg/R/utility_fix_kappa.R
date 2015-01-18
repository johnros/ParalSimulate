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










# For each configuration, average MSEs, and get ratio between algorithms
getMSERatio <- function(x){
  getRatio <- function(y) y[['averaged']]/y[['centralized']]
  ratio <- x %>% colMeans %>% getRatio
}
