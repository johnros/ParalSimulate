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
                                       data.maker){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      kappa=kappa, 
                                      n=n, 
                                      kappa=kappa, 
                                      model=list(model),
                                      link=c(link),
                                      sigma=sigma, 
                                      data.maker=c(data.maker))
  
  
  configurations.frame %<>% 
    mutate(p=floor(n*kappa), N=n*m) 
    
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



