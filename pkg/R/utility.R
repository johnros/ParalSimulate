# Make fitting and coef extracting function
my.ols <-  list(fitter=function(y, x,...) lm(y~x-1),
                coefs= function(fit) coef(fit))

my.ridge <- list(fitter=function(y, x, lambda, ...) linearRidge(y~x-1, lambda=lambda),
                 coefs= function(fit) coef(fit))

my.log.link <- list(fitter=function(y, x, beta.star,...) {
  .start <- beta.star
  .control <- glm.control(epsilon=1e-4, maxit = 1e2 )
  glm(formula = y~x-1, family=gaussian(link='log'),  start=.start,
      control = .control)
},
coefs= function(fit) coef(fit))

my.logistic <- list(fitter=function(y, x,...) glm(y~x-1, family = binomial),
                    coefs= function(fit) coef(fit))


my.huber <- list(fitter=my.huber <- function(y, x,...) rlm(y~x-1,...), 
                 coefs= function(fit) coef(fit))

my.absolute <- list(fitter=function(y, x,...) rq(y~x-1, method="fn",...),
                    coefs=function(fit) coef(fit))




NA_fun <- function() NA

Trace <- function(A) A %>% diag %>% sum


rep.row<-function(x,n){
  result <- matrix(rep(c(x),each=n),nrow=n)
  result %<>% as.data.frame 
  names(result) <- names(x)
  return(result)
}





## make beta.star functions
## Compute risk minimizer for Ridge problem
BetaStarIdentity <- function(beta, ...){
  return(beta)
}
## Testing:
# identityBeta(rep(1,10), 2)
# identityBeta(makeBetasDeterministic(10), 2)
# identityBeta(makeBetasDeterministic(2), 2, matrix(c(10,3,3,2),2,2))








## Compute risk minimizer for Ridge problem
BetaStarRidge <- function(beta, lambda, Sigma){
  stopifnot(!is.na(lambda))
  
  if(missing(Sigma)){
    beta.star <- beta/(1+lambda)
  }
  else{
    p <- length(beta)
    beta.star <- solve(Sigma+lambda*diag(p)) %*% Sigma %*% beta
  }
  return(beta.star)
}
## Testing:
# ridgeBeta(rep(1,10), 2)
# ridgeBeta(makeBetasDeterministic(10), 2)
# ridgeBeta(makeBetasDeterministic(2), 2, matrix(c(10,3,3,2),2,2))






# The logistic CDF as link for logistic regression
sigmoid <- function(x) 1/(1 + exp(-x))





# Compute sum of squares 
SSQ <- function(x) x^2 %>% sum




makePerformance <- function(){
  result <- list(bias=list(fix.p=NA, high.dim=NA),
                 MSE=list(fix.p=NA, high.dim=NA))
  return(result)
}
##Testing:
# makePerformance()




# The norm of the parallelization in Ridge 
biasNormRidge_Fixp <- function(lambda, p, N, m, beta.norm, ...){
  beta.norm * m/N * lambda/(1+lambda) * (p+1)/(1+lambda)
}
## Testing:
# biasNormRidge_Fixp(lambda = 2, p = 5e3, N = 5e4, m = 1e1, beta.norm = 1)
# biasNormRidge_Fixp(lambda = 0, p = 5e3, N = 5e4, m = 1e1, beta.norm = 1)


biasMeanRidge_Fixp <- function(lambda, p, N, m, beta, ...){
  if(is.list(beta)) beta <- unlist(beta)
  beta.mean <- mean(beta)
  - beta.mean * m/N * lambda/(1+lambda) * (p+1)/(1+lambda) 
}
## Testing:


biasSingleCoordinateRidge_Fixp <- function(lambda, p, N, m, beta, coordinate, ...){
  if(is.list(beta)) beta <- unlist(beta)
  - beta[coordinate] * m/N * lambda/(1+lambda)^3 * (p+1) 
}
## Testing:



# The norm of the parallelization in Ridge 
biasNormRidge_HighDim <- function(lambda, p, N, m, beta.norm, ...){
  # TODO
}
## Testing:
# biasRidge_HighDim(lambda = 2, p = 50, N = 5e4, m = 1e2)


# The norm of the parallelization in OLS 
biasOLS_Fixp <- function(...){
  0
}
## Testing:
# biasOLSFixp(lambda = 2, p = 50, N = 5e4, m = 1e2)

biasOLS_HighDim <- function(...){
  0
}
## Testing:
# biasOLS_HighDim(lambda = 2, p = 50, N = 5e4, m = 1e2)





# Compute statistics of errors in each replication
errorFun <- function(errors){
  # Get squared norm of error
  MSEs <- lapply(errors, SSQ)
  # Compute the ratio of squared norms
  ratio <- with(MSEs, averaged/centralized)
  # Returns error summary and raw error
  result <- c(list(ratio=ratio), 
              MSE=MSEs )
}
## Testing:
# errorFun(.errors)










# Get configuration and return MSE and ratio of each replication
replicateMSE <- function(configuration){
  MSEs <- replicate(configuration$replications,{
    # get the estimation errors of centralized and parallelized
    errors <- getErrors(configuration) # get raw errors
    # Return raw errors with statistic
    list(errorFun(errors), errors=errors)
  })
  return(MSEs)
}
## Testing:
# makeTest()
# .configurations <- makeConfiguration(reps = .reps, m = .m, p = .p, n = .n, lambda = 2, model = .model, link = identity, sigma = 1, beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, data.maker = makeRegressionData, name='ols')
# .configuration <- .configurations[1,,drop=FALSE]
# apply(.configuration, 1, replicateMSE)






# A function used by frameMSEs and frameMSEs_fixKappa
cleanMSEs <- function(x) {
  x[!rownames(x)=='errors',] %>% 
    do.call(cbind,.) %>%
    apply(2,unlist)
}



# Optimize m, fixed-p, fixed-N (fixed error):
choose_m_fixp_fixN <- function(eps, p, N, sigma.sq){
  (eps/p - sigma.sq*p/N)*(N^2 / ((1+p)*p*sigma.sq))
}
## Testing:
# choose_m_fixp_fixN(eps=0.2, p=100, N=1e6, sigma.sq = 1e1) 


# Optimize m, fixed-p, fixed-n (fixed error):
choose_m_fixp_fixn <- function(eps, p, n, sigma.sq){
  p/eps * sigma.sq * p * (1/n+(1+p)/n^2)
}
## Testing:
# choose_m_fixp_fixn(eps=0.2, p=100, n=1e4, sigma.sq = 1e1) 

# Optimize m, fixed-p, fixed-N (proportional error):
choose_m_fixp_fixN_prop <- function(eps, p, N, sigma.sq){
  1+ eps + eps * N/(1+p)
}
## Testing:
# choose_m_fixp_fixN_prop(eps=0.1, p=1e2, N=1e6, sigma.sq = 1e1) 
# choose_m_fixp_fixN_prop(eps=0.1, p=1e3, N=1e6, sigma.sq = 1e1) 


# Optimize m, fixed-p, fixed-n (proportional error):
choose_m_fixp_fixn_prop <- function(eps, p, n, sigma.sq){
  (1+eps)*(1+p)/(p+1-eps*n)
}
## Testing:
# choose_m_fixp_fixn_prop(eps=0.01, p=1e2, n=1e4, sigma.sq = 1e1) 
# choose_m_fixp_fixn_prop(eps=0.01, p=1e3, n=1e4, sigma.sq = 1e1) 

