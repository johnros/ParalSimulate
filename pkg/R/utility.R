# Make fitting and coef extracting function
my.ols <-  list(fitter=function(y, x,...) lm(y~x,...),
                coefs= function(x) coef(x)[-1])
my.huber <- list(fitter=my.huber <- function(y,x,...) rlm(y~x,...), 
                 coefs= function(x) coef(x)[-1])
my.absolute <- list(fitter=function(y,x,...) rq(y~x, method="fn",...),
                    coefs=function(x) coef(x)[-1])
my.logistic <- list(fitter=function(x,y,...) glm(y~x, family = binomial,...),
                    coefs= function(fit) coef(fit)[-1] )




# Generate true parameters
makeBetas <- function(p){
  betas <- rnorm(p)
  betas <- betas / sqrt(betas %*% betas)
  return(betas)
}
## Testing:
#makeBetas(100)



makeTest <- function(reps=1e1, 
                     m=c(1e1,1e2), 
                     p=5e1, 
                     n=c(1e2,1e3), 
                     kappa=0.1, 
                     model=my.ols){
  .reps <<- reps
  .m <<- m
  .p <<- p
  .n <<- n
  .kappa <<- kappa
  .model <<- model
}
## Testing
# makeTest()
# .p  





# Return a frame with all simulation configurations
makeConfiguration <- function(reps, m, p, n, kappa, model, parameters){
  
  configurations.frame <- expand.grid(replications=reps, 
                                      m=m, 
                                      p=p, 
                                      n=n, 
                                      kappa=kappa, 
                                      model=list(model), 
                                      params=c(makeBetas))
  return(configurations.frame)
}
## Testing:
# makeTest()
# .configuration <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params)
# .configuration %>% dim
# .configuration$model 


# Which configurations not to run
excludeConfigurations <- function(configurations){
  result <- configurations %>% 
    filter(p/n < kappa)
  return(result)
}
## Testing:
# makeTest(p=1e1)
# .configuration <- makeConfiguration(.reps, .m, .p, .n, .kappa, .model, .params)
# excludeConfigurations(.configuration) 



