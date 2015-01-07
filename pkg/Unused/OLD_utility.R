# Remove all functions from the workspace:
# function.ind <- unlist(
#   lapply(
#     lapply(
#       mget(ls()),
#       class)
#     , function(x) x[[1]]=='function')) 
# rm(list=ls()[function.ind])


stderr <- function(x) {
  sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
}

my.ols <-  function(y, x,...) lm(y~x,...)
coefs.ols <- function(x) coef(x)[-1]

my.huber <- function(y,x,...) rlm(y~x,...)
coefs.huber <- function(x) coef(x)[-1]

my.absolute <- function(y,x,...) rq(y~x, method="fn",...)
coefs.absolute <- function(x) coef(x)[-1]

my.logistic <- function(x,y,...) glm(y~x, family = binomial,...)
coefs.logistic <- function(fit) coef(fit)[-1] 

truthNULL <- function(kappa,m) 100
truthOLS <- function(kappa,m) {
  kappa <- as.numeric(as.character(kappa))
  m <- as.numeric(as.character(m))
  (1 - kappa/m) / (1-kappa)  
}
truthAbsolute <- function(kappa,m) 1 + 0.9 * kappa * (1-1/as.numeric(as.character(m)))



# Take betas and make sample:
## betas are assumed to have unit variance.
makeRegressionData <- function(p, N, betas, link, sigma){
  X <-  matrix(rnorm(p*N), nrow=N, ncol=p)
  linear.effect <- X %*% betas
  y <- link(linear.effect) + rnorm(N, 0, sd=sigma)
  result <- list(y=y, X=X)
  return(result)
}
## Testing:
# p <- 100
# N <- 10000
# betas <- rt(100, df = 3, ncp=1  )
# makeRegressionData(p,N,betas, identity, 1)
# rm(p,N,betas)






analyzeParallel <- function(y, X, m, FITTER, COEFS){
  N <- length(y)
  p <- ncol(X)
  
  ## Centralized Solution:
  the.fit <- FITTER(y=y,x=X)
  center.coefs <- COEFS(the.fit)
  
  ## Parallelized solution:
  machine.ind <- sample(1:m, size = N, replace = TRUE)
  .min <- machine.ind %>% table %>% min
  if(.min < p) stop('Not enough observations per machine')
  machine.wise <-  matrix(NA, ncol = m, nrow = ncol(X))
  for(i in seq_len(m)){
    .the.fit <- FITTER(y=y[machine.ind==i], x=X[machine.ind==i,])
    .coefs<- COEFS(.the.fit)
    machine.wise[,i] <- .coefs 
  }
  averaged.coefs <- rowMeans(machine.wise, na.rm=TRUE)
  
  result <- list(averaged=averaged.coefs, 
                 centralized=center.coefs)
  return(result)
}
## Testing:
.m <- 1e1
.N <- 1e4
.p <- 1e2
.betas <- rnorm(.p, 0, 1/sqrt(.p))
.link <- identity()
.test <- makeRegressionData(.p, .N ,.betas) 
.y <- .test$y
.X <- .test$X
.FITTER <-  function(y, x,...) lm(y~x,...)
.COEFS <- function(x) coef(x)[-1]
.FITTER <- function(y,x,...) rq(y~x, method="fn",...)
.COEFS <- function(x) coef(x)[-1]
analyzeParallel(.y, .X, .m, .FITTER, .COEFS)

.test <- makeClassificationData(p, N ,betas) 
y <- .test$y
X <- .test$X
FITTER <- function(x,y,...) glm(y~x, family = binomial,...)
COEFS <- function(fit) coef(fit)[-1] 
analyzeParallel(y,X,m,FITTER,COEFS)








# Generate data and compare analysis methods:
## Takes the number of parameters, kappa, and m
## Computes N
compareParallelizeRegression <- function(
  p, kappa, m, FITTER, COEFS, plot.ind=TRUE){
  
  n <- floor(p/kappa)
  N <- n * m
  
  betas <- rnorm(p)
  betas <- betas / sqrt(betas %*% betas)
  
  .data <- makeRegressionData(p = p, N = N, betas = betas)
  .estimates <- analyzeParallel(.data$y, .data$X, m, FITTER, COEFS)
  
  if(plot.ind) {
    plot(averaged~centralized, data=.estimates)
    abline(0, 1, col='grey')
    title(paste('m=',m,'p=',p, 'n=',N/m))
  }
  
  errorFun <- function(estim) (estim-betas)^2 %>% sum
  MSEs <- lapply(.estimates, errorFun)
  invisible(MSEs)
}
## Testing:
# m <- 1e1
# p <- 1e2
# FITTER <-  function(y, x,...) lm(y~x,...)
# COEFS <- function(x) coef(x)[-1]
# plot.ind <- TRUE
# kappa <- 0.5
# .test <- compareParallelizeRegression(p, kappa, m, FITTER, COEFS)
# .test



# Compute the ratio of MSEs:
getMSERegression <- function(replications, kappa, p, m, FITTER, COEFS){
  replicate(replications,{    
    estimates <- compareParallelizeRegression(
      p, kappa, m, FITTER, COEFS, plot=FALSE)
    estimates$averaged / estimates$centralized
  })
}
## Testing:
# replications <- 1e1
# kappa <- 0.2
# p <- 1e2
# m <- 1e1
# FITTER <- function(y, x,...) lm(y~x,...)
# COEFS <- function(x) coef(x)[-1]
# getMSE(replications, kappa, p, m, FITTER, COEFS)



getMSEClassification <- function(replications, kappa, p, m, FITTER, COEFS){
  replicate(replications,{    
    estimates <- compareParallelizeClassification(
      p, kappa, m, FITTER, COEFS, plot=FALSE)
    estimates$averaged / estimates$centralized
  })
}
## Testing:
# FITTER <- function(x,y,...) glm(y~x, family = binomial,...)
# COEFS <- function(fit) coef(fit)[-1] 
# getMSEClassification(replications, kappa, p, m, FITTER, COEFS)



# Make data for classification problems:
makeClassificationData <- function(p, N, betas, link){
  X <-  matrix(rnorm(p*N), nrow=N, ncol=p)
  linear.effect <- X %*% betas
  probs <- link(linear.effect)
  y <- rbinom(N, 1, probs) %>% factor
  result <- list(y=y,X=X)
  return(result)
}
## Testing:
# p <- 1e2
# N <- 1e4
# betas <- rnorm(p, 0, 1/sqrt(p))
# makeClassificationData(p, N, betas)



# Compare centralized and parallelized classification performance:
# Hold p,kappa,m fixed and adapt n and N.
compareParallelizeClassification <- function(
  p, kappa, m, FITTER, COEFS, plot.ind=TRUE){
  
  n <- floor(p/kappa)
  N <- n * m
  
  betas <- rnorm(p)
  betas <- betas / sqrt(betas %*% betas)
  
  .data <- makeClassificationData(p, N, betas)
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
# m <- 1e1
# p <- 1e2
# kappa <- 0.2
# COEFS <- function(fit) c(fit$W) 
# FITTER <- function(x,y,...)LiblineaR(data=x, labels = y, type=1, bias = FALSE,...)
# plot.ind <- TRUE
# .test <- compareParallelizeClassification(p, kappa, m, FITTER, COEFS)
# .test




frameMSEs <- function(MSEs, p, kappas, replications){
  MSEs.framed <- data.frame(
    MSEs=unlist(MSEs), 
    kappas=rep(kappas, each=replications))
  
  MSEs.framed$ns <- with(MSEs.framed, p/kappas)
  
  return(MSEs.framed)
}
## Testing:
# frameMSEs(MSEs.0,p, kappas,replications)


# Plot results of fixed p regime:
plotMSE <- function(MSEs, p, kappas, 
                    replications,
                    the.title, 
                    y.lab= '', 
                    y.lim=c(0,2)){
  
  MSEs.framed <- frameMSEs(MSEs, p, kappas, replications)
    
  plot.1 <- ggplot(data = MSEs.framed, aes(x =ns, y=MSEs))+
    labs(title = the.title)+
    ylab(y.lab)+
    ylim(y.lim)+
    xlab(expression(n))+
    stat_summary(fun.data=mean_cl_normal, 
                 colour="red", 
                 geom='errorbar', 
                 size = 1)+
    stat_summary(fun.data=mean_cl_normal, 
                 colour="black", 
                 geom='line', 
                 size = 0.5)+
    scale_x_continuous(trans=log10_trans())+
    theme_bw()+
    theme(text = element_text(size=20)) 
  return(plot.1)  
}
## Testing
# plotMSE(MSEs.0, p, kappas, replications , "test")
