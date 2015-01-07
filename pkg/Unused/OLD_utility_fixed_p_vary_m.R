analyzeCentralized <- function(y, X, FITTER, COEFS){
  ## Centralized Solution:
  the.fit <- FITTER(y=y,x=X)
  center.coefs <- COEFS(the.fit)
  
  result <- list(centralized=center.coefs)
  return(result)
}
## Testing:
# FITTER <-  function(y, x,...) lm(y~x,...)
# COEFS <- function(x) coef(x)[-1]
# p <- 1e2
# betas <- rnorm(p,0,1/sqrt(p))
# .test <- makeRegressionData(p, N ,betas) 
# y <- .test$y
# X <- .test$X
# analyzeCentralized(y, X, FITTER, COEFS)

  


# The same as analyzeParallel, but allowing multiple m's:
analyzeParallel_ms <- function(y, X, ms, FITTER, COEFS){
  N <- length(y)
  p <- ncol(X)
  
  ## Parallelized solution:
  averaged.coefs <- list()
  for (j in seq_along(ms)){
    m <- ms[j]
    
    for (s in 1:100){
      machine.ind <- sample(1:m, size = N, replace = TRUE)
      .min <- machine.ind %>% table %>% min
      if (.min > p*2) break()
      else if(s<100 && .min < p*2) next()
      else if(min(table(machine.ind)) < p*2) {
        browser()
        stop('Not enough observations per machine') 
      }
    }
    
    machine.wise <-  matrix(NA, ncol = m, nrow = ncol(X))
    for(i in seq_len(m)){
      .the.fit <- FITTER(y=y[machine.ind==i], x=X[machine.ind==i,, drop=FALSE])
      .coefs<- COEFS(.the.fit)
      machine.wise[,i] <- .coefs 
    }
    averaged.coefs[[m]] <- list(rowMeans(machine.wise, na.rm=TRUE))
  }
  
  result <- list(averaged=averaged.coefs)
  return(result)
}
## Testing:
.FITTER <-  function(y, x,...) lm(y~x,...)
.COEFS <- function(x) coef(x)[-1]
.ms <- c(1e1, 5e1, 1e2)
.p <- 1e2
.betas <- rnorm(p,0,1/sqrt(p))

.test <- makeRegressionData(p, N ,betas) 
.y <- .test$y
.X <- .test$X
analyzeParallel_ms(y, X, m, FITTER, COEFS)
rm(FITTER, COEFS, betas, .test, the.fit, center.coefs)


# For any number of machines:
# Compute the l_2 error of centralized and parallelized.
getMSEs <- function(ms, n, .data, betas, FITTER, COEFS, plot.ind){
  
  .estimates.list <- list()
  for(j in seq_along(ms)){
    m <- ms[j]
    N.effective <- m*n
    .estimates <- analyzeParallel_ms(y = .data$y[1:N.effective], X =                            .data$X[1:N.effective,], 
                                  m, FITTER, COEFS)  
    .estimates.list[[m]] <- .estimates
  }
  
  # Create output:
  errorFun <- function(estim) (estim-betas)^2 %>% sum
  MSEs <- list()
  for(m in ms){
    MSEs[[m]] <- .estimates.list[[m]] %>% lapply(errorFun)
  } 
  
  
  if(plot.ind) {
    p <- ncol(.data$X)
    for(m in ms){
      with(.estimates.list[[m]], plot(averaged ~ centralized))
      abline(0, 1, col='grey')
      title(paste('m=',m,'p=',p, 'n=',n))
    }
  }
  
  return(MSEs)
}
## Testing:
# .n
# .data
# FITTER <- my.ols
# COEFS <- coefs.ols
# plot.ind
# getMSEs()






# Fixed kappa, p and m:
## Generate parameters, data, and get estimate MSEs
compareParallelizeRegression_ms <- function(
  p, kappa, ms, FITTER, COEFS, 
  plot.ind=TRUE, link=identity, sigma=1){
  
  betas <- rnorm(p, 0, 1/sqrt(p))
  betas <- betas / sqrt(betas %*% betas)
  n <- floor(p/kappa)
  .data <- makeRegressionData(p = p, N = n* max(ms), betas = betas, link=link, sigma = sigma)
  
  MSEs <- getMSEs(ms, n, .data, betas, FITTER, COEFS, plot.ind)
  
  invisible(MSEs)
  }
## Testing:
# ms <- c(1e1, 2e1, 5e1)
# p <- 1e2
# kappa <- 5e-1
# FITTER <-  my.ols
# COEFS <- coefs.ols
# FITTER <-  my.absolute
# plot.ind <- TRUE
# .test <- compareParallelizeRegression_ms(p = 1e2, kappa = 5e-1, ms=c(1e1, 2e1, 5e1), FITTER = my.ols, COEFS=coefs.ols)
# .test


## Get the MSE of centralized and parallelized estimators:
# Repeatedly create parameters, data, estimate and average.
getMSERegression_ms <- function(
  replications, kappa, p, ms, FITTER, COEFS, link=identity, sigma=1){
  
  replicate(replications,{    
    
    estimates <- compareParallelizeRegression_ms(
      p, kappa, ms, FITTER, COEFS, plot=FALSE, link, sigma)
  
    result <- rep(NA,length.out = length(ms))
    names(result) <- ms
    for(m in ms){
      result[paste(m)] <- with(estimates[[m]], averaged / centralized)
    }
    return(result)
  })
}
## Testing:
# replications <- 1e1
# kappa <- 0.2
# p <- 1e2
# m <- 1e1
# FITTER <- function(y, x,...) lm(y~x,...)
# COEFS <- function(x) coef(x)[-1]
# link <- exp
# sigma <- 0
# getMSERegression_ms(replications, kappa, p, ms, FITTER, COEFS, link, sigma)



frameMSE_ms <- function(MSEs,p, kappas, replications){
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
# frameMSE_ms(MSEs_ms.0, p, kappas)



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
# MSEs <- MSEs_ms.0
# , p, kappas, 
# replications,
# the.title <- 'test'
# y.lab= ''
# y.lim=c(0,2)
# plotMSE_ms(MSEs_ms.0, p, kappas, replications , the.title)



compareParallelizeClassification_ms <- function(
  p, kappa, ms, FITTER, COEFS, plot.ind=TRUE){

  betas <- rnorm(p, 0, 1/sqrt(p))
  betas <- betas / sqrt(betas %*% betas)
  n <- floor(p/kappa)
  .data <- makeClassificationData(p, n* max(ms), betas)
  
  .estimates.list <- list()
  for(j in seq_along(ms)){
    m <- ms[j]
    N.effective <- m*n
    .estimates <- analyzeParallel(.data$y[1:N.effective], 
                                  .data$X[1:N.effective,], 
                                  m, FITTER, COEFS)  
    .estimates.list[[m]] <- .estimates
  }
  
  
  if(plot.ind) {
    for(m in ms){
      with(.estimates.list[[m]], plot(averaged ~ centralized))
      abline(0, 1, col='grey')
      title(paste('m=',m,'p=',p, 'n=',n))
    }
  }
  
  errorFun <- function(estim) (estim-betas)^2 %>% sum
  MSEs <- list()
  for(m in ms){
    MSEs[[m]] <- .estimates.list[[m]] %>% lapply(errorFun)
  }
  
  invisible(MSEs)
}
## Testing:
# m <- 1e1
# p <- 1e2
# kappa <- 0.2
# FITTER <- function(x,y,...) glm(y~x, family = binomial,...)
# COEFS <- function(fit) coef(fit)[-1] 
# plot.ind <- TRUE
# .test <- compareParallelizeClassification_ms(p, kappa, ms, FITTER, COEFS)
# .test



getMSEClassification_ms <- function(replications, kappa, p, ms, FITTER, COEFS){
  replicate(replications,{    
    estimates <- compareParallelizeClassification_ms(
      p, kappa, ms, FITTER, COEFS, plot.ind=FALSE)
    
    result <- rep(NA,length.out = length(ms))
    names(result) <- ms
    for(m in ms){
      result[paste(m)] <- with(estimates[[m]], averaged / centralized)
    }
    return(result)
  })
}
## Testing:
# replications <- 2e0
# kappa <- 0.2
# p <- 1e2
# m <- 1e1
# FITTER <- function(x,y,...) glm(y~x, family = binomial,...)
# COEFS <- function(fit) coef(fit)[-1] 
# getMSEClassification_ms(replications, kappa, p, ms, FITTER, COEFS)
