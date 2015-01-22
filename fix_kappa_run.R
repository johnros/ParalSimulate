library(InformationAndInference)
.m <- c(1e1, 2e1, 4e1, 1e2)
.kappa <- 0.2
.n <- seq(4e1, 1e3, length.out=6)
.sigma <- 1e1
.ylim <- c(1,4)
file.ending <- 2
file.name.00 <- paste('RData/MSEs.00.',file.ending,'.RData', sep='')
file.name.10 <- paste('RData/MSEs.10.',file.ending,'.RData', sep='')
file.name.30 <- paste('RData/MSEs.30.',file.ending,'.RData', sep='')
file.name.40 <- paste('RData/MSEs.40.',file.ending,'.RData', sep='')


## OLS
configurations.00 <- makeConfiguration_fixKappa(
  reps = 5e1, 
  m = .m, 
  n = .n , 
  kappa = .kappa, 
  lambda = NA, 
  sigma = .sigma, 
  model = my.ols, link = identity, beta.maker = makeBetasDeterministic, 
  beta.star.maker = BetaStarIdentity, data.maker = makeRegressionData,
  truth.fun=truthOLS) 
## Ridge
configurations.10 <- makeConfiguration_fixKappa(
  reps = 5e1,
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  lambda=2,
  model = my.ridge, link = identity, sigma = .sigma, 
  beta.maker = makeBetasDeterministic, beta.star.maker = BetaStarRidge,
  data.maker=makeRegressionData) 
## Non Linear
configurations.30 <- makeConfiguration_fixKappa(
  reps = 5e1, 
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  lambda = NA, 
  model = my.log.link, link = exp, sigma = 1, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker=makeRegressionData) 
## Logistic Regression
configurations.40<- makeConfiguration_fixKappa(
  reps = 1e2, 
  m = .m, 
  kappa = .kappa, 
  n = .n, 
  model = my.logistic, link = sigmoid, sigma = .sigma, 
  beta.maker = makeBetasRandom, beta.star.maker = BetaStarIdentity, 
  data.maker = makeClassificationData) 


configurations <- rbind(
  configurations.00,
  configurations.10,
  configurations.30,
  configurations.40)


cl <- makeCluster(30)
clusterEvalQ(cl, library(InformationAndInference))
MSEs <- parApply(cl, configurations, 1, replicateMSE)
stopCluster(cl)

save(MSEs, configurations, file='RData/MSEs_fix_kappa.1.RData')
############################################

# TODO: How to analyze the data simulated togeter?




MSEs.00 <- parApply(cl, configurations.00, 1, replicateMSE)
MSEs.framed.00 <- frameMSEs_fixKappa(MSEs.00, configurations.00)






```{r Ridge plot, eval=FALSE, echo=FALSE}
MSEs.framed.10 <- frameMSEs_fixKappa(MSEs.10, configurations.10)
plotMSEs_fixKappa(MSEs.framed.10, 'Ridge', legend.position='right', y.lim=c(1,2))
```


```{r Ridge saving}
save(MSEs.10, configurations.10, file='RData/MSEs.10.1.RData')
```






```{r nonlinear plot, eval=FALSE, echo=FALSE}
# load(file='RData/MSEs.30.RData')
MSEs.framed.30 <- frameMSEs_fixKappa(MSEs.30, configurations.30)
MSEs.framed.30 %>% select(n, average, p, m, kappa)
plotMSEs_fixKappa(MSEs.framed.30, 'Non-Linear Least Squares', legend.position='right', y.lim=.ylim )
```

```{r nonlinear save}
save(MSEs.30, configurations.30, file='RData/MSEs.30.1.RData')
```



```{r logistic plot, eval=FALSE, echo=FALSE}
MSEs.framed.40 <- frameMSEs_fixKappa(MSEs.40, configurations.40)
plot.40 <- plotMSEs_fixKappa(MSEs.framed.40, 'Logistic', legend.position='right', y.lim=.ylim)
plot.40
```

# Saving:
save(MSEs.00, configurations.00, file=file.name.00)
save(MSEs.10, configurations.10, file=file.name.10)
save(MSEs.30, configurations.30, file=file.name.30)
save(MSEs.40, configurations.40, file=file.name.40)


# Plotting:
plot.00 <- plotMSEs_fixKappa(MSEs.framed.00, '(a) OLS', legend.position='none', y.lim=.ylim)
plot.10 <- plotMSEs_fixKappa(MSEs.framed.10, '(b) Ridge', legend.position='none', y.lim=.ylim)
plot.30 <- plotMSEs_fixKappa(MSEs.framed.30, '(c) Non-Linear Least Squares', legend.position='none', y.lim=.ylim )
plot.40 <- plotMSEs_fixKappa(MSEs.framed.40, '(d) Logistic', legend.position='none', y.lim=.ylim)

directory <- "~/workspace/ParalSimulate/output/"
the.file <-paste(directory, "highdim_asymptotics_1.pdf", sep='')
pdf(file=the.file, onefile = FALSE, paper = "special", width=8, height=8)
grid.arrange(plot.00, plot.10, plot.30, plot.40, ncol=2)
dev.off()


