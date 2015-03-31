# Testing ridge.lm:
BetaStarRidge <- function(beta, lambda) beta/(1+lambda)

..p <- 100L
..n <- 1e4
..x <- rnorm(..p* ..n)  %>% matrix(ncol=..p, nrow=..n)
var(..x) %>% round(,-1)

## If arbitrary y allowed:
..beta <- seq(1:..p)/sqrt(sum((1:..p)^2))
..sigma <- 1e1
..y <- ..x %*% ..beta + rnorm(..n, sd = ..sigma)
var(..y)


## If y with unit variance required (mainly for glmnet):
..beta <- seq(1:..p)/sqrt(sum((1:..p)^2))/sqrt(2)
..beta^2 %>% sum 
..sigma <- 1e1
..y <- ..x %*% ..beta + rnorm(..n, sd = 1/sqrt(2))
var(..y)  


## Using MASS
library(MASS)
..lambda <- 1e1
..beta.star <- BetaStarRidge(beta = ..beta, lambda = ..lambda)
..ridge.1 <- lm.ridge(..y~..x-1, lambda=..lambda)
..ridge.1$Inter
..coef.1 <- coef(..ridge.1)
plot(..beta~..coef.1);abline(0,1)
plot(..beta.star~..coef.1);abline(0,1)

## Ridge
library(ridge)
..lambda <- 1e1
..ridge.2 <- linearRidge(..y~..x-1, lambda=..lambda)
..coef.2 <- coef(..ridge.2)
..beta.star <- BetaStarRidge(beta = ..beta, lambda = ..lambda)
plot(..beta~..coef.2);abline(0,1)
plot(..beta.star~..coef.2);abline(0,1)

# # Using elasticnet
# library(elasticnet)
# ..lambda <- 1e1
# ..beta.star <- BetaStarRidge(beta = ..beta, lambda = ..lambda)
# ..ridge.3 <- enet(x =..x, y =..y, lambda=..lambda, intercept = FALSE)
# ..coef.3 <- coef(..ridge.3)
# plot(..beta~..coef.3);abline(0,1)
# plot(..beta.star~..coef.3);abline(0,1)

# Using glmnet
library(glmnet)
..lambda <- 1e1
..beta.star <- BetaStarRidge(beta = ..beta, lambda = ..lambda)
..ridge.4 <- glmnet(x =..x, y =..y, family = 'gaussian', alpha = 0, lambda=..lambda, intercept = FALSE)
..coef.4 <- coef(..ridge.4)@x
plot(..beta~..coef.4);abline(0,1)
plot(..beta.star~..coef.4);abline(0,1)

# Using genridge
library(genridge)
..lambda <- 1e1
..beta.star <- BetaStarRidge(beta = ..beta, lambda = ..lambda)
..ridge.5 <- ridge(..y~..x-1, lambda=..lambda)
..coef.5 <- coef(..ridge.5) %>% drop
plot(..beta~..coef.5);abline(0,1)
plot(..beta.star~..coef.5);abline(0,1)

## Using ElemStatLearn
library(ElemStatLearn)
..lambda <- 1e1
..beta.star <- BetaStarRidge(beta = ..beta, lambda = ..lambda)
..ridge.6 <- simple.ridge(y=..y,x=..x, lambda=..lambda)
..coef.6 <- ..ridge.6$beta %>% drop
plot(..beta~..coef.6);abline(0,1)
plot(..beta.star~..coef.6);abline(0,1)
