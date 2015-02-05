# Testing ridge.lm:
..p <- 100L
..n <- 1e4
..x <- rexp(..p* ..n)  %>% matrix(ncol=..p, nrow=..n)
..beta <- rnorm(..p,mean = 2)
..sigma <- 1e1
..y <- ..x %*% ..beta + rnorm(..n, sd = ..sigma)

## Using MASS
..lambda <- 1e-1
..beta.star <- BetaStarRidge(beta = ..beta, lambda = ..lambda)
..ridge.1 <- lm.ridge(..y~..x-1, lambda=..lambda)
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

