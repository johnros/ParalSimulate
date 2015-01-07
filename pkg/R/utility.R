# Make fitting and coef extracting function
my.ols <-  function(y, x,...) lm(y~x,...)
coefs.ols <- function(x) coef(x)[-1]

my.huber <- function(y,x,...) rlm(y~x,...)
coefs.huber <- function(x) coef(x)[-1]

my.absolute <- function(y,x,...) rq(y~x, method="fn",...)
coefs.absolute <- function(x) coef(x)[-1]

my.logistic <- function(x,y,...) glm(y~x, family = binomial,...)
coefs.logistic <- function(fit) coef(fit)[-1] 




