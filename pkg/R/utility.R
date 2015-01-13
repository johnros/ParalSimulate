# Make fitting and coef extracting function
my.ols <-  list(fitter=function(y, x,...) lm(y~x-1),
                coefs= function(fit) coef(fit))

my.ridge <-  list(fitter=function(y, x,...) lm.ridge(y~x-1,...),
                coefs= function(fit) coef(fit))

my.log.link <- list(fitter=function(y, x,beta.star,...) {
  .start <- beta.star
  .control <- glm.control(epsilon=1e-4, maxit = 1e2 )
  glm(formula = y~x-1, family=gaussian(link='log'),  start=.start,
      control = .control)
  },
  coefs= function(fit) coef(fit))

my.logistic <- list(fitter=function(y, x,...) glm(y~x-1, family = binomial,...),
                    coefs= function(fit) coef(fit))


my.huber <- list(fitter=my.huber <- function(y, x,...) rlm(y~x-1,...), 
                 coefs= function(fit) coef(fit))

my.absolute <- list(fitter=function(y, x,...) rq(y~x-1, method="fn",...),
                    coefs=function(fit) coef(fit))



# Convert a list of lists to a matrix.
extractor <- function(x) apply(x, 1, unlist)

sigmoid <- function(x) 1/(1 + exp(-x))

