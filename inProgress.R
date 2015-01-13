## nls
.p <- 1e2
.data <- makeRegressionData(p = .p, N=.p*1e1, beta = makeBetasRandom(.p), link = exp, 1e1) 
.data2 <- do.call(data.frame, .data) 
.start <- rep(0, .p)

.vars <- paste('X', seq(.p), sep='.', collapse ='+')
nls(y~exp(.vars), data = .data2, start = .start)

example(nls)
?SSlogis

?nlm
example(nlm)
dim(.data2)
length(.start)

#glm
.p <- 1e2
.betas <- makeBetasDeterministic(.p)
.data <- makeRegressionData(p = .p, N=.p*1e1, beta =.betas , link = exp, 1e1) 
plot(.data$y, type='h')
.data2 <- do.call(data.frame, .data) 
.data2 %>% dim
glm(formula = y~.-1,family =gaussian(link='log'), data = .data2, start = .betas, 
    control = glm.control(epsilon = 1e-6, maxit = 1e3, trace = FALSE ))


# Starting with OLS estimates:
.p <- 1e2
.betas <- makeBetasDeterministic(.p)
.data <- makeRegressionData(p = .p, N=.p*1e2, beta =.betas , link = exp, sigma = 1e2) 
.data2 <- do.call(data.frame, .data) 
.start <- coef(lm(y~.-1, data=.data2))
glm.1 <- glm(formula = y~.-1,family =gaussian(link='log'), data = .data2,  start=.start,
    control = glm.control(epsilon = 1e-4, maxit = 1e3, trace = FALSE ))
plot(coef(glm.1)~ .betas); abline(0,1)


