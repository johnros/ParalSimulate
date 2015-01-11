?lm.ridge
example(lm.ridge)

lm.1 <- lm.ridge(y ~ .-1, longley)
coef(lm.1)
