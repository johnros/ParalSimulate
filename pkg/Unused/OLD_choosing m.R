make.fix.N <- function(p, N, r1, r2){
  fix.N <- function(m){
   1+ p/N * r2/r1 *(m-1) 
  }
  return(fix.N)
}

make.fix.kappa <- function(kappa, r1, r2){
  fix.kappa <- function(m){
    1 + kappa * r2/r1 * (1-1/m)
  }  
  return(fix.kappa)
}

# a <- make.fix.kappa(0.2, 1, 1)
# b <- make.fix.N(1e2, 1e4, 1, 1)

# curve(a, 2, 20)
# curve(b, add=TRUE)
