# Repeat estimation for a given configuration
replicateMSE_fixKappa <- function(configuration){
  MSEs <- replicate(configuration$replications,{
    errors <- getErrors_fixKAppa(configuration)
    errorFun(errors)
  })
  return(MSEs)
}
## Testing:
# configuration <- .configurations[1,]
# replicateMSE_fixKappa(configuration)


##TODO: write getErrors_fixKappa