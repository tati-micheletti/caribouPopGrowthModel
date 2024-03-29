# Simple pop model - non-stochastic, and non- density dependent
annualGrowth <- function(N, SadF, recr, ...) {
  newN <- N * SadF
  newN <- newN + newN * (recr / 2) # only 1/2 the calves will be female
  round(newN, 0)
}

# more complicated population model - non-stochastic, but logistic (i.e. density-dependent)
annualGrowthLogistic <- function(N, SadF, recr){
  # set intrinsic capacity for increase
  r <- 0.10 # set to 10%. ASSUMPTION - this can change - currently obtained from Sutherland et al. (in prep) demonstrating that
  # recruitment needs to be > 0.1 for annual population growth
  newN<- r * N
  # set the carrying capacity of the population
  K <- 10000 # we know there are at most 7000 boreal caribou in NWT, but historically there were more.
  newN <- newN * (K - N)/K
  round(newN, 0)
  return(newN)
  plot(newN)
}

# Simple lambda model (i.e. realized population growth rate)
annualLambda <- function(SadF, recr, ...){
  if (any(recr > 1, recr < 0)){
    stop("Recruitment needs to be provided per capita (0-1)")
  }
  if (any(SadF > 1, SadF < 0)){
    stop("Female survival needs to be provided per capita (0-1)")
  }
  mortF <- (1-SadF)*100
  newL <- (100-mortF)/(100-(100*recr/2))
  # basic McLaughlin et al. 2003 lambda model, also used in Sorrensen et al. 2006
  return(newL)
}

# WE NEED TO REVIEW THE CODE BELOW WHEN WE DECIDE TO IMPLEMENT 
# POPULATION SIZE CALCULATION
# timestepLambda <- function(SadF, recr, ts = 10, ...){ 
#   return(annualLambda(SadF, recr, ...) ^ ts)
# }

# more complicated population model - non-stochastic, but logistic (i.e. density-dependent)
annualGrowthLogisticLambda <- function(N, SadF, recr, ...){
  
  # NOT IMPLEMENTED!
  
  # set intrinsic capacity for increase
  r <- 0.10 # set to 10%. ASSUMPTION - this can change - currently obtained from Sutherland et al. (in prep) demonstrating that
  # recruitment needs to be > 0.1 for annual population growth
  newN<- r * N
  # set the carrying capacity of the population
  K <- 10000 # we know there are at most 7000 boreal caribou in NWT, but historically there were more.
  newN <- newN * (K - N)/K
  round(newN, 0)
  return(newN)
}

