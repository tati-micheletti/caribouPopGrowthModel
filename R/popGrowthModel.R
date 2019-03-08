popGrowthModel <- function(caribouModels = sim$caribouModels,
                           DH_Tot = sim$DH_Tot,
                           currentPop = sim$currentPop,
                           currentTime = time(sim),
                           startTime = start(sim),
                           adultFemaleSurv = sim$adultFemaleSurv){
  
  message("Growing some Caribous...")
  cummDist <- data.frame(DH_Tot = DH_Tot[[paste0("Year", currentTime)]])
  predParams <- lapply(X = names(caribouModels), FUN = function(model){
    mod <- predict(caribouModels[[model]], newdata = cummDist, se = TRUE)
    recr <- mod$fit/100 # average proportion across 4 herds from 2008 data.
    SadF <- adultFemaleSurv # ECCC 2012 set this to 0.85, and we do not have any LPU specific values for the NWT. Therefore, I am making this same assumption

# MODEL OPTION 1
# Simple pop model - non-stochastic, and non- density dependent
    annualGrowth <- function(N, SadF, recr) {
      newN <- N * SadF
      newN <- newN + newN * (recr / 2) # only 1/2 the calves will be female
      round(newN, 0)
      return(newN)
    }

# MODEL OPTION 2    
# more complicated population model - non-stochastic, but logistic (i.e. density-dependent)
    annualGrowthL <- function(N, SadF, recr){
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

####################################################################################################################    
# MODEL OPTION 3 
# Simple lambda model (i.e. realized population growth rate)
    annualLambda <- function(SadF, recr){
      mortF <- (1-SadF)
      mortR <- (1-recr)
      newL <- (1-mortF)/(1-mortR) # basic McLaughlin et al. 2003 lambda model, also used in Sorrensen et al. 2006, and Hervieux et al. 2013
      round(newL, 2)
      return(newL)
    }
    
#######################################################################################################################
    
# For each model, extract the current currentPop if class(currentPop) == "list"
 if (class(currentPop) == "list"){
   currentPop <- currentPop[[model]]
 }
    
    newPop <- annualGrowth(N = currentPop,
                           SadF = SadF, 
                           recr = recr)
    
    newLambda <- annualLambda(SadF = SadF, 
                           recr = recr)
    
    return(list(Pred = mod, Rec = recr, adultFemaleSurv = SadF, currentPopUpdated = newPop, lambda = newLambda))
  })
  names(predParams) <- names(caribouModels)
  return(predParams)
}



######################################################################################################################################
######################################## To be completed in future ###################################################################
######################################################################################################################################

# MODEL OPTION 4 
#more complicated population model - ## CURRENTLY LOGISTIC, BUT NOT YET STOCHASTIC ####
# based on pers coms with caribou professionals
Psurviv <- recr/2 #probability of survival from juvenile to adult, for females only
# from caribou/BRAT calculations (winder et al. in review):
# we know from maternal penning that 80% of calves survive to day 1, that adult femape conception rates are at 90%, and
# that 90% of calves survive from day 1 to 30
pregR <- 0.9 # DATA/ASSUMPTION (S. McNay pers com)
surv1stDay <- 0.8 # DATA/ASSUMPTION (S. McNay pers com)
surv30thDay <- 0.9 # DATA/ASSUMPTION (S. McNay pers com)
Initsurviv <- (SadF*pregR*surv1stDay*surv30thDay)  # proportion of adult females that produce a calf
FInitiSurviv <- Initsurviv*0.5 # proportion of these calves that are female
FSurviv <- FInitiSurviv - (recr/2) # prorportion of calves that survive until survey, and contribute to the cow:calf ratio
#########################################
annualGrowthS<- function(N, FSurviv) {
  # set the carrying capacity of the population
  K <- 10000 # we know there are at most 7000 boreal caribou in NWT, but historically there were more
  newN <- N + (K - N*(N*FSurviv)/K)
  N <- newN
  return(newN)
}

Nyears <- 100
N <- integer(Nyears)
N[1] <- 6731
for (yr in 1:Nyears) {
  N[yr +1] <- annualGrowthS(N[yr], FSurviv)
}
print(N)
plot(N)

### START HERE FRANCES ---- ###

# MODEL OPTION 5
# LOGISTIC AND STOCHASTIC
# I need to know the range of recr and SadF values in order to make this stochastic
# boot strap this range to generate the CV for each value
# then apply the CV to the population growth



# MODEL OPTION 6
# ### Population growth event - Glenn's Model
# Demography <- function(popnData, repPopnScnParams.df, # switchout "popnData" for "OutputTable"
#                        popnDataTemplate, currYear, startTime)  {
#    prevYear <- as.character(as.numeric(currYear)-1);
#   #popnData[[currYear]]$flgDDAdj <- FALSE
#   #popnData[[currYear]]$flgRmAdj <- FALSE
# 
#   # Step 1: calculate unadjusted currPopn with density dependence acting on recruitment but before applying constraint:  rmax)
#   if(as.numeric(currYear) > startTime) {
#     prevPopn <- popnData[[prevYear]]$prjNt;
#     # popnData[[currYear]] <- popnDataTemplate
# 
#     # calculate prjRect and prjSadFt for this year
#     # popnData[[currYear]]$prjNt <- repPopnScnParams.df$N0; # initial projected population size
#     #    browser()
#   } else {
#     #    prevPopn <- popnData[[currYear]]$prjNt;
#     prevPopn <- popnData[[currYear]]$prjNt;
#     prevYear <- currYear
#   }
# 
#    # projected recruitment (unadjusted) and SadF
#    # Aug 13: should now be pre-calculated
#    #  popnData[[currYear]]$prjRect <- repPopnScnParams.df$rRec
#    # projected survival of adult females
#    #  popnData[[currYear]]$prjSadFt <- repPopnScnParams.df$rSadF
# 
#    # only do the remaining calculations if the previous year's population > 0 (TODO?: or not NA)
#    #  prevPopnExtant <- !is.na(prevPopn) & prevPopn > 0
#    prevPopnExtant <- prevPopn > 0
#    if(anyNA(prevPopnExtant)) {
#      prevPopnExtant[is.na(prevPopnExtant)] <- FALSE
#    }
# 
#    if(any(!prevPopnExtant )) {
#      popnData[[currYear]][!prevPopnExtant,-(1:4)] <- 0
#      #    popnData[[currYear]][!prevPopnExtant,-(1:4)] <- NA_integer_
#    }
# 
#    # All calculated values become 0, not column 1 which is time index
#    if (any(prevPopn > 0)) {
#      n_deaths <- round(prevPopn[prevPopnExtant] * (1 - popnData[[currYear]]$prjSadFt[prevPopnExtant]), digits=0);
#      surviving_adFemales <- prevPopn[prevPopnExtant] - n_deaths;
# 
#      rK <- repPopnScnParams.df$K[prevPopnExtant] * prevPopn[prevPopnExtant];
