popGrowthModel <- function(caribouModels = sim$caribouModels, # <==== Caribou models should be just the simple model
                           disturbances = sim$disturbances,
                           currentPop = sim$currentPop,
                           currentTime = time(sim),
                           startTime = start(sim),
                           adultFemaleSurv = sim$adultFemaleSurv,
                           popModel = P(sim)$popModel,
                           listSACaribou = sim$listSACaribou){

  message("Growing some Caribous...")
  
  yearPrediction <- lapply(X = disturbances, FUN = function(yr){
    shpPrediction <- lapply(X = yr, FUN = function(shp){
      polyPrediction <- lapply(X = shp, FUN = function(polyg){
        predParams <- lapply(X = caribouModels, FUN = function(model){
          attach(polyg)
          recr <- eval(parse(text = model))
          detach(polyg)
          SadF <- adultFemaleSurv # ECCC 2012 set this to 0.85, and we do not have any LPU specific values for the NWT. Therefore, I am making this same assumption
          # For each model, extract the current currentPop if class(currentPop) == "list"
          if (class(currentPop) == "list"){
            currentPop <- currentPop[[model]]
          }
          param <- do.call(what = popModel, args = list(N = currentPop,
                                                        SadF = SadF,recr = recr))
          dt <- data.table::data.table(Rec = mean(recr),
                                       minRec = min(recr),
                                       maxRec = max(recr),
                                       sdRec = sd(recr),
                                       adultFemaleSurv = SadF,
                                       modelParam = param,
                                       populationModel = popModel,
                                       caribouModel = model)
          return(list(Pred = caribouModels, results = dt))
        })
        names(predParams) <- names(caribouModels)
        return(predParams)
      })
      names(polyPrediction) <- names(shp)
      return(polyPrediction)
    })
    names(shpPrediction) <- names(yr)
    return(shpPrediction)
  })
  return(yearPrediction[[paste0("Year", currentTime)]])
}

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
