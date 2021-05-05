makeDTforPopGrowth <- function(populationGrowthTable, 
                               responseVariable, # recruitment or femaleSurvival
                               modelVersion, 
                               # ECCC or Johnson; can be null if responseVariable is femaleSurvival; 
                               # ECCC doesn't have this model
                               modelNumber, # M1-M12
                               Type = "National" # all models implemented in table are national;
                               # left this here in case someone wants to pass another model -- i.e. 
                               # modify the table
                               ){
  
  resVar <- responseVariable
  modVer <- modelVersion
  modNum <- modelNumber
  
  # As we only have one possibility of female survival model, the user might 
  # not see that two values are necessary. 
  if (resVar == "femaleSurvival"){
      if (length(modVer) != length(modNum)){
        modVer <- rep(modVer, times = length(modNum))
      }
  }
  # As we only have one possibility of Type, the user might 
  # not see that two values are necessary. We can apply a fix.
  # This should me changed if we add other model Type i.e. Regional
  if (length(Type) != length(modVer)){
    modType <- rep(Type, times = length(modVer))
  } else {
    modType <- Type
  }
  
  # Lapply over combinations of the following variables
  # modelVersion, modelNumber, Type
  testthat::expect_true(all(length(modVer) == length(modNum),
                            length(modVer) == length(modType)), 
                        label = paste0("Please provide one modelNumber for each modelVersion and Type. ",
                                       "length(modelVersion) == length(modelNumber) or ",
                                       "length(modelVersion) == length(Type) "))
  
  modName <- paste(modVer, modNum,Type, sep = "_")
  
  DTs <- lapply(seq_along(modVer), FUN = function(modelIndex){
    # 1. Subset the table
    DT <- populationGrowthTable[responseVariable %in% resVar &
                                  modelVersion  %in% modVer[modelIndex] &
                                  ModelNumber  %in% modNum[modelIndex] &
                                  Type %in% modType[modelIndex],]
    if (any(is.na(DT[["StdErr"]]))){ 
      # If StdErr is NA, calculate StdErr
      # This only happens on Johnson models
      # Data points: recruitment (N = 58); adult female survival (N = 46) --> From Johnson et al 2020

      # N <- ifelse(resVar == "recruitment", 58, 46) # Chatting with EM on May 4th we
      # agreed that we don't need the N
      stdErrCalc <- calcFromCI(N = N,
                               ci_lower = DT[["lowerCI"]],
                               ci_upper = DT[["upperCI"]])
      DT[, StdErr := stdErrCalc]
    }
    return(DT)
  })
  names(DTs) <- modName
  return(DTs)
}

calcFromCI <- function(N, ci_upper, ci_lower, std = TRUE){
  # SD <- (sqrt(N)*(ci_upper-ci_lower))/3.92
  SD <- (ci_upper-ci_lower)/3.92
  # for StdErr
  # if (std)
    return(SD) #else
      # return(SD/sqrt(N))
}
