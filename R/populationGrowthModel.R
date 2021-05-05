populationGrowthModel <-  function(femaleSurvivalModel,
                                   recruitmentModel,
                                   disturbances,
                                   currentTime,
                                   # growthInterval, # Hard coded here until we deal with yearly population
                                   popModel
                                   ){
  
  message(paste0("Forecasting caribou population growth for ", currentTime,"..."))
  covTable <- rbindlist(lapply(names(disturbances), function(shpfls){
    shpTable <- rbindlist(lapply(names(disturbances[[shpfls]]), function(pols){
      DT <- data.table::data.table(disturbances[[shpfls]][[pols]])
      DT[, c("polygon", "area") := list(pols, shpfls)]
      return(DT)
    }))
    return(shpTable)
  }))
  femaleSurvivalPredictions <- rbindlist(lapply(X = names(femaleSurvivalModel), 
                                      FUN = function(modelType) {
    responseList <- generatePopGrowthPredictions(covTable = covTable,
                                                 coeffTable = femaleSurvivalModel[[modelType]][["coeffTable"]],
                                                 coeffValues = femaleSurvivalModel[[modelType]][["coeffValues"]],
                                                 modelType =  modelType,
                                                 model = "femaleSurevival")
    responseList[, c("modelType", "model") := list(modelType, "femaleSurvival")]
    return(responseList)
  }))
  recruitmentPredictions <- rbindlist(lapply(X = names(recruitmentModel), 
                                                FUN = function(modelType) {
    responseList <- generatePopGrowthPredictions(covTable = covTable, 
                                                 coeffTable = recruitmentModel[[modelType]][["coeffTable"]],
                                                 coeffValues = recruitmentModel[[modelType]][["coeffValues"]],
                                                 modelType =  modelType,
                                                 model = "recruitment")
    responseList[, c("modelType", "model") := list(modelType, "recruitment")]
    return(responseList)
}))
  # data.table with these cols: polygon, area, average, stdErr, modelType, model
  # For each (i) polygon, (ii) area, and (iii) modelType, we need to apply the popMod equation

  combinations <- data.table(expand.grid(femaleSurvivalModel = names(femaleSurvivalModel),
                              recruitmentModel = names(recruitmentModel)))
  
  DT <- rbindlist(lapply(1:NROW(combinations), function(index){
    CI <- function(SD) qnorm(0.975)*SD
    femSurvMod <- unique(combinations[[index, "femaleSurvivalModel"]])
    recrMod <- unique(combinations[[index, "recruitmentModel"]])
    
    SadF <- femaleSurvivalPredictions[modelType == femSurvMod, "average"]
    SadFmin <- SadF - CI(femaleSurvivalPredictions[modelType == femSurvMod, "stdErr"])
    SadFmax <- SadF + CI(femaleSurvivalPredictions[modelType == femSurvMod, "stdErr"])
    
    recr <- recruitmentPredictions[modelType == recrMod, "average"]
    recrmin <- recr - CI(recruitmentPredictions[modelType == recrMod, "stdErr"])
    recrmax <- recr + CI(recruitmentPredictions[modelType == recrMod, "stdErr"])
    # ts <- growthInterval
    
    testthat::expect_true(NROW(SadF) == NROW(recr))
    
    growth <- do.call(what = popModel, args = alist(SadF = SadF,
                                        recr = recr))
    growthMin <-  do.call(what = popModel, args = alist(SadF = SadFmin,
                                                      recr = recrmin))
    growthMax <-  do.call(what = popModel, args = alist(SadF = SadFmax,
                                                      recr = recrmax))
    
    # Melt DT to put recruitment and femaleSurvival results in the same table 
    DTbind <- rbind(recruitmentPredictions[modelType == recrMod,],
                    femaleSurvivalPredictions[modelType == femSurvMod,])
    DTbind <- dcast(DTbind, Herd + area ~ model, value.var = c("average", "stdErr"))
    growthDT <- data.table(annualLambda = growth[["average"]], 
                           annualLambdaMax = growthMax[["average"]],
                           annualLambdaMin = growthMin[["average"]],
                           Herd = femaleSurvivalPredictions$Herd)
    DTbind <- merge(DTbind, growthDT, by = "Herd")
    DTbind[, femSurvMod_recrMod := paste(femSurvMod, recrMod, sep = "::")]
    return(DTbind)
  }))
  
  # # Adding to annualLambda  :: Leaving here for backward checks
  # DT[, c("annualLambda", "annualLambdaMin", "annualLambdaMax") := list(growth,#^(1/growthInterval), 
  #                                                                      growthMin,#^(1/growthInterval), 
  #                                                                      growthMax)]#^(1/growthInterval))]
  
  return(DT)
}