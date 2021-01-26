populationGrowthModel <-  function(femaleSurvivalModel,
                                   recruitmentModel,
                                   disturbances,
                                   currentTime,
                                   growthInterval,
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
  polygonsAreas <- covTable[, c("polygon", "area")]
  femaleSurvivalPredictions <- rbindlist(lapply(X = names(femaleSurvivalModel), 
                                      FUN = function(modelType) {
                                        
    responseList <- generatePopGrowthPredictions(covTable = covTable,
                                                 coeffTable = femaleSurvivalModel[[modelType]][["coeffTable"]],
                                                 coeffValues = femaleSurvivalModel[[modelType]][["coeffValues"]],
                                                 modelType =  modelType,
                                                 polygonsAreas = polygonsAreas,
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
                                                 polygonsAreas = polygonsAreas,
                                                 model = "recruitment")
    responseList[, c("modelType", "model") := list(modelType, "recruitment")]
    return(responseList)
}))
  # data.table with these cols: polygon, area, average, stdErr, modelType, model
  # For each (i) polygon, (ii) area, and (iii) modelType, we need to apply the popMod equation

  combinations <- data.table(expand.grid(femaleSurvivalModel = names(femaleSurvivalModel),
                              recruitmentModel = names(recruitmentModel)))
  
  DT <- rbindlist(lapply(1:NROW(combinations), function(index){

    CI <- function(SD) qnorm(0.975)*SD/sqrt(100) # This dev came from 100 samples
    
    femSurvMod <- unique(combinations[[index, "femaleSurvivalModel"]])
    recrMod <- unique(combinations[[index, "recruitmentModel"]])
    
    SadF <- femaleSurvivalPredictions[modelType == femSurvMod, "average"]
    SadFmin <- SadF - CI(femaleSurvivalPredictions[modelType == femSurvMod, "stdErr"])
    SadFmax <- SadF + CI(femaleSurvivalPredictions[modelType == femSurvMod, "stdErr"])
    
    recr <- recruitmentPredictions[modelType == recrMod, "average"]
    recrmin <- recr - CI(recruitmentPredictions[modelType == recrMod, "stdErr"])
    recrmax <- recr + CI(recruitmentPredictions[modelType == recrMod, "stdErr"])
    ts <- growthInterval
    
    growth <- do.call(what = popModel, args = alist(SadF = SadF,
                                        recr = recr,
                                        ts = ts))
    growthMin <-  do.call(what = popModel, args = alist(SadF = SadFmin,
                                                      recr = recrmin,
                                                      ts = ts))
    growthMax <-  do.call(what = popModel, args = alist(SadF = SadFmax,
                                                      recr = recrmax,
                                                      ts = ts))
    
    # Melt DT to put recruitment and femaleSurvival results in the same table 
    DT <- rbind(recruitmentPredictions[index],
                    femaleSurvivalPredictions[index])
    DT[, c("growth", "growthMin", "growthMax") := list(as.numeric(growth), 
                                                       as.numeric(growthMin), 
                                                       as.numeric(growthMax))]
    return(DT)
  }))
  return(DT)
}