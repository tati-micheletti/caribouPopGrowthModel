populationGrowthModel <-  function(femaleSurvivalModel,
                                   recruitmentModel,
                                   disturbances,
                                   currentTime,
                                   popModel,
                                   useQuantiles = TRUE # Change this to another argument i.e., usePrecision if needed
                                   ){

  usequantiles <- if (isTRUE(useQuantiles)) c(0.025, 0.975) else 
    if (isFALSE(useQuantiles)) c(0, 1) else 
      useQuantiles
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

  # Below has been changed to be used with the newest caribouMetrics -- Something is wrong in this function.
  # Check with Eliot. In the meantime I will use my version. It is missing the beta distribution, but that doesn't
  # change much the results.
  # responseList <- sampleRates(covTable = covTable,
  #                             coefSample = femaleSurvivalModel[[modelType]][["coefSamples"]],
  #                             coefValues = femaleSurvivalModel[[modelType]][["coefValues"]],
  #                             modelVersion = modelType,
  #                             resVar = "femaleSurvival",
  #                             ignorePrecision = FALSE,
  #                             returnSample = FALSE,
  #                             quantilesToUse = usequantiles) # Broken.


  responseList <- generatePopGrowthPredictions(covTable = covTable,
                                               coeffTable = femaleSurvivalModel[[modelType]][["coefSamples"]],
                                               coeffValues = femaleSurvivalModel[[modelType]][["coefValues"]],
                                               modelType =  modelType,
                                               model = "femaleSurvival",
                                               usequantiles = usequantiles)

    responseList[, c("modelType", "model") := list(modelType, "femaleSurvival")]
    return(responseList)
  }))
  recruitmentPredictions <- rbindlist(lapply(X = names(recruitmentModel),
                                                FUN = function(modelType) {

  responseList <- generatePopGrowthPredictions(covTable = covTable,
                                               coeffTable = recruitmentModel[[modelType]][["coefSamples"]],
                                               coeffValues = recruitmentModel[[modelType]][["coefValues"]],
                                               modelType =  modelType,
                                               model = "recruitment",
                                               usequantiles = usequantiles)

    responseList[, c("modelType", "model") := list(modelType, "recruitment")]
    return(responseList)
}))
  # data.table with these cols: polygon, area, average, stdErr, modelType, model
  # For each (i) polygon, (ii) area, and (iii) modelType, we need to apply the popMod equation

  combinations <- data.table(expand.grid(femaleSurvivalModel = names(femaleSurvivalModel),
                              recruitmentModel = names(recruitmentModel)))

    DT <- rbindlist(lapply(1:NROW(combinations), function(index){

      femSurvMod <- unique(combinations[[index, "femaleSurvivalModel"]])
      recrMod <- unique(combinations[[index, "recruitmentModel"]])

      SadF <- femaleSurvivalPredictions[modelType == femSurvMod, "average"]
      SadFmin <- femaleSurvivalPredictions[modelType == femSurvMod, "PIlow"]
      SadFmax <- femaleSurvivalPredictions[modelType == femSurvMod, "PIhigh"]

      recr <- recruitmentPredictions[modelType == recrMod, "average"]
      recrmin <- recruitmentPredictions[modelType == recrMod, "PIlow"]
      recrmax <- recruitmentPredictions[modelType == recrMod, "PIhigh"]

      testthat::expect_true(NROW(SadF) == NROW(recr))

      growth <- do.call(what = popModel, args = alist(SadF = as.numeric(unlist(SadF)),
                                                      recr = as.numeric(unlist(recr))))
      growthMin <-  do.call(what = popModel, args = alist(SadF = as.numeric(unlist(SadFmin)),
                                                          recr = as.numeric(unlist(recrmin))))
      growthMax <-  do.call(what = popModel, args = alist(SadF = as.numeric(unlist(SadFmax)),
                                                          recr = as.numeric(unlist(recrmax))))

      # Melt DT to put recruitment and femaleSurvival results in the same table
      DTbind <- rbind(recruitmentPredictions[modelType == recrMod,],
                      femaleSurvivalPredictions[modelType == femSurvMod,])
      DTbind <- dcast(DTbind, Herd + area ~ model, value.var = c("average", "stdErr"))
      growthDT <- data.table(annualLambda = growth,
                             annualLambdaMax = growthMax,
                             annualLambdaMin = growthMin,
                             Herd = femaleSurvivalPredictions$Herd)
      DTbind <- merge(DTbind, growthDT, by = "Herd")
      DTbind[, femSurvMod_recrMod := paste(femSurvMod, recrMod, sep = "::")]
      return(DTbind)
    }))
  
  return(DT)
}

