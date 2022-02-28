populationGrowthModel <-  function(femaleSurvivalModel,
                                   recruitmentModel,
                                   disturbances,
                                   currentTime,
                                   popModel,
                                   outputDir,
                                   useQuantiles = TRUE
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
if (useQuantiles){
  responseList <- sampleRates(covTable = covTable,
                              coefSample = femaleSurvivalModel[[modelType]][["coefSamples"]],
                              coefValues = femaleSurvivalModel[[modelType]][["coefValues"]],
                              modVer = modelType,
                              resVar = "femaleSurvival",
                              ignorePrecision = FALSE,
                              outputDir = outputDir,
                              returnSample = FALSE,
                              useQuantiles = FALSE)
} else {
  responseList <- generatePopGrowthPredictions(covTable = covTable,
                                               coeffTable = femaleSurvivalModel[[modelType]][["coeffTable"]],
                                               coeffValues = femaleSurvivalModel[[modelType]][["coeffValues"]],
                                               modelType =  modelType,
                                               model = "femaleSurevival")
}

    responseList[, c("modelType", "model") := list(modelType, "femaleSurvival")]
    return(responseList)
  }))
  recruitmentPredictions <- rbindlist(lapply(X = names(recruitmentModel),
                                                FUN = function(modelType) {
if (useQuantiles){
  responseList <- sampleRates(covTable = covTable,
                              coefSample = recruitmentModel[[modelType]][["coefSamples"]],
                              coefValues = recruitmentModel[[modelType]][["coefValues"]],
                              modVer = modelType,
                              resVar = "recruitment",
                              ignorePrecision = FALSE,
                              outputDir = outputDir,
                              returnSample = FALSE,
                              useQuantiles = FALSE)
} else {
  responseList <- generatePopGrowthPredictions(covTable = covTable,
                                               coeffTable = recruitmentModel[[modelType]][["coeffTable"]],
                                               coeffValues = recruitmentModel[[modelType]][["coeffValues"]],
                                               modelType =  modelType,
                                               model = "recruitment")
}
    responseList[, c("modelType", "model") := list(modelType, "recruitment")]
    return(responseList)
}))
  # data.table with these cols: polygon, area, average, stdErr, modelType, model
  # For each (i) polygon, (ii) area, and (iii) modelType, we need to apply the popMod equation

  combinations <- data.table(expand.grid(femaleSurvivalModel = names(femaleSurvivalModel),
                              recruitmentModel = names(recruitmentModel)))
  if (useQuantiles){
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

      growth <- do.call(what = popModel, args = alist(SadF = SadF,
                                                      recr = recr))
      growthMin <-  do.call(what = popModel, args = alist(SadF = SadFmin,
                                                          recr = recrmin))
      growthMax <-  do.call(what = popModel, args = alist(SadF = SadFmax,
                                                          recr = recrmax))

      # Melt DT to put recruitment and femaleSurvival results in the same table
      DTbind <- rbind(recruitmentPredictions[modelType == recrMod,],
                      femaleSurvivalPredictions[modelType == femSurvMod,])
      DTbind <- dcast(DTbind, polygon + area ~ model, value.var = c("average", "stdErr"))
      growthDT <- data.table(annualLambda = growth[["average"]],
                             annualLambdaMax = growthMax[[1]],
                             annualLambdaMin = growthMin[[1]],
                             polygon = femaleSurvivalPredictions$polygon)
      DTbind <- merge(DTbind, growthDT, by = "polygon")
      DTbind[, femSurvMod_recrMod := paste(femSurvMod, recrMod, sep = "::")]
      return(DTbind)
    }))
  } else {
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
      DTbind <- dcast(DTbind, polygon + area ~ model, value.var = c("average", "stdErr"))
      growthDT <- data.table(annualLambda = growth[["average"]],
                             annualLambdaMax = growthMax[["average"]],
                             annualLambdaMin = growthMin[["average"]],
                             polygon = femaleSurvivalPredictions$polygon)
      DTbind <- merge(DTbind, growthDT, by = "polygon")
      DTbind[, femSurvMod_recrMod := paste(femSurvMod, recrMod, sep = "::")]
      return(DTbind)
    }))
  }

  return(DT)
}
