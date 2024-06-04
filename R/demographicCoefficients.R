demographicCoefficients <- function (replicates, modelVersion = "Johnson", 
                                     survivalModelNumber = "M1", 
                                     recruitmentModelNumber = "M4", 
                                     useQuantiles = TRUE, 
                                     populationGrowthTable) {

  if (any(length(modelVersion) > 1, length(survivalModelNumber) > 
          1, length(recruitmentModelNumber) > 1)) {
    stop("Multiple models. modelVersion, survivalModelNumber, ", 
         "and recruitmentModelNumber must have length 1", 
         call. = FALSE)
  }
  quantsToUse <- prepQuantiles(useQuantiles)
  populationGrowthTable <- data.table::data.table(populationGrowthTable)
  DT_S <- getCoefs(populationGrowthTable, resVar = "femaleSurvival", 
                   modelVersion = modelVersion, modNum = survivalModelNumber)[[1]]
  coefSamples_S <- sampleCoefs(DT_S, replicates)
  DT_R <- getCoefs(populationGrowthTable, resVar = "recruitment", 
                   modelVersion = modelVersion, modNum = recruitmentModelNumber)[[1]]
  coefSamples_R <- sampleCoefs(DT_R, replicates)
    coefSamples_S$quantiles <- sample(getQuantiles(x = replicates, 
                                                   low = quantsToUse[1], 
                                                   high = quantsToUse[2]), 
                                      replace = FALSE)
    coefSamples_R$quantiles <- sample(getQuantiles(x = replicates, 
                                                   low = quantsToUse[1], 
                                                   high = quantsToUse[2]), 
                                      replace = FALSE)
  return(list(modelVersion = modelVersion, 
              coefSamples_Survival = coefSamples_S, 
              coefSamples_Recruitment = coefSamples_R))
 }
 
 getQuantiles <- function (x, low = 0.025, high = 0.975){
   return(low + (seq(0, x - 1)/(x - 1)) * (high - low))
 }
 
 prepQuantiles <- function (useQuantiles, quantilesIn = NULL){
   if (length(useQuantiles) == 2) {
     if (!all(min(useQuantiles) >= 0, max(useQuantiles) <= 
              1)) {
       stop("useQuantiles must be between 0 and 1")
     }
     if (!is.null(quantilesIn)) {
       warning("popGrowthPars contains quantiles so they are used and useQuantiles is ignored", 
               call. = FALSE)
       quantsToUse <- quantilesIn
     }
     else {
       quantsToUse <- useQuantiles
     }
   }
   else if (length(useQuantiles) == 1) {
     if (useQuantiles) {
       if (!is.null(quantilesIn)) {
         message("popGrowthPars contains quantiles so they are used instead of the defaults")
         quantsToUse <- quantilesIn
       }
       else {
         quantsToUse <- c(0.025, 0.975)
       }
     }
     else {
       quantsToUse <- NULL
     }
   }
   else {
     stop("useQuantiles must have length 1 or 2")
   }
   return(quantsToUse)
 }
 
 getCoefs <- function (populationGrowthTable, resVar, modelVersion, modNum){
   populationGrowthTableReduced <- populationGrowthTable[responseVariable %in% resVar,]
   if (resVar == "femaleSurvival") {
     if (length(modelVersion) != length(modNum)) {
       modelVersion <- rep(modelVersion, times = length(modNum))
     }
   }
   if (length(modelVersion) != length(modNum)) {
     stop("Please provide one modNum for each modelVersion. length(modelVersion) == length(modNum)", 
          call. = FALSE)
   }
   selectedMods <- data.frame(modelVersion = modelVersion, responseVariable = resVar, 
                              ModelNumber = modNum)
   missingMods <- dplyr::anti_join(selectedMods, populationGrowthTableReduced, 
                            by = c("modelVersion", "ModelNumber", "responseVariable"))
   if (nrow(missingMods) > 0) {
     stop("Model not available. There is no model: ", paste0(missingMods$modelVersion, 
                                                             ", ", missingMods$responseVariable, ", ", missingMods$ModelNumber, 
                                                             collapse = "\r\n"), call. = FALSE)
   }
   Type <- "National" #TODO Once we have more/other models work on this
   modType <- rep(Type, times = length(modelVersion))
   
   modName <- paste(modelVersion, modNum, Type, sep = "_")

   DTs <- lapply(seq_along(modelVersion), FUN = function(modelIndex) {
     DT <- populationGrowthTableReduced[responseVariable %in% resVar & 
                                          modelVersion %in% modelVersion & 
                                          ModelNumber %in% modNum & 
                                          Type %in% modType, ]
     if (any(is.na(DT[["StdErr"]]))) {
       stdErrCalc <- calcFromCI(ci_lower = DT[["lowerCI"]], 
                                ci_upper = DT[["upperCI"]])
       stdErrCalc[!is.na(DT[["StdErr"]])] <- DT[["StdErr"]][!is.na(DT[["StdErr"]])]
       DT[,StdErr := stdErrCalc]
     }
     return(DT)
   })
   names(DTs) <- modName
   return(DTs)
 }
 
 calcFromCI <- function(ci_upper, ci_lower) {
   SD <- (ci_upper - ci_lower)/3.92
   return(SD)
 }
 
 sampleCoefs <- function (coefTable, replicates) {
   allCoefs <- coefTable[["Coefficient"]]
   coefSamples <- lapply(X = allCoefs, function(coef) {
       vec <- rnorm(n = replicates, mean = as.numeric(
         coefTable[Coefficient == coef, "Value"]),
         sd = as.numeric(
           coefTable[Coefficient == coef, "StdErr"]))
       return(vec)
   })
   names(coefSamples) <- allCoefs
   coefMatrix <- do.call(cbind, coefSamples)
   coefValues <- data.table::data.table(t(coefTable[, "Value"]))
   names(coefValues) <- coefTable[["Coefficient"]]
   coefStdErrs <- data.table::data.table(t(coefTable[, "StdErr"]))
   names(coefStdErrs) <- coefTable[["Coefficient"]]
   modList <- list(coefSamples = coefMatrix, 
                   coefValues = coefValues, 
                   coefStdErrs = coefStdErrs)
   return(modList)
 }
 