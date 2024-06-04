sampleRates <- function (covTable, coefSamples, coefValues, modelVersion, resVar,
                         ignorePrecision, returnSample, quantilesToUse = NULL, 
                         predInterval = c(0.025, 0.975), 
                         transformFn = function(y) {y}){
  
  browser() # To revise with Eliot!
  
  
  whichCovariates <- names(coefValues)[!names(coefValues) %in% 
                                         c("Intercept", "intercept", 
                                           "precision", "Precision")]
  
  missingCovs <- setdiff(whichCovariates, colnames(covTable))
  if (length(missingCovs) > 0) {
    stop("Covariates missing in covTable: ", paste0(missingCovs, 
                                                    collapse = ", "), call. = FALSE)
  }
  covTableRed <- covTable[, ..whichCovariates]
  if (grepl(x = modelVersion, pattern = "Johnson")) {
    predictSDFun <- function(x, intt) {
      transformFn(exp(intt + x))
    }
    phiSampleFun <- betaSample
    # predictFun <- function(coefValues, covTableRed) {
    #   transformFn(exp(as.numeric(as.matrix(coefValues)[which(colnames(coefValues) %in% 
    #                                                            c("Intercept", "intercept"))] + as.matrix(covTableRed) %*% 
    #                                as.matrix(coefValues)[-which(colnames(coefValues) %in% 
    #                                                               c("Intercept", "intercept", "precision", "Precision"))])))
    # }
    recruitDiv <- 1
  }
  else if (grepl(x = modelVersion, pattern = "ECCC")) {
    predictSDFun <- function(x, intt) {
      intt + x
    }
    phiSampleFun <- normalSample
    # predictFun <- function(coefValues, covTableRed) {
    #   as.numeric(as.matrix(coefValues)[which(colnames(coefValues) %in% 
    #                                            c("Intercept", "intercept"))] + as.matrix(covTableRed) %*% 
    #                as.matrix(coefValues)[-which(colnames(coefValues) %in% 
    #                                               c("Intercept", "intercept", "precision", "Precision"))])
    # }
    recruitDiv <- 100
  }
  else {
    stop("Currently only ECCC 2011 and Johnson et al., 2020 models implemented")
  }
  intt <- coefSamples[, which(colnames(coefSamples) %in% c("Intercept", 
                                                           "intercept"))]
  phi <- coefSamples[, which(colnames(coefSamples) %in% c("Precision", 
                                                          "precision"))]
  # predictedTableSD <- as.matrix(covTableRed) %*% t(coefSamples[, 
  #                                                              -which(colnames(coefSamples) %in% c("Intercept", "intercept", 
  #                                                                                                  "precision", "Precision"))])
  # predictedTableSD <- t(apply(X = predictedTableSD, MARGIN = 1, 
  #                             FUN = predictSDFun, 
  #                             intt = intt))
  if (resVar == "recruitment") {
    predictedTableSD <- predictedTableSD/recruitDiv
  }
  if (!ignorePrecision) {
    if (length(phi) == 0) {
      stop("Missing precision parameter. Set ignorePrecision = TRUE or", 
           " add a precision column to coefSamples", call. = FALSE)
    }
    if (nrow(predictedTableSD) < 1) {
      stop("This code assumes at least one row.")
    }

    # predictedTableSD <- t(apply(predictedTableSD, 1, phiSampleFun, 
    #                             phi = phi, quantilesToUse = quantilesToUse))
  } 
  # Here I need to sample the quantiles I am interested in over the columns.    
  quants <- matrixStats::rowQuantiles(predictedTableSD, probs = usequantiles)
  
  # Now I need to gather all values in the row that are between the quantiles
  predicted <- t(sapply(X = 1:NROW(predictedTableSD),
                        FUN = getSDquantiles, 
                        predictedTableSD = predictedTableSD,
                        quants = quants))
  
  if (resVar == "recruitment") {
    predicted = predicted/recruitDiv
  }

  resultDT <- data.table(Herd = covTable[["polygon"]],
                         average = predicted[["Average"]],
                         stdErr = predicted[["SD"]],
                         PIlow = quants[1],
                         PIhigh = quants[2],
                         area = covTable[["area"]])
  
  return(resultDT)
}

# Function for returning the stderr of all values in the quantile
getSDquantiles <- function(INDEX, predictedTableSD, quants){
  quantIndex <- quants[INDEX,]
  predIndex <- predictedTableSD[INDEX,]
  allVals <- predIndex[predIndex >= quantIndex[1] & predIndex <= quantIndex[2]]
  SD <- sd(allVals)
  Average <- mean(allVals)
  return(data.table(Average = Average,
                    SD = SD))
}
