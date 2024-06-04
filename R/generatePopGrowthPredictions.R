generatePopGrowthPredictions <- function(covTable,
                                   coeffTable,
                                   coeffValues,
                                   modelType,
                                   model,
                                   usequantiles){

  tic(paste0("Elapsed time for caribou prediction for ",
             model, " for ", modelType,":"))
  # # Simplifying the covariates table
  # if ("polygon" %in% names(covTable))
  #     covTable[, c("polygon", "area"):= NULL]
  library("matrixStats")
  # The matrix multiple -- will result in 100 columns
  # Coefficients (coeffTable); Covariates (covTableRed)
  # This results in a pixel * bootstrap replicate (rows x columns)
  whichCovariates <- names(coeffValues)[!names(coeffValues) %in% c("Intercept",
                                                                   "intercept",
                                                                   "Precision",
                                                                   "precision")]
  missingCovs <- setdiff(whichCovariates, colnames(covTable))
  if (length(missingCovs) > 0) {
    stop("Covariates missing in covTable: ", paste0(missingCovs, 
                                                    collapse = ", "), call. = FALSE)
  }
  
  covTableRed <- covTable[, ..whichCovariates]

  if (grepl(x = modelType, pattern = "Johnson")) {
    # If the models are Johnson models:
    # After multiplying, need to back transform

    # Not sure what is right, but they used log-link, which would be just exp()
    # expit <- function (x) 1/(1 + exp(-x))
    # logit <- log(µ/(1−µ))
    # logit <- exp(x)/(1+exp(x))
    
    int <- coeffTable[, which(colnames(coeffTable) %in% c("Intercept",
                                                          "intercept"))]

    predictedTableSD <- exp(int +
                                as.matrix(covTableRed) %*%
                                t(coeffTable[,-which(colnames(coeffTable) %in% c("Intercept",
                                                                                 "intercept",
                                                                                 "Precision",
                                                                                 "precision"))]))
    
    # Here I need to sample the quantiles I am interested in over the columns.    
    quants <- matrixStats::rowQuantiles(predictedTableSD, probs = usequantiles)

    # Now I need to gather all values in the row that are between the quantiles
    predicted <- t(sapply(X = 1:NROW(predictedTableSD),
                             FUN = getSDquantiles, 
                             predictedTableSD = predictedTableSD,
                             quants = quants))
  } else {
    if (grepl(x = modelType, pattern = "ECCC")){
      
      int <- coeffTable[, which(colnames(coeffTable) %in% c("Intercept",
                                                            "intercept"))]
      predictedTableSD <- int + (as.matrix(covTableRed) %*%
                                   t(coeffTable[,-which(colnames(coeffTable) %in% c("Intercept",
                                                                                    "intercept",
                                                                                    "Precision",
                                                                                    "precision"))]))
      if (resVar == "recruitment") {
        predictedTableSD = predictedTableSD/100
      }
      
      # Here I need to sample the quantiles I am interested in over the columns.    
      quants <- matrixStats::rowQuantiles(predictedTableSD, probs = usequantiles)
      
      # Now I need to gather all values in the row that are between the quantiles
      predicted <- t(sapply(X = 1:NROW(predictedTableSD),
                            FUN = getSDquantiles, 
                            predictedTableSD = predictedTableSD,
                            quants = quants))
      } else {
      stop("Currently only ECCC 2011 and Johnson et al., 2020 models implemented")
    }
  }

  resultDT <- data.table(Herd = covTable[["polygon"]],
                         average = predicted[,1],
                         stdErr = predicted[,2],
                         PIlow = quants[,1],
                         PIhigh = quants[,2],
                         area = covTable[["area"]])
  toc()
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
