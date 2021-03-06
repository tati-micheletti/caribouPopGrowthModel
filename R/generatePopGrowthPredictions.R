generatePopGrowthPredictions <- function(covTable,
                                   coeffTable,
                                   coeffValues,
                                   modelType,
                                   polygonsAreas, 
                                   model){

  tic(paste0("Elapsed time for caribou prediction for ",
             model, " for ", modelType,":"))
  # Simplifying the covariates table
  if ("polygon" %in% names(covTable))
      covTable[, c("polygon", "area"):= NULL]
  library("matrixStats")
  # The matrix multiple -- will result in 100 columns
  # Coefficients (coeffTable); Covariates (covTableRed)
  # This results in a pixel * bootstrap replicate (rows x columns) 
  whichCovariates <- names(coeffValues)[!names(coeffValues) %in% c("Intercept", 
                                                                   "intercept")]
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
                                                                                 "intercept"))]))
    # Uncertainty across replicates
    predictedSD <- matrixStats::rowSds(predictedTableSD)

    # Now the model calculations
    predicted <- as.numeric(exp(as.matrix(coeffValues)[which(colnames(coeffValues) %in% c("Intercept",
                                                                                            "intercept"))] +
                                    as.matrix(covTableRed) %*%
                                    as.matrix(coeffValues)[-which(colnames(coeffValues) %in% c("Intercept",
                                                                                               "intercept"))]))
  } else {
    if (grepl(x = modelType, pattern = "ECCC")){
      int <- coeffTable[, which(colnames(coeffTable) %in% c("Intercept",
                                                            "intercept"))]
      predictedTableSD <- int + (as.matrix(covTableRed) %*%
                                   t(coeffTable[,-which(colnames(coeffTable) %in% c("Intercept",
                                                                                    "intercept"))]))
      # Uncertainty across replicates
      predictedSD <- matrixStats::rowSds(predictedTableSD)

      # Now the model calculations
      predicted <- as.numeric(as.matrix(coeffValues)[which(colnames(coeffValues) %in% c("Intercept",
                                                                                        "intercept"))] +
                                (as.matrix(covTableRed) %*%
                                   as.matrix(coeffValues)[-which(colnames(coeffValues) %in% c("Intercept",
                                                                                              "intercept"))]))

      } else {
      stop("Currently only ECCC 2011 and Johnson et al., 2020 models implemented")
    }
  }
  
  resultDT <- data.table(polygonsAreas,
                         average = predicted, 
                         stdErr = predictedSD)
  toc()
  return(resultDT)
}
