createModels <- function(caribouCoefTable = sim$caribouCoefTable,
                         modelsToUse = sim$modelsToUse){

  message("Using model ", crayon::magenta(modelsToUse)," for caribou...")
  if (length(modelsToUse)>1)
    stop("Please provide only one model to be used")
  if (!modelsToUse %in% c("M3", "M7"))
    stop("Only model M3 and M7 have been implemented so far")
  
  modelCoeff <- caribouCoefTable[ModelNum == modelsToUse]

  equation <- createEquation(model = modelCoeff)
  modList <- list(equation)
  names(modList) <- modelsToUse
  return(modList)
}
