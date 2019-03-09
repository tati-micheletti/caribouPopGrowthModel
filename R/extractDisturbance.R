extractDisturbance <- function(ageMap = ageMap, 
                               caribouShapefile = caribouShapefile, 
                               recoveryTime = recoveryTime){

  reprojCaribouShape <- reproducible::postProcess(x = caribouShapefile, rasterToMatch = ageMap,
                                                  destinationPath = tempdir(), filename2 = NULL)
  extr <- Cache(raster::extract, x = ageMap, y = reprojCaribouShape, na.rm = FALSE)
  nm <- if (!is.null(reprojCaribouShape$NAME)) "NAME" else "Name"
  names(extr) <- reprojCaribouShape[[nm]]
  
  listExtr <- lapply(X = extr, FUN = function(eachPoly){
    totPixels <- length(eachPoly)
    totPixelsNotNA <- sum(!is.na(eachPoly))
    cummFire <- sum(!is.na(eachPoly) & eachPoly <= recoveryTime, na.rm = TRUE)
    value <- 100*(cummFire/totPixelsNotNA) # We are calculating percent disturbance ONLY for those pixels that are forest/within BCR 6 NWT
  })
  
  return(listExtr)
}