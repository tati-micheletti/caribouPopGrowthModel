extractDisturbance <- function(ageMap = ageMap, 
                               caribouShapefile = caribouShapefile, 
                               recoveryTime = recoveryTime,
                               anthropogenicLayer = anthropogenicLayer){
  
  # For fire disturbance; if not everything overlaps/matches, postProcess the layers
  tryCatch(expr = {
    extrFire <- Cache(raster::extract, x = ageMap, y = caribouShapefile, na.rm = FALSE)
  }, error = function(e){
    reprojCaribouShape <- reproducible::postProcess(x = reprojCaribouShape, rasterToMatch = ageMap,
                                                          destinationPath = tempdir(), filename2 = NULL)
    extrFire <- Cache(raster::extract, x = ageMap, y = reprojCaribouShape, na.rm = FALSE)
  }
  )
  
  # For anthropogenic disturbance; if not everything overlaps/matches, postProcess the layers
  if (!is.null(anthropogenicLayer)){
    tryCatch(expr = {
      extrAnthro <- Cache(raster::extract, x = anthropogenicLayer, y = caribouShapefile, na.rm = FALSE)
    }, error = function(e){
      if (!exists(reprojCaribouShape))
        reprojCaribouShape <- reproducible::postProcess(x = reprojCaribouShape, rasterToMatch = ageMap,
                                                        destinationPath = tempdir(), filename2 = NULL)
      tryCatch({raster::stack(anthropogenicLayer, ageMap)}, error = function(e){
        anthropogenicLayer <- reproducible::postProcess(x = anthropogenicLayer, rasterToMatch = ageMap,
                                                        destinationPath = tempdir(), filename2 = NULL)
      })
      extrAnthro <- Cache(raster::extract, x = anthropogenicLayer, y = reprojCaribouShape, na.rm = FALSE)
    }
    ) 
  }
  
  #Naming both fire and anthro disturbances
  nm <- if (!is.null(caribouShapefile$NAME)) "NAME" else "Name"
  names(extrFire) <- caribouShapefile[[nm]]
  
  browser() # HOW SHOULD I EXTRACT BOTH INFORMATION (ANTHROPO + FIRE?)
  listExtr <- lapply(X = extrFire, FUN = function(eachPoly){
    totPixels <- length(eachPoly)
    totPixelsNotNA <- sum(!is.na(eachPoly))
    cummFire <- sum(!is.na(eachPoly) & eachPoly <= recoveryTime, na.rm = TRUE)
    value <- 100*(cummFire/totPixelsNotNA) # We are calculating percent disturbance ONLY for those pixels that are forest/within BCR 6 NWT
  })
  
  return(listExtr)
}