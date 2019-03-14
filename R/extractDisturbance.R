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
  names(extrFire) <- names(extrAnthro) <- caribouShapefile[[nm]]
  
  listExtr <- lapply(X = caribouShapefile[[nm]], FUN = function(eachPoly){
    if (!is.null(extrAnthro) & (length(extrAnthro[[eachPoly]]) != length(extrFire[[eachPoly]])))
      stop("Something went wrong with the fire and anthropogenic polygons. Please debug")
    totPixels <- length(extrAnthro[[eachPoly]])
    
    # Fire
    totPixelsNotNAFire <- sum(!is.na(extrFire[[eachPoly]]))
    cummFire <- sum(!is.na(extrFire[[eachPoly]]) & 
                      extrFire[[eachPoly]] <= recoveryTime, na.rm = TRUE)
    percentFire <- 100*(cummFire/totPixelsNotNAFire) # We are calculating percent disturbance ONLY for those pixels that are forest/within BCR 6 NWT
    
    # ============ WHEN ATHROPO LAYER IS READY, DELETE BERLOW LINES  ============================== 
    if (!is.null(extrAnthro)){
      areaName <- if (!is.null(caribouShapefile$Area_ha)) "Area_ha" else "Area_km2"
      areaSize <- caribouShapefile[caribouShapefile[[nm]] == eachPoly,][[areaName]]
      multi <- if (areaName == "Area_km2") 1 else 0.01 # Multiplication factor to correct from ha to km2
      percentAnthopo <- 100*sum(extrAnthro[[eachPoly]], na.rm = TRUE)/(multi*areaSize) # Converting area from ha to km2 and making the % by multiplying by 100
      
      # ============ WHEN ATHROPO LAYER IS READY, DELETE ABOVE LINES AND USE THESE INSTEAD ======== 
      
      # percentAnthopo <- sum(extrAnthro[[eachPoly]], na.rm = TRUE)/totPixels
      # We are calculating percent disturbance of anthropogenic features for ALL pixels in each poly of BCR 6 NWT
      
      # ===========================================================================================
    } else {
      percentAnthopo <- 0
    }
    # Data.frame of the disturbances:
    df <- data.frame(fire = percentFire, anthropo = percentAnthopo)
  })
  names(listExtr) <- caribouShapefile[[nm]]
  
  return(listExtr)
}