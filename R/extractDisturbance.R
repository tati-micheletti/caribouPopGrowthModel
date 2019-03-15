extractDisturbance <- function(ageMap = ageMap, 
                               caribouShapefile = caribouShapefile, 
                               recoveryTime = recoveryTime,
                               anthropogenicLayer = anthropogenicLayer,
                               waterRaster = waterRaster){
  
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
    waterRaster <- reproducible::postProcess(x = waterRaster, rasterToMatch = ageMap,
                                                    destinationPath = tempdir(), filename2 = NULL)
    anthropogenicLayer <- reproducible::postProcess(x = anthropogenicLayer, rasterToMatch = waterRaster,
                                                    maskWithRTM = TRUE, destinationPath = tempdir(),
                                                    filename2 = NULL)
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
      if (!is.null(extrAnthro) & (length(extrAnthro[[eachPoly]]) != length(extrFire[[eachPoly]]))){
        stop("Something went wrong with the fire and anthropogenic polygons. Please debug")
      }
    
    totPixels <- length(extrAnthro[[eachPoly]])
    
    # For fire:
    # From the age map calculate for each polygon the total amount of pixels that had fires
    # over the total number of pixels "available" to burn (non-NA, as NA can be cities/water/etc). # CHECK ABOUT WATER HERE!
    # Then multiply by 100 to work with %.
    
    totPixelsNotNAFire <- sum(!is.na(extrFire[[eachPoly]]))
    isRecentFire <- !is.na(extrFire[[eachPoly]]) & 
      extrFire[[eachPoly]] <= recoveryTime
    cummFire <- sum(isRecentFire, na.rm = TRUE)
    percentFire <- 100*(cummFire/totPixelsNotNAFire) # We are calculating percent disturbance ONLY for those pixels that are forest/within BCR 6 NWT
    
    if (!is.null(extrAnthro)){

      extrAnthro[[eachPoly]][extrAnthro[[eachPoly]] > 0] <- 1     # ============ WHEN ATHROPO LAYER IS READY, DELETE THIS LINE!!

      # Fore anthropogenic:
      # From the anthropo layer we calculate for each polygon the total amount of pixels that had disturbances
      # over the total number of pixels "available" to have it (non-NA when na is JUST WATER).
      # Then multiply by 100 to work with %.
      
      totPixelsNotNAAnthro <- sum(!is.na(extrAnthro[[eachPoly]]))
      isDevelopment <- !is.na(extrAnthro[[eachPoly]]) &
        extrAnthro[[eachPoly]] > 0
      cummAnthro <- sum(isDevelopment, na.rm = TRUE)
      percentAnthopo <- 100*(cummAnthro/totPixelsNotNAAnthro)
      # We are calculating percent disturbance ONLY for those pixels that are not water (non-NA) within BCR 6 NWT
      
      # CREATE TOTAL DISTURBANCE USING FIRE OR ANTHROPO
      isDistrubance <- isDevelopment | isRecentFire
      totPixelsNotNADist <- max(totPixelsNotNAFire, totPixelsNotNAAnthro)
      cummDist <- sum(isDistrubance, na.rm = TRUE)
      totalDisturbance <- 100*(cummDist/totPixelsNotNADist)
      # ===========================================================================================
    } else {
      percentAnthopo <- 0
    }
    # Data.frame of the disturbances:
    df <- data.frame(DH_Fire = percentFire, DH_Anthro = percentAnthopo, DH_Total = totalDisturbance)
    
  })
  names(listExtr) <- caribouShapefile[[nm]]
  
  return(listExtr)
}