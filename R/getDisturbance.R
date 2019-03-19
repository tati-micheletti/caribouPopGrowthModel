getDisturbance <- function(currentTime = time(sim),
                           cohortData = sim$cohortData, # Has age info per pixel group
                           pixelGroupMap = sim$pixelGroupMap, #Map of pixel groups
                           startTime = start(sim),
                           endTime = end(sim),
                           recoveryTime = P(sim)$recoveryTime,
                           listSACaribou = sim$listSACaribou,
                           anthropogenicLayer = sim$anthropogenicLayer,
                           waterRaster = sim$waterRaster,
                           isRSF = FALSE){

  reproducible::Require("raster")
  reproducible::Require("magrittr")
  reproducible::Require("plyr")
  originalTime <- currentTime
  if (startTime > 1){
    relEndTime <- endTime - startTime
    currentTime <- originalTime - startTime
  }

  threadsDT <- getDTthreads()
  setDTthreads(1)
  on.exit({setDTthreads(threadsDT)}, add = TRUE)
  
  valPixelGroup <- data.table(pixelGroup = raster::getValues(x = pixelGroupMap)) %>%
    plyr::join(cohortData[, max(age), by = "pixelGroup"])
  
  names(valPixelGroup)[2] <- "age"
  ageMap <- raster::setValues(x = pixelGroupMap, values = valPixelGroup$age)
  
  if (!isRSF){
    listDistForEachShpForEachPoly <- lapply(X = listSACaribou, FUN = function(caribouShapefile){
      listPolyDist <- Cache(extractDisturbance, ageMap = ageMap,
                            caribouShapefile = caribouShapefile,
                            recoveryTime = recoveryTime,
                            anthropogenicLayer = anthropogenicLayer,
                            waterRaster = waterRaster)
    })
    disturbances <- list(listDistForEachShpForEachPoly) # List of the year
    name <- paste0("Year", originalTime)
    names(disturbances) <- name
  } else {
    # STOPPED HERE! Still need to adapt thhis function...
    partialLayers <- Cache(extractDisturbanceRSF, ageMap = ageMap,
                          recoveryTime = recoveryTime,
                          anthropogenicLayer = anthropogenicLayer,
                          waterRaster = waterRaster) # Here i can extract water Layer, roadDensity, oldBurn, newBurn, deciduos
    # Still missing elevation, vrug, shrub, herb and elevation^2, vrug^2 
    disturbances <- list(partialLayers) # List of the year
    name <- paste0("Year", originalTime)
    names(disturbances) <- name
  }
  
  return(disturbances)
}
