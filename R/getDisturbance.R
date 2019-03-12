getDisturbance <- function(currentTime = time(sim),
                           cohortData = sim$cohortData, # Has age info per pixel group
                           pixelGroupMap = sim$pixelGroupMap, #Map of pixel groups
                           startTime = start(sim),
                           endTime = end(sim),
                           recoveryTime = P(sim)$recoveryTime,
                           listSACaribou = sim$listSACaribou,
                           anthropogenicLayer = sim$anthropogenicLayer){

  reproducible::Require("raster")
  reproducible::Require("magrittr")
  reproducible::Require("plyr")
  originalTime <- currentTime
  if (startTime > 1){
    relEndTime <- endTime - startTime
    currentTime <- originalTime - startTime
  }
  
  valPixelGroup <- data.table(pixelGroup = raster::getValues(x = pixelGroupMap)) %>%
    plyr::join(cohortData[, max(age), by = "pixelGroup"])
  names(valPixelGroup)[2] <- "age"
  ageMap <- raster::setValues(x = pixelGroupMap, values = valPixelGroup$age)
  
  listDistForEachShpForEachPoly <- lapply(X = listSACaribou, FUN = function(caribouShapefile){
    listPolyDist <- Cache(extractDisturbance, ageMap = ageMap,
                          caribouShapefile = caribouShapefile,
                          recoveryTime = recoveryTime,
                          anthropogenicLayer = anthropogenicLayer)
  })
  names(listDistForEachShpForEachPoly) <- names(listSACaribou)
  
  DH_Tot <- list(listDistForEachShpForEachPoly)
  name <- paste0("Year", originalTime)
  names(DH_Tot) <- name
  # Calc is done in percentage of the area
  return(DH_Tot)
}
