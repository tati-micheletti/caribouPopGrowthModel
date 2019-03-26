getLayers <- function(currentTime,
                           cohortData, # Has age info per pixel group
                           pixelGroupMap, #Map of pixel groups
                           startTime,
                           endTime,
                           recoveryTime,
                           listSACaribou,
                           anthropogenicLayer,
                           waterRaster,
                           isRSF = FALSE,
                           decidousSp,
                      oldBurnTime,
                      caribouDynCovs,
                      elevation,
                      vrug,
                      LCC05,
                      reclassLCC05,
                      RTM){
  
  # In a posterior version, will need to make this flexible for the model covariates
  reproducible::Require("raster")
  reproducible::Require("magrittr")
  originalTime <- currentTime
  if (startTime > 1){
    relEndTime <- endTime - startTime
    currentTime <- originalTime - startTime
  }
  
  threadsDT <- getDTthreads()
  setDTthreads(1)
  on.exit({setDTthreads(threadsDT)}, add = TRUE)
  
  ageMap <- raster(pixelGroupMap)
  valsAge <- data.table(pixelID = 1:ncell(ageMap), pixelGroup = getValues(x = pixelGroupMap))
  newAgeVals <- valsAge[cohortData[, list(age = max(age)), by = "pixelGroup"], on = "pixelGroup"]
  ageMap[newAgeVals$pixelID] <- newAgeVals$age
  names(ageMap) <- "ageMap"
  
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
    # Determine which pixels are deciduous
    setkey(cohortData, B)
    cohortData[, domSp := speciesCode[.N], by = "pixelGroup"]
    cohortData[, deciduous := ifelse(domSp %in% decidousSp, 1, 0)] 
    
    # Create the deciduous map
    cohortDataRed <- cohortData[, c("pixelGroup", "deciduous"), with = FALSE]    
    setkey(cohortDataRed, pixelGroup)
    cohortDataRed <- unique(cohortDataRed,  by = "pixelGroup")
    biomassMap <- SpaDES.tools::rasterizeReduced(reduced = cohortDataRed, fullRaster = pixelGroupMap,
                                                 newRasterCols = "deciduous", mapcode = "pixelGroup")
    
    # ageMap = old and new burns
    # anthropogenicLayer = roadDensity
    # waterLayer = waterRaster
    # Deciduous = biomassMap
    
    dynamicLayers <- Cache(createDynamicLayersRSF, 
                           ageMap = ageMap,
                           biomassMap = biomassMap,
                           biomassMapName = "Deciduous",
                           oldBurnTime = oldBurnTime,
                           oldBurnName = "OldBurn",
                           newBurnName = "RecentBurn",
                           anthropogenicLayer = anthropogenicLayer,
                           anthropogenicLayerName = "RoadDensity",
                           waterRaster = waterRaster,
                           waterRasterName = "Water",
                           RTM = RTM)
    
    staticLayers <- Cache(createStaticLayersRSF, 
                          elevation = elevation,
                          vrug = vrug,
                          LCC = LCC05,
                          shrubName = "Shrub",
                          herbName = "Herb",
                          elevationName = "Elevation",
                          vrugName = "Vrug",
                          reclassLCC05 = reclassLCC05,
                          dynamicLayers = dynamicLayers,
                          RTM = RTM)
    
    # We need to override the LandR_Biomass pixels with deciduous trees that were originally classified as "herbaceous" by ECCC 
    dynamicLayers[["Deciduous"]] <- staticLayers[["Deciduous"]]
    staticLayers <- raster::dropLayer(staticLayers, i = which(names(staticLayers)=="Deciduous"))
    
    # Stack both dynamic and static layers for prediction
    covStack <- raster::stack(dynamicLayers, staticLayers)
    covStack <- list(covStack) # List of the year
    name <- paste0("Year", originalTime)
    names(covStack) <- name
  }
  
  return(covStack)
}
