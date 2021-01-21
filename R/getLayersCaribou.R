#' Get the necessary layers for predictive modules such as caribouPopGrowth and caribouRSF.
#'
#' @param currentTime numeric. Current time being used (i.e. \code{time(sim)}).
#' @param cohortData data.table. Output from LandR_Biomass module.
#' @param pixelGroupMap raster to identify the cohortData.
#' @param startTime numeric. startTime of the simulation. Needed to verify and potentially adjust relative simulation times.
#' @param endTime numeric. endTime of the simulation. Needed to verify and potentially adjust relative simulation times.
#' @param recoveryTime numeric. Recovery time in years that the forest needs to support Caribou. Default = 40.
#' @param listSACaribou list of shapefiles with polygons for which we want to calculate lambda for the caribou demographic model.
#' @param anthropogenicLayer Anthropogenic disturbance (raster) layer. Currently, 500m buffered anthropogenic disturbance for demographic models.
#' @param roadDensity Anthropogenic disturbance (raster) layer. Currently, road density layer used for RSF models.
#' @param waterRaster Raster layer indicating water bodies.
#' @param isRSF logical. Identify if it should get the layers for the RSF or demographic model.
#' @param decidousSp binary raster layer indicating if the dominant biomass in a pixel belongs to a deciduous species.
#' @param oldBurnTime numeric. Definition of the initial interval considered to be old burn. The end of this time is 20 years later (i.e. 40-60 years).
#' @param elevation RasterLayer of elevation
#' @param vrug RasterLayer of ruggeness
#' @param LCC05 RasterLayer of landcover classes 2005
#' @param reclassLCC05 List with reclassification for LCC05 values (i.e. LCC05 classes that should be classified as shrub or herbs)
#' @param rasterToMatch RasterLayer template for these layers to match.
#' @param destinationPath TODO
#'
#' @return TODO
#'
#' @author Tati Micheletti
#' @export
#' @importFrom crayon red
#' @importFrom data.table data.table setkey
#' @importFrom LandR prepInputsLCC
#' @importFrom raster dropLayer extract projectRaster raster stack
#' @importFrom reproducible prepInputs postProcess
#' @importFrom SpaDES.tools rasterizeReduced
#' @include createDynamicLayersRSF.R
#' @include createStaticLayersRSF.R
#' @include extractDisturbanceFast.R
#'
#' @rdname getLayersCaribou
getLayersCaribou <- function(currentTime,
                      cohortData, # Has age info per pixel group
                      pixelGroupMap, #Map of pixel groups
                      startTime,
                      endTime,
                      recoveryTime = 40,
                      listSACaribou,
                      anthropogenicLayer,
                      roadDensity,
                      waterRaster,
                      isRSF = FALSE,
                      decidousSp = NULL,
                      oldBurnTime = NULL,
                      elevation = NULL,
                      vrug = NULL,
                      LCC05 = NULL,
                      reclassLCC05 = NULL,
                      rasterToMatch = NULL,
                      destinationPath) {
  
  if (is.null(pixelGroupMap)){
    message(crayon::red(paste0("pixelGroupMap is NULL for year ", currentTime, ". Returning NA")))
    return(NA)
  }
  # In a posterior version, will need to make this flexible for the model covariates
  originalTime <- currentTime
  if (startTime > 1){
    relEndTime <- endTime - startTime
    currentTime <- originalTime - startTime
  }
  
  threadsDT <- getDTthreads()
  setDTthreads(1)
  on.exit({setDTthreads(threadsDT)}, add = TRUE)
  # Data assertions
  if (length(!is.na(cohortData$age)) != length(is.na(cohortData$age))){
    message(crayon::red("cohortData age has NA values and shouldn't. Activating browser for debug"))
    browser()
  }
  ageMap <- raster(pixelGroupMap)
  valsAge <- data.table(pixelID = 1:ncell(ageMap), pixelGroup = getValues(x = pixelGroupMap))
  newAgeVals <- valsAge[cohortData[, list(age = max(age, na.rm = TRUE)), by = "pixelGroup"], on = "pixelGroup"]
  ageMap[newAgeVals$pixelID] <- newAgeVals$age
  names(ageMap) <- "ageMap"
  
  if (!isRSF){
    listDistForEachShpForEachPoly <- lapply(X = names(listSACaribou), FUN = function(caribouShapefile){
      message("Calculating disturbance for ", caribouShapefile)
      listPolyDist <- extractDisturbFast(ageMap = ageMap,
                                             caribouShapefile = listSACaribou[[caribouShapefile]],
                                             recoveryTime = recoveryTime,
                                             anthropogenicLayer = anthropogenicLayer,
                                             waterRaster = waterRaster,
                                             rasterToMatch = rasterToMatch,
                                             destinationPath = destinationPath)
    })
    names(listDistForEachShpForEachPoly) <- names(listSACaribou)
    return(listDistForEachShpForEachPoly)
  } else {
    # Determine which pixels are deciduous
    setkey(cohortData, B)
    cohortData[, domSp := speciesCode[.N], by = "pixelGroup"]
    cohortData[, deciduous := ifelse(domSp %in% decidousSp, 1, 0)]
    
    # Create the deciduous map
    cohortDataRed <- cohortData[, c("pixelGroup", "deciduous"), with = FALSE]
    setkey(cohortDataRed, pixelGroup)
    cohortDataRed <- unique(cohortDataRed,  by = "pixelGroup")
    biomassMap <- SpaDES.tools::rasterizeReduced(reduced = cohortDataRed,
                                                 fullRaster = pixelGroupMap,
                                                 newRasterCols = "deciduous",
                                                 mapcode = "pixelGroup")
    
    # ageMap = old and new burns
    # anthropogenicLayer = roadDensity
    # waterLayer = waterRaster
    # Deciduous = biomassMap
    
    dynamicLayers <- createDynamicLayersRSF(ageMap = ageMap,
                                            biomassMap = biomassMap,
                                            biomassMapName = "Deciduous",
                                            oldBurnTime = oldBurnTime,
                                            oldBurnName = "OldBurn",
                                            newBurnName = "RecentBurn",
                                            roadDensity = roadDensity,
                                            roadDensityName = "RoadDensity",
                                            waterRaster = waterRaster,
                                            waterRasterName = "Water",
                                            RTM = rasterToMatch)
    
    staticLayers <- createStaticLayersRSF(elevation = elevation,
                                          vrug = vrug,
                                          LCC = LCC05,
                                          shrubName = "Shrub",
                                          herbName = "Herb",
                                          elevationName = "Elevation",
                                          vrugName = "Vrug",
                                          reclassLCC05 = reclassLCC05,
                                          dynamicLayers = dynamicLayers,
                                          RTM = rasterToMatch,
                                          destinationPath = destinationPath)
    
    # We need to override the LandR_Biomass pixels with deciduous trees that were originally classified as
    # "herbaceous" by ECCC
    staticLayers[["Deciduous"]][dynamicLayers[["Water"]] == 1] <- 0
    
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
