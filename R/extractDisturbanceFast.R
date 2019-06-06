extractDisturbanceFast <- function(ageMap, 
                               caribouShapefile, 
                               recoveryTime = 40,
                               anthropogenicLayer = NULL,
                               waterRaster){
  
  # Make sure that the layers align. This will only happen once until the anthropogenic layer becomes dynamic.
  waterRaster <- reproducible::postProcess(x = waterRaster,
                                           rasterToMatch = ageMap,
                                           destinationPath = tempdir(),
                                           filename2 = NULL)
  # Convert NA background of lineDensity/anthropogenic disturbance to 0, while water stays as NA
  backgroundWithoutWater <- rasterToMatch
  backgroundWithoutWater[waterRaster == 1] <- NA
  backgroundWithoutWater[!is.na(backgroundWithoutWater)] <- 0
  
  # Check for anthropogenic layer and make sure it aligns with the others
  if (!is.null(anthropogenicLayer)){
    anthropogenicLayer <- reproducible::postProcess(x = anthropogenicLayer,
                                                    rasterToMatch = waterRaster,
                                                    maskWithRTM = TRUE,
                                                    destinationPath = tempdir(),
                                                    filename2 = NULL)
    # Fix anthropogenic layer that is NA/1 only. Add zeros to all pixels that are water, basically (which is where we can't build anything) 
    backgroundWithoutWater[anthropogenicLayer == 1] <- 1
    anthropogenicLayer <- backgroundWithoutWater
  } else {
    message(crayon::red("anthropogenicLayer is NULL. The prediction will assume anthropogenic disturbances do not exist"))
    anthropogenicLayer <- rasterToMatch
    anthropogenicLayer[anthropogenicLayer == 1] <- 0
    }
  
  # Extract the caribou shapefile values by fasterizing it. Way faster than raster::extract
    caribouShapefile <- reproducible::postProcess(x = caribouShapefile, rasterToMatch = ageMap,
                                                    destinationPath = tempdir(), filename2 = NULL)
    caribouShapefileSF <- st_as_sf(caribouShapefile)
    caribouShapefileSF$ID <- as.numeric(seq(1:length(caribouShapefileSF$NAME)))
    caribouShapefileRas <- fasterize::fasterize(sf = caribouShapefileSF, raster = ageMap, field = "ID")
    
    # Extract Fire
    listExtr <- lapply(X = caribouShapefileSF$ID, FUN = function(pol){
      # For fire:
      # From the age map calculate for each polygon the total amount of pixels that had fires
      # over the total number of pixels "available" to burn (non-NA, as NA can be cities/water/etc).
      # Then multiply by 100 to work with %.
      # We are calculating percent disturbance ONLY for those pixels that are forest/within BCR 6 NWT
          percentFire <- calculateDisturbance(ras = ageMap,
                                              rule = "<= recoveryTime", # Need to be a character string of the rule
                                              pol = pol,
                                              shp = caribouShapefileRas,
                                              recoveryTime = recoveryTime)
          if (percentFire$percentDisturbance < 0 | percentFire$percentDisturbance > 100)  # Data sanity check
            stop("Something went wrong with the fire distubance calculation. Value is either negative or above 100%. Please debug.")
      # ~~~~~~~~~~~~~~~~~~~
      # For anthropogenic:
      # From the anthropo layer we calculate for each polygon the total amount of pixels that had disturbances
      # over the total number of pixels "available" to have it (non-NA when na is JUST WATER).
      # Then multiply by 100 to work with %.
          percentAnthopo <- calculateDisturbance(ras = anthropogenicLayer,
                                                    rule = "> 0", # Need to be a character string of the rule
                                                    pol = pol,
                                                    shp = caribouShapefileRas)
          if (percentAnthopo$percentDisturbance < 0 | percentAnthopo$percentDisturbance > 100)  # Data sanity check
            stop("Something went wrong with the anthropogenic distubance calculation. Value is either negative or above 100%. Please debug.")
      # ~~~~~~~~~~~~~~~~~~~
      # For total disturbance: 
      # Need to overlay the disturbances and extract the total
          isDistrubance <- percentFire$isDisturbance | percentAnthopo$isDisturbance
          totPixelsNotNADist <- max(percentFire$totPixelsNotNA, percentAnthopo$totPixelsNotNA) # Total number of pixels that 
          # can have any type of disturbance, so need to maximize (i.e. I might not be able to have fire, but 
          # I sure could have anthropogenic)
          cummDist <- sum(isDistrubance, na.rm = TRUE)
          percentCumm <- 100*(cummDist/totPixelsNotNADist)
          if (percentCumm < 0 | percentCumm > 100)  # Data sanity check
            stop("Something went wrong with the total distubance calculation. Value is either negative or above 100%. Please debug.")
      # Making the data.frame
          df <- data.frame(DH_Fire = percentFire$percentDisturbance, DH_Anthro = percentAnthopo$percentDisturbance, DH_Total = percentCumm)
          return(df)
    })
    #Naming both fire and anthro disturbances
browser()
    nm <- if (!is.null(caribouShapefile$NAME)) "NAME" else "Name"
    names(listExtr) <- caribouShapefile[[nm]]
  return(listExtr)
}