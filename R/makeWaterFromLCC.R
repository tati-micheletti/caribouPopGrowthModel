makeWaterFromLCC <- function(year = 2019,
                             destinationPath = asPath("."),
                             method = "ngb",
                             studyArea,
                             rasterToMatch,
                             whichMethod = "LCC"){
  
  if (whichMethod == "LCC"){
    urlLCC <- paste0("https://opendata.nfis.org/downloads/",
           "forest_change/CA_forest_VLCE2_", year,
           ".zip")
    # https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_XXXX.zip

    landcoverMap <- Cache(reproducible::prepInputs, url = urlLCC,
                                             targetFile = paste0("CA_forest_VLCE2_", year,".tif"),
                                             destinationPath = destinationPath,
                                             studyArea = studyArea,
                                             rasterToMatch = rasterToMatch,
                                             method = "ngb",
                                             userTags = "objectName:landcoverMap")
    landcoverMap[!is.na(landcoverMap[]) &
               landcoverMap[] != 20] <- NA
    landcoverMap[!is.na(landcoverMap[])] <- 1
    terra::plot(landcoverMap); terra::plot(studyArea, add = TRUE, border = 'red')
    return(landcoverMap)
  } else {
    if (whichMethod == "EOSD"){
      print("Method untested. Use at your own risk.")
      caribouLCC <- Cache(prepInputs, targetFile = "EOSD_NT1_BCR6_JAN21.tif",
                          archive = "EOSD_NT1_BCR6_JAN21.zip",
                          alsoExtract = "similar",
                          url = "https://drive.google.com/file/d/11VEo4dhI0uFcWTlmm41bMFGkiE8wKpKT",
                          studyArea = studyAreaCaribou,
                          rasterToMatch = rasterToMatch,
                          destinationPath = Paths$inputPath,
                          fun = "raster::raster",
                          userTags = c("outFun:Cache", "step:prepEOSDcaribou"))
      
      # As the caribou RSF doesn't have broadleaf sparse (223) and mixedwood sparse (233), we should reclassify
      # these to to broadleaf open (222) and mixedwood open (232), respectively. This was done in the same
      # way as GNWT (J. Hodson) did to update RSF maps. 
      caribouLCC[caribouLCC == 223] <- 222 # originally 667 pixels
      caribouLCC[caribouLCC == 233] <- 232 # originally 2517 pixels
      caribouLCC[caribouLCC == 255] <- NA # originally 2 pixels. It is not even in the metadata. Go figure!
      
      # New EOSD Water, Uplands and Lowlands derived
      WaterClass <- c(20, 31)
      watersVals <- raster::getValues(caribouLCC)
      # Fix missing 1's in Mackenzie River + Slave Lake
      rtmVals <- getValues(rasterToMatch)
      watersVals[is.na(watersVals) & rtmVals == 1] <- 20
      
      # Water
      watersValsToChange <- watersVals 
      watersValsToChange[!is.na(watersValsToChange) & !(watersValsToChange %in% WaterClass)] <- NA
      watersValsToChange[!is.na(watersValsToChange)] <- 1
      waterRaster <- raster::setValues(x = raster(caribouLCC), watersValsToChange)
      return(waterRaster)
    } else 
      if (whichMethod == "DUCKS") {
        print("Method untested. Use at your own risk.")
        ###  Prep Layers: Exclude water, rocks and ice from flammableRTM --> NA
        watersRaster <- Cache(prepInputs, url = "https://drive.google.com/open?id=1nPd03gaVXkkaHorirR4UhYrDi95ZgyJM",
                              destinationPath = Paths$inputPath,
                              studyArea = studyArea,
                              rasterToMatch = rasterToMatch,
                              userTags = c("objectName:watersRaster", "outFun:Cache"))
        watersVals <- raster::getValues(watersRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
        # Fix missing 1's in Mackenzie River + Slave Lake
        rtmVals <- getValues(rasterToMatch)
        watersVals[is.na(watersVals) & rtmVals == 1] <- 1
        watersValsToChange <- watersVals
        watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 1] <- NA
        waterRaster <- raster::setValues(x = watersRaster, watersValsToChange)
        return(waterRaster)
      } else 
        stop(paste0("Please provide whichMethod. It can be one of the following:",
                    "'LCC', 'EOSD' or 'DUCKS'. LCC is the prefered method."))
  }
}
