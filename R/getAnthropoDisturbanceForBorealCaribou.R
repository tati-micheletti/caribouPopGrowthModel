# Create the polygons for anthropogenic disturbance.
# This function is needed because all layers are separated in the original
# file. So we need to recover ALL of these, collate, and then crop to SA

getAnthropoDisturbanceForBorealCaribou <- function(studyArea,
                                                   rasterToMatch,
                                                   pathData){

  fullAthropoCanada <- file.path(pathData, "completeAnthropogenicDisturbance_caribou.qs")
  if (!file.exists(fullAthropoCanada)){

    url <- "https://ec.gc.ca/data_donnees/STB-DGST/001/BorealCaribouDisturbanceShapefiles_Updated2012.zip"
    allAnthropoLayers <- preProcess(url = url,
                                    destinationPath = checkPath(file.path(pathData, "anthropogenicLayers"),
                                                                create = TRUE))
    # Find all files that represent the herds
    allFiles <- allAnthropoLayers[["checkSums"]][["actualFile"]]
    patt <- "_Anthropogenic_Disturbance_Footprint"
    filesToLoad <- grepMulti(patterns = c(patt, ".shp"), x = allFiles, unwanted = ".xml")

    # Loading all files to postprocess
    allShpFiles <- lapply(filesToLoad, function(shapePath){
      herdName <- strsplit(x = shapePath, split = "_")[[1]][1]
      oneOf <- which(filesToLoad == shapePath)
      print(paste0("Processing ", herdName, " (", oneOf, " of ", length(filesToLoad),")"))
      shp <- raster::shapefile(file.path(pathData, "anthropogenicLayers", shapePath))
      shp <- projectInputs(x = shp, targetCRS = crs(studyArea))
      shp[["herd"]] <- herdName
      return(shp)
    })
    # Use bind to combine all polys
    allHerds <- do.call(what = bind, allShpFiles)
    qs::qsave(x = allHerds, file = fullAthropoCanada)
  } else {
    allHerds <- qs::qread(fullAthropoCanada) # Just load the file if it exists
  }
  herdDisturbance <- postProcess(x = allHerds,
                                 studyArea = studyArea)
  # Convert polygons to raster
  bufferedAnthropogenicDisturbance500mSF <- sf::st_as_sf(herdDisturbance)
  bufferedAnthropogenicDisturbance500mSF$fieldSF <- 1
  bufferedAnthropogenicDisturbance500m <- fasterize::fasterize(sf = bufferedAnthropogenicDisturbance500mSF,
                                                               raster = rasterToMatch, field = "fieldSF",
                                                               background = 0)
  buffAnthroDist500m <- Cache(postProcess, x = bufferedAnthropogenicDisturbance500m,
                              destinationPath = pathData,
                              studyArea = studyArea,
                              rasterToMatch = rasterToMatch,
                              userTags = c("step:maskAnthropogenicDistLayer", "outFun:Cache"))
return(buffAnthroDist500m)
  }
