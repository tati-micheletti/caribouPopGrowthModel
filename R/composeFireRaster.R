composeFireRaster <- function(currentTime,
                              pathData, # To compare to current time. First time needs
                              # to be different as we are creating layers, not updating them
                              historicalFires,
                              studyArea,
                              rasterToMatch,
                              recoveryTime,
                              thisYearsFires){

  firesFilenameRas <- file.path(pathData, paste0("historicalFireRaster_",
                                                 1+(currentTime-recoveryTime),"to",
                                                 currentTime, "_",
                                                 ncell(rasterToMatch),".tif"))
  firesFilenameList <- file.path(pathData, paste0("historicalFireList","_",
                                 ncell(rasterToMatch), ".qs"))

  minYear <- 1+(currentTime - recoveryTime)

  if (!file.exists(firesFilenameRas)){
    if (!file.exists(firesFilenameList)){
      fireRas <- rasterToMatch
      fireRas[!is.na(fireRas)] <- 0
      tsRas <- lapply(unique(historicalFires$fireYear), function(YYYY){
        message(paste0("Fires for year ", YYYY, " being processed..."))
        subst <- historicalFires[historicalFires$fireYear == YYYY, ]
        if (!length(subst) == 0){
          substSF <- sf::st_as_sf(subst)
          yearFire <- suppressWarnings(fasterize::fasterize(sf = substSF,
                                                            raster = rasterToMatch,
                                                            field = "fireYear"))
          fireRas[yearFire == YYYY] <- YYYY
        }
        return(fireRas)
      })

      names(tsRas) <- paste0("Year", unique(historicalFires$fireYear))
      tsRas <- lapply(names(tsRas), function(r){
        ras <- tsRas[[r]]
        names(ras) <- paste0("historicalFires", r)
        return(ras)
      })

      names(tsRas) <- paste0("Year", unique(historicalFires$fireYear))
      qs::qsave(tsRas, file = firesFilenameList)
    } else {
      tsRas <- qs::qread(firesFilenameList)
    }

    # This is the first year. I need to create one historical raster of burns with years as counters
    # Place the maximum year of the list in the pixel

    # 1. Exclude the years that are more than the current if any (i.e. if sim starts in 2011)
    # it will exclude the years that burned after that, otherwise we might be overestimating
    # fire later on
    minCurrTime <- min(currentTime, max(unique(historicalFires$fireYear)))
    if (minYear > minCurrTime){
      minYearLapply <- minCurrTime
    } else {
      minYearLapply <- minYear
    }

    subThisYears <- raster::stack(lapply(minYearLapply:minCurrTime, function(Y){
      yN <- grep(Y, names(tsRas))
      if (length(yN) == 0){ # No fires in this specific year, layer doesn't exist
        y <- tsRas[[1]] # template raster
        y[!is.na(y)] <- 0 # Make a zeroed fire year layer
      } else {
        y <- tsRas[[yN]]
        y[y > 0] <- Y
      }
      names(y) <- paste0("Year", Y)
      return(y)
    }))

    if (minYearLapply == minYear){
      counterRaster <- raster::calc(subThisYears, fun = max, na.rm = TRUE)
    } else {
      counterRaster <- unstack(subThisYears)[[1]]
    }
    names(counterRaster) <- tools::file_path_sans_ext(basename(firesFilenameRas))
    writeRaster(counterRaster, firesFilenameRas, format = "GTiff")
  } else {
    counterRaster <- raster::raster(firesFilenameRas)
    counterRaster[] <- counterRaster[]
  }

  # 1. Determine which years are in "thisYearsFires"
  maxYearFromData <- maxValue(counterRaster)
  # If the max of the counter is not the current year, we need data from simulations
  if (maxYearFromData < currentTime){
    minYear <- max(minYear, (maxYearFromData+1))
    subThisYears <- raster::stack(lapply(minYear:currentTime, function(Y){
      y <- thisYearsFires[[grep(Y, names(thisYearsFires))]]
      y[y > 0] <- Y
      names(y) <- paste0("Year", Y)
      return(y)
    }))
    # Add the new years
    counterRaster <- raster::calc(raster::stack(counterRaster,
                                                subThisYears), fun = max,
                                  na.rm = TRUE)
  }
  # Mask counter Raster to RTM map to avoid overestimating caribou later on
  counterRasterMasked <- maskInputs(counterRaster,
                                    studyArea = studyArea,
                               destinationPath = pathData,
                               rasterToMatch = rasterToMatch,
                              maskWithRTM = TRUE)

  return(counterRasterMasked)
}
