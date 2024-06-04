createHistoricalFilesAll <- function(url,
                                     archive,
                                     targetFile,
                                     studyArea,
                                     destinationPath,
                                     rerun = FALSE){
  
  historicalFiresAllPath <- file.path(destinationPath, "historicalFiresAll.shp")
  if (any(rerun, 
          !file.exists(historicalFiresAllPath))){
    historicalFires <- reproducible::prepInputs(url = url,
                                                archive = archive,
                                                targetFile = targetFile,
                                                studyArea = studyArea,
                                                fun = "terra::vect", 
                                                destinationPath = destinationPath)
    # simplifying
    historicalFiresS <- historicalFires[, names(historicalFires) %in% c("YEAR", "DECADE")]
    historicalFiresDT <- data.table::data.table(sf::st_as_sf(historicalFiresS[, c("YEAR", "DECADE")]))
    historicalFiresDT[, geometry := NULL]
    historicalFiresDT[, decadeYear := 5 + (as.numeric(unlist(lapply(strsplit(historicalFiresDT$DECADE, split = "-"), `[[`, 1))))]
    historicalFiresDT[, fireYear := ifelse(YEAR == -9999, decadeYear, YEAR)]
    historicalFiresS$fireYear <- historicalFiresDT$fireYear
    historicalFires <- historicalFiresS[, "fireYear"]
    historicalFiresAll <- terra::project(historicalFires, terra::crs(studyArea))
    terra::writeVector(x = historicalFiresAll, filename = historicalFiresAllPath)
    return(historicalFiresAll)
  } else {
    historicalFiresAll <- terra::vect(historicalFiresAllPath)
    return(historicalFiresAll)
  }
}