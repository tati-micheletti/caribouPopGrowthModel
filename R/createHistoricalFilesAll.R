createHistoricalFilesAll <- function(url,
                                     archive,
                                     targetFile,
                                     studyArea,
                                     destinationPath){
  
  historicalFiresAllPath <- file.path(destinationPath, "historicalFiresAll.shp")
  if (any(rerun, 
          !file.exists(historicalFiresAllPath))){
    historicalFires <- reproducible::prepInputs(url = url,
                                                archive = archive,
                                                targetFile = targetFile,
                                                studyArea = studyArea,
                                                fun = "sf::sf", 
                                                destinationPath = destinationPath)
    # simplifying
    historicalFiresS <- historicalFires[, names(historicalFires) %in% c("YEAR", "DECADE")]
    historicalFiresDT <- data.table::data.table(sf::st_as_sf(historicalFiresS[, c("YEAR", "DECADE")]))
    historicalFiresDT[, geometry := NULL]
    historicalFiresDT[, decadeYear := 5 + (as.numeric(unlist(lapply(strsplit(historicalFiresDT$DECADE, split = "-"), `[[`, 1))))]
    historicalFiresDT[, fireYear := ifelse(YEAR == -9999, decadeYear, YEAR)]
    historicalFiresS$fireYear <- historicalFiresDT$fireYear
    historicalFires <- historicalFiresS[, "fireYear"]
    historicalFiresAll <- projectInputs(historicalFires, targetCRS = as.character(raster::crs(studyArea)))
    browser() # I think I can write with terra??
    rgdal::writeOGR(obj = historicalFiresAll, dsn = destinationPath, 
                    layer = basename(tools::file_path_sans_ext(historicalFiresAllPath)), 
                    driver = "ESRI Shapefile")
  } else {
    historicalFiresAll <- raster::shapefile(historicalFiresAllPath)
  }
}