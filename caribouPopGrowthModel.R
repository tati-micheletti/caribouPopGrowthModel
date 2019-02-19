defineModule(sim, list(
  name = "caribouPopGrowthModel",
  description = paste0("Module to simulate Caribou population growth, based", 
                       " on lambda using published ECCC data"),
  keywords = c("Caribou", "population", "lambda"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Frances", "Stewart", email = "frances.stewart@canada.ca", role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "Eliot.McIntire@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5", caribouPopGrowthModel = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "caribouPopGrowthModel.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame", 
                 desc = "Study area to predict caribou population to", 
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"),
    expectsInput(objectName = "caribouStudyArea", objectClass = "SpatialPolygonsDataFrame", 
                 desc = "Shapefile containing the spatial description of the caribout study areas", 
                 sourceURL = "https://drive.google.com/open?id=1ZaFsXPRsGOj69nyVv3SDNyT7qTHkX2kU")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "predictedCaribou", objectClass = "data.table", 
                  desc = "Contains the total population size per year, as well as other parameters")
  )
))

doEvent.caribouPopGrowthModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "growingCaribous")
    },
    growingCaribous = {
      browser()
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- cloudCache(prepInputs, 
                                url = extractURL("studyArea"),
                                destinationPath = dataPath(sim), 
                                useCloud = TRUE, cloudFolderID = cloudFolderID)
  }
  if (!suppliedElsewhere("caribouStudyArea", sim)){
    sim$caribouStudyArea <- cloudCache(prepInputs,
                                       url = extractURL("caribouStudyArea"),
                                       destinationPath = dataPath(sim), 
                                       useCloud = TRUE, cloudFolderID = cloudFolderID)
  }
  if (!suppliedElsewhere("caribouData", sim)){
    sim$caribouData <- cloudCache(prepInputs, targetFile = "CaribouPopData_2008.csv",
                                  url = extractURL("caribouData"),
                                  destinationPath = dataPath(sim), fun = "data.table::fread",
                                  useCloud = TRUE, cloudFolderID = cloudFolderID)
  }
  return(invisible(sim))
}