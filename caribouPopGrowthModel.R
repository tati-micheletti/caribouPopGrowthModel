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
  reqdPkgs = list("data.table", "ggplot2"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter(".plotInitialTime", "numeric", start(sim) + 1, NA, NA, "inital plot time"),
    defineParameter(".plotTimeInterval", "numeric", 1, NA, NA, "Interval of plotting time"),
    defineParameter(".useDummyData", "logical", FALSE, NA, NA, "Should use dummy data? Automatically set")
  ),
  inputObjects = bind_rows(
    # expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame", 
    #              desc = "Study area to predict caribou population to", 
    #              sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"),
    # expectsInput(objectName = "caribouStudyArea", objectClass = "SpatialPolygonsDataFrame", 
    #              desc = "Shapefile containing the spatial description of the caribout study areas", 
    #              sourceURL = "https://drive.google.com/open?id=1ZaFsXPRsGOj69nyVv3SDNyT7qTHkX2kU"),
    expectsInput(objectName = "caribouModels", objectClass = "list", 
                 desc = "List with model objects. Default is M3 (ECCC 2011, Table 56) downloaded if needed.", 
                 sourceURL = "https://drive.google.com/open?id=1jWOr5ZyVRobw8Wo_9z2UnbI48IufuyJc"),
    expectsInput(objectName = "DH_Tot", objectClass = "data.table", 
                 desc = "Table with years and total disturbance per year to be predicted", # MAYBE MAKE SEVERAL REPLICATIONS, TAKE THE MEAN?
                 sourceURL = ""), # prepInput it once done
    expectsInput(objectName = "currentPop", objectClass = "numeric", 
                 desc = "Caribou population size in the study area. Is updated every time step",
                 sourceURL = NA),
    expectsInput(objectName = "adultFemaleSurv", objectClass = "numeric", 
                 desc = "Caribou female survival probability in the study area. Default of 0.85",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "predictedCaribou", objectClass = "list", 
                  desc = "Data.table that contains the total population size per year, as well as other parameters"),
    createsOutput(objectName = "currentPop", objectClass = "numeric", 
                  desc = "Caribou population size in the study area. Is updated every time step"),
    createsOutput(objectName = "plotCaribou", objectClass = "ggplot2", 
                  desc = "Caribou population size through time")
  )
))

doEvent.caribouPopGrowthModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      sim$predictedCaribou <- list()
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "growingCaribou")
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "updatingPopulationSize")
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "caribouPopGrowthModel", "plot") 
    },
    growingCaribou = {

      if (params(sim)$.useDummyData == TRUE){
        out <- summary(sim$caribouModels$M3)
        sim$DH_Tot <- data.table::data.table(DH_Tot = rnorm(n = 1, 
                                                            mean = abs(out$coefficients[2, "Estimate"]), 
                                                            sd = abs(out$coefficients[2, "Std. Error"])))
      }
      # Make sure this is being created every year based on dist map (fire)
      sim$predictedCaribou[[paste0("Year", time(sim))]] <- popGrowthModel(caribouModels = sim$caribouModels,
                                             DH_Tot = sim$DH_Tot,
                                             currentPop = sim$currentPop,
                                             currentTime = time(sim),
                                             startTime = start(sim),
                                             adultFemaleSurv = sim$adultFemaleSurv)
      # schedule future event(s)
        sim <- scheduleEvent(sim, time(sim) + 1, "caribouPopGrowthModel", "growingCaribou")
      
    },
    updatingPopulationSize = {
      
      sim$currentPop <- lapply(X = names(sim$caribouModels), FUN = function(model){
        sim$predictedCaribou[[paste0("Year", time(sim))]][[model]]$currentPopUpdated
      })
      names(sim$currentPop) <- names(sim$caribouModels)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + 1, "caribouPopGrowthModel", "updatingPopulationSize")
      
    },
    plot = {

        sim$plotCaribou <- plotCaribou(startTime = P(sim)$.plotInitialTime,
                                       currentTime = time(sim),
                                       predictedCaribou = sim$predictedCaribou)
        
        Plot(sim$plotCaribou, title = "Caribou population dynamics", new = TRUE)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotTimeInterval, "caribouPopGrowthModel", "plot")
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  params(sim)$.useDummyData <- FALSE
  
  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  # if (!suppliedElsewhere(object = "studyArea", sim = sim)){
  #   sim$studyArea <- cloudCache(prepInputs, 
  #                               url = extractURL("studyArea"),
  #                               destinationPath = dataPath(sim), 
  #                               useCloud = TRUE, cloudFolderID = cloudFolderID)
  # }
  # if (!suppliedElsewhere("caribouStudyArea", sim)){
  #   sim$caribouStudyArea <- cloudCache(prepInputs,
  #                                      url = extractURL("caribouStudyArea"),
  #                                      destinationPath = dataPath(sim), 
  #                                      useCloud = TRUE, cloudFolderID = cloudFolderID)
  # }
  # if (!suppliedElsewhere("caribouData", sim)){
  #   sim$caribouData <- cloudCache(prepInputs, targetFile = "CaribouPopData_2008.csv",
  #                                 url = extractURL("caribouData"),
  #                                 destinationPath = dataPath(sim), fun = "data.table::fread",
  #                                 useCloud = TRUE, cloudFolderID = cloudFolderID)
  if (!suppliedElsewhere("caribouModels", sim)){
    sim$caribouModels <- reproducible::prepInputs(url = extractURL("caribouModels"), 
                                                  destinationPath = dataPath(sim), 
                                                  fun = "base::readRDS", 
                                                  omitArgs = c("destinationPath"))
  }
  if (!suppliedElsewhere("currentPop", sim)){
    message(crayon::yellow(paste0("Initial population size not provided.", 
                               "\nGenerating DUMMY DATA to test the module (n = 200).")))
    sim$currentPop <- 200
      
  }
  if (!suppliedElsewhere("DH_Tot", sim)){
    message(crayon::red(paste0("Disturbance total information was not generated.", 
                   "\nGenerating DUMMY DATA to test the module.")))
    params(sim)$.useDummyData <- TRUE
  }
  if (!suppliedElsewhere("adultFemaleSurv", sim)){
    message(crayon::yellow(paste0("No LPU specific values for the female survival is available for NWT.", 
                               "\nUsing national ECCC value of 0.85.")))
    sim$adultFemaleSurv <- 0.85
  }

  return(invisible(sim))
}