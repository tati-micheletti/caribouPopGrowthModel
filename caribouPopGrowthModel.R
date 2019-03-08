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
    defineParameter("meanFire", "numeric", 30.75, NA, NA, "Mean cummulative fire from ECCC Scientific report 2011"),
    defineParameter("sdFire", "numeric", 10.6, NA, NA, "SD cummulative fire from ECCC Scientific report 2011"),
    defineParameter(".plotInitialTime", "numeric", start(sim) + 1, NA, NA, "inital plot time"),
    defineParameter(".plotTimeInterval", "numeric", 1, NA, NA, "Interval of plotting time"),
    defineParameter(".useDummyData", "logical", FALSE, NA, NA, "Should use dummy data? Automatically set"),
    defineParameter("recoveryTime", "numeric", 40, NA, NA, "Time to recover the forest enough for caribou")
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
    expectsInput(objectName = "currentPop", objectClass = "numeric", 
                 desc = "Caribou population size in the study area. Is updated every time step",
                 sourceURL = NA),
    expectsInput(objectName = "adultFemaleSurv", objectClass = "numeric", 
                 desc = "Caribou female survival probability in the study area. Default of 0.85",
                 sourceURL = NA),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer",
                 desc = paste0("Map of groups of pixels that share the same info from cohortData (sp, age, biomass, etc).",
                               " Should at some point be genrated by the fire model (i.e. scfmSpread)")),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = paste0("data.table with information by pixel group of sp, age, biomass, etc"))
  ), 
  outputObjects = bind_rows(
    createsOutput(objectName = "predictedCaribou", objectClass = "list", 
                  desc = "Data.table that contains the total population size per year, as well as other parameters"),
    createsOutput(objectName = "currentPop", objectClass = "numeric", 
                  desc = "Caribou population size in the study area. Is updated every time step"),
    createsOutput(objectName = "plotCaribou", objectClass = "ggplot2", 
                  desc = "Caribou population size through time"),
    createsOutput(objectName = "DH_Tot", objectClass = "numeric", 
                  desc = paste0("DH_Tot is the total disturbance in the area in percentage.",
                                " If not provided, it is created from sim$rstCurrentBurn coming from scfmSpread"))
  )
))

doEvent.caribouPopGrowthModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      if (any(!suppliedElsewhere("cohortData", sim = sim, where = "sim"), !suppliedElsewhere("pixelGroupMap", sim))){
        params(sim)$.useDummyData <- TRUE
        message(crayon::red(paste0("Disturbance total information (pixelGroupMap & cohortData) was not found.", 
                                   "\nGenerating DUMMY DATA to test the module.")))
      }
      
      sim$predictedCaribou <- list()

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "growingCaribou")
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "updatingPopulationSize")
      sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "plot") 
    },
    growingCaribou = {
      if (params(sim)$.useDummyData == TRUE){
        out <- summary(sim$caribouModels$M3)
        if (is.null(sim$DH_Tot)){
          sim$DH_Tot <- list(Year0 = data.frame(DH_Tot = rnorm(n = 1, mean = P(sim)$meanFire, sd = P(sim)$sdFire)))
        } else {
          trunc_rnorm <- function(n, mean = 0, sd = 1, lb = 0){ 
            lb <- pnorm(lb, mean, sd) 
            qnorm(runif(n, lb, 1), mean, sd)
          }
          sim$DH_Tot[[paste0("Year", time(sim))]] <- data.frame(DH_Tot = sim$DH_Tot[[paste0("Year", time(sim)-1)]] +
                                                                              (sim$DH_Tot[[paste0("Year", time(sim)-1)]]*
                                                                              trunc_rnorm(n = 1, mean = 0.1, sd = 0.05, lb = 0)))
        }
      } else {
        if (is.null(sim$DH_Tot)){
          sim$DH_Tot <- list()
        }
        
        sim$DH_Tot <- getDisturbance(currentTime = time(sim),
                                     startTime = start(sim),
                                     endTime = end(sim),
                                     cohortData = sim$cohortData, # Has age info per pixel group
                                     pixelGroupMap = sim$pixelGroupMap,
                                     recoveryTime = P(sim)$recoveryTime)
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

        sim$plotCaribou <- plotCaribou(startTime = start(sim),
                                       currentTime = time(sim),
                                       predictedCaribou = sim$predictedCaribou)
        
      plot(sim$plotCaribou, title = "Caribou population dynamics", new = TRUE, 
           ylim = c(0, max(sim$plotCaribou$data$CaribouPopulationSize) + 10))
      
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
                                                  targetFile = "caribouModels.rds", 
                                                  destinationPath = dataPath(sim), 
                                                  fun = "base::readRDS", 
                                                  omitArgs = c("destinationPath"))
  }
  if (!suppliedElsewhere("currentPop", sim)){
    message(crayon::yellow(paste0("Initial population size not provided.", 
                               "\nGenerating DUMMY DATA to test the module (n = 200).")))
    sim$currentPop <- 6731 # most up to date estimate of caribou (NWT gov 2019, pers comm. with Jame Hodgson)
      
  }
  if (!suppliedElsewhere("adultFemaleSurv", sim)){
    message(crayon::yellow(paste0("No LPU specific values for the female survival is available for NWT.", 
                               "\nUsing national ECCC value of 0.85.")))
    sim$adultFemaleSurv <- 0.85
  }

  return(invisible(sim))
}