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
    defineParameter("recoveryTime", "numeric", 40, NA, NA, "Time to recover the forest enough for caribou"),
    defineParameter("popModel", "character", "annualLambda", NA, NA, paste0("Which population model to use? Options", 
                                                                            "are in the file popModels.R in the R folder", 
                                                                            " Default is the simplest lamdba model")),
    defineParameter(name = "baseLayer", class = "character", default = 2005, min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05 or LCC10?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from anthropogenic layer",
                 sourceURL = NA),
    expectsInput(objectName = "caribouArea1", objectClass = "SpatialPolygonsDataFrame",
                 desc = "Study area to predict caribou population to (NWT_Regions_2015_LCs_DC_SS)",
                 sourceURL = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO"),
    expectsInput(objectName = "caribouArea2", objectClass = "SpatialPolygonsDataFrame",
                 desc = "Study area to predict caribou population to (NT1_BOCA_spatial_units_for_landscape)",
                 sourceURL = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV"),
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
                 desc = paste0("data.table with information by pixel group of sp, age, biomass, etc")),
    expectsInput(objectName = "anthropogenicLayer", objectClass = "RasterLayer",
                 desc = paste0("Layer that maps the % of anthropogenic disturbance of in each pixel.", 
                               "This layer is static if no modules are forecasting anthropogenic disturbances")),
    expectsInput(objectName = "modelsToUse", objectClass = "character", 
                 desc = "Which models from ECCC to be used?", 
                 sourceURL = NA),
    expectsInput(objectName = "caribouData", objectClass = "data.table", 
                 desc = "Data containing recruitment and other pop covariates", 
                 sourceURL = "https://drive.google.com/open?id=1SOimSD2jehRxV-SbMmgLUh3W5yStwhdq"),
    expectsInput(objectName = "provinces", objectClass = "character", 
                 desc = "Which province caribou data should be used for the module?"),
    expectsInput(objectName = "caribouCoefTable", objectClass = "data.table", 
                 desc = "Published caribou coefficients", 
                 sourceURL = "https://drive.google.com/open?id=14ck35G8A3A6s65vSAWWeUp2_vXgmYZe5")
  ), 
  outputObjects = bind_rows(
    createsOutput(objectName = "caribouModels", objectClass = "list", 
                 desc = "List with model equations. Default is M3 (ECCC 2011, Table 56) downloaded if needed."),
    createsOutput(objectName = "predictedCaribou", objectClass = "list", 
                  desc = "Data.table that contains the total population size per year, as well as other parameters"),
    createsOutput(objectName = "currentPop", objectClass = "numeric", 
                  desc = "Caribou population size in the study area. Is updated every time step"),
    createsOutput(objectName = "plotCaribou", objectClass = "ggplot2", 
                  desc = "Caribou population size through time"),
    createsOutput(objectName = "disturbances", objectClass = "list", 
                  desc = paste0("disturbances is a list of shapefiles that have polygons with ", 
                                "the total disturbance in the area in percentage.",
                                " If not provided, it is created from sim$rstCurrentBurn", 
                                " coming from scfmSpread")),
    createsOutput(objectName = "listSACaribou", objectClass = "list", 
                  desc = paste0("List of caribou areas to prodict for",
                                " Currently only takes 2 shapefiles"))
  )
))

doEvent.caribouPopGrowthModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      sim$listSACaribou = list(sim$caribouArea1, sim$caribouArea2)
      names(sim$listSACaribou) <- c("caribouArea1", "caribouArea2")
      sim$predictedCaribou <- list()

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "makingModel")
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "gettingData")
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "growingCaribou")
      sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "plot", eventPriority = .last())
      if (P(sim)$popModel != "annualLambda")
        sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "updatingPopulationSize")
    },
    makingModel = {
      
      sim$caribouModels <- createModels(caribouCoefTable = sim$caribouCoefTable, 
                                        modelsToUse = sim$modelsToUse)
    },
    gettingData = {
      Require("magrittr")
      if (!suppliedElsewhere(object = "cohortData", sim = sim)){
        message(crayon::yellow(paste0("cohortData not supplied by another module.", 
                                      " Will try using files in inputPath(sim) or create dummy data")))
        mod$cohortDataName <- grepMulti(x = list.files(inputPath(sim), 
                                               recursive = TRUE), 
                                patterns = c("cohortData", time(sim)))
        if (length(mod$cohortDataName) == 0){
          mod$cohortData <- NULL
        } else {
          mod$cohortData <- readRDS(file.path(inputPath(sim), mod$cohortDataName))
        }
        if (!is.null(mod$cohortData)) message(paste0("cohortData loaded from " , 
                                                          crayon::magenta(file.path(inputPath(sim), mod$cohortDataName)),
                                                          " for year ", time(sim)))
      }
      
      if (!suppliedElsewhere(object = "pixelGroupMap", sim = sim)){
        message(crayon::yellow(paste0("pixelGroupMap not supplied by another module." , 
                                      " Will try using files in inputPath(sim) or create dummy data")))
        mod$pixelGroupMapName <- grepMulti(x = list.files(inputPath(sim), 
                                               recursive = TRUE), 
                                patterns = c("pixelGroupMap", time(sim)))
        if (length(mod$pixelGroupMapName) == 0) {
          mod$pixelGroupMap <- NULL
        } else {
          mod$pixelGroupMap <- readRDS(file.path(inputPath(sim), mod$pixelGroupMapName))
        }
        if (!is.null(mod$pixelGroupMap)) message(paste0("pixelGroupMap loaded from ", 
                                                        crayon::magenta(file.path(inputPath(sim), mod$cohortDataName)), 
                                                        " for year ", time(sim)))
      }
      
      if (any(is.null(mod$pixelGroupMap), is.null(mod$cohortData))) {
      params(sim)$.useDummyData <- TRUE
      }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + 1, "caribouPopGrowthModel", "gettingData")
      
    },
    growingCaribou = {
      
      if (params(sim)$.useDummyData == TRUE){
        message(crayon::red(paste0("Disturbance total information (pixelGroupMap & cohortData) was not found.", 
                                   "\nGenerating DUMMY DATA to test the module.")))
        if (is.null(sim$disturbances)){
          sim$disturbances <- list(Year0 = data.frame(disturbances = rnorm(n = 1, mean = P(sim)$meanFire, sd = P(sim)$sdFire)))
        } else {
          
          sim$disturbances[[paste0("Year", time(sim))]] <- sim$disturbances[[paste0("Year", time(sim) - 1)]]
        }
      } else {
        if (is.null(sim$disturbances)){
          sim$disturbances <- list()
        }
        cohortData <- if (!is.null(sim$cohortData)) sim$cohortData else mod$cohortData
        pixelGroupMap <- if (!is.null(sim$pixelGroupMap)) sim$pixelGroupMap else mod$pixelGroupMap

        sim$disturbances <- getDisturbance(currentTime = time(sim),
                                     startTime = start(sim),
                                     endTime = end(sim),
                                     cohortData = cohortData, # Has age info per pixel group
                                     pixelGroupMap = pixelGroupMap,
                                     recoveryTime = P(sim)$recoveryTime,
                                     listSACaribou = sim$listSACaribou,
                                     anthropogenicLayer = sim$anthropogenicLayer,
                                     waterRaster = sim$waterRaster)
      }

      sim$predictedCaribou[[paste0("Year", time(sim))]] <- popGrowthModel(caribouModels = sim$caribouModels,
                                             disturbances = sim$disturbances,
                                             currentPop = sim$currentPop,
                                             currentTime = time(sim),
                                             startTime = start(sim),
                                             adultFemaleSurv = sim$adultFemaleSurv,
                                             popModel = P(sim)$popModel,
                                             listSACaribou = sim$listSACaribou)
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
                                       endTime = end(sim),
                                       predictedCaribou = sim$predictedCaribou)
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
  
  cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
  
  # if (!suppliedElsewhere("caribouData", sim)){ # Think I am not using this...
  #   sim$caribouData <- Cache(prepInputs, targetFile = "CaribouPopData_2008.csv",
  #                                 url = extractURL(objectName = "caribouData", module = "caribouRecDistRelationship"),
                                  # destinationPath = dataPath(sim), fun = "data.table::fread",
  #                            omitArgs = "destinationPath")
  # }
  if (!suppliedElsewhere("provinces", sim)){
    sim$provinces <- "NWT"
  }
  if (!suppliedElsewhere("caribouCoefTable", sim)){
    sim$caribouCoefTable <- prepInputs(targetFile = "caribouCoefTable.csv", url = extractURL("caribouCoefTable"),
                                       destinationPath = dataPath(sim), fun = "data.table::fread", 
                                       omitArgs = "destinationPath")
  }
  if (!suppliedElsewhere("modelsToUse", sim)){
    sim$modelsToUse <- "M3"
    message(paste0("No models were specified. Running simulation using model ", crayon::magenta("M3"), 
                   " from ECCC 2011 report (Table 56)"))
  }
    
  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- cloudCache(prepInputs,
                                url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
                                destinationPath = dataPath(sim),
                                cloudFolderID = sim$cloudFolderID,
                                omitArgs = c("destinationPath", "cloudFolderID"))
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
    sim$rasterToMatch <- cloudCache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df", 
                                    studyArea = sim$studyArea,
                                    targetFile = "RTM.tif", destinationPath = dataPath(sim), 
                                    useCloud = getOption("reproducible.useCloud", FALSE),
                                    cloudFolderID = sim$cloudFolderID, overwrite = TRUE, filename2 = NULL,
                                    omitArgs = c("destinationPath", "cloudFolderID", "useCloud", "overwrite", "filename2"))
  }
  
  if (!suppliedElsewhere(object = "caribouArea2", sim = sim)){
    sim$caribouArea2 <- cloudCache(prepInputs,
                                url = extractURL("caribouArea2"),
                                destinationPath = dataPath(sim), filename2 = "caribouArea2",
                                useCloud = TRUE, cloudFolderID = cloudFolderID)
  }
  if (!suppliedElsewhere("caribouArea1", sim)){
    sim$caribouArea1 <- cloudCache(prepInputs,
                                       url = extractURL("caribouArea1"), studyArea = sim$studyArea,
                                       destinationPath = dataPath(sim), filename2 = "caribouArea1",
                                       rasterToMatch = sim$rasterToMatch,
                                       useCloud = TRUE, cloudFolderID = cloudFolderID)
  }

  if (!suppliedElsewhere("currentPop", sim) &
      P(sim)$popModel != "annualLambda"){
    message(crayon::yellow(paste0("Initial population size not provided.", 
                               "\nGenerating a mean population size for the studyArea of Edehzhie (n = 353).")))
    sim$currentPop <- 353 # [ FIX ] should pass a file that is a list of population sizes for each one of the units/LPU for each studyArea shp 
      
  }
  if (!suppliedElsewhere("adultFemaleSurv", sim)){
    message(crayon::yellow(paste0("No LPU specific values for the female survival is available for NWT.", 
                               "\nUsing national ECCC value of 0.85.")))
    sim$adultFemaleSurv <- 0.85
  }
  
  if (!suppliedElsewhere("waterRaster", sim)){
  wetlandRaster <- Cache(prepInputsLayers_DUCKS, destinationPath = dataPath(sim), 
                               studyArea = sim$studyArea, 
                               userTags = "objectName:wetlandRaster")
  sim$waterRaster <- Cache(classifyWetlands, LCC = P(sim)$baseLayer,
                             wetLayerInput = wetlandRaster,
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea,
                             userTags = c("objectName:wetLCC"))
  waterVals <- raster::getValues(sim$waterRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
  waterVals[waterVals == 1] <- NA
  waterVals[waterVals > 1] <- 1
  sim$waterRaster <- raster::setValues(sim$waterRaster, waterVals)
  }
  # This specific one is for RSF, not growth model if just being used here as a test for when the right one is done. When the 500m is done, just substitute
  # the url and the name!
  if (!suppliedElsewhere("anthropogenicLayer", sim)){
  sim$anthropogenicLayer <- prepInputs(targetFile = "lineDensityMap_BCR6_NWT_Roads_v0.2.3_FFT_radius10000_t0.tif",
                                       url = "https://drive.google.com/open?id=11Fr-Mq3rPHppjRqJ868aBDEBCHT7aqXb",
                                       destinationPath = dataPath(sim), studyArea = sim$studyArea,
                                       overwrite = TRUE, 
                                       rasterToMatch = sim$rasterToMatch)
  }

  return(invisible(sim))
}