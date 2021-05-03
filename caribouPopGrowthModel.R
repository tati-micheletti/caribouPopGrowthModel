defineModule(sim, list(
  name = "caribouPopGrowthModel",
  description = paste0("Module to simulate Caribou population growth, based", 
                       " on lambda using published ECCC data or Johnson et al., 2020 models"),
  keywords = c("Caribou", "population", "lambda"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", 
                     role = c("aut", "cre")),
              person("Frances", "Stewart", email = "frances.stewart@canada.ca", 
                     role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "Eliot.McIntire@canada.ca", 
                     role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5", caribouPopGrowthModel = "0.2.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "caribouPopGrowthModel.Rmd"),
  reqdPkgs = list("data.table", "ggplot2", "sf", "tati-micheletti/usefulFuns", "tictoc",
                  "future", "future.apply", "PredictiveEcology/fireSenseUtils", "achubaty/amc"),
  parameters = rbind(
    defineParameter("predictLastYear", "logical", TRUE, NA, NA, 
                    paste0("Should it schedule events for the last year",
                           " of simulation if this is not a multiple of interval?")),
    defineParameter("polygonsOfInterest", "character", NULL, NA, NA, 
                    paste0("If results should be presented only for a set of polygons",
                           " these should be expressed here")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, 
                    "Should this entire module be run with caching activated?"),
    defineParameter("nBootstrap", "numeric", 100, NA, NA, 
                    "How many bootstrap replicates do we want for the coefficients?"),
    defineParameter(".plotInitialTime", "numeric", start(sim) + 1, NA, NA, 
                    "inital plot time"),
    defineParameter(".plotTimeInterval", "numeric", 1, NA, NA, 
                    "Interval of plotting time"),
    defineParameter(".useDummyData", "logical", FALSE, NA, NA, 
                    "Should use dummy data? Automatically set when data is not available"),
    defineParameter(name = "climateModel", class = "character",
                    default = NULL, min = NA, max = NA,
                    desc = paste0("For identifying the climate scenario")),
    defineParameter("recoveryTime", "numeric", 40, NA, NA, 
                    paste0("Time to recover the forest ",
                           "enough for caribou (ECCC 2011; Johnson et al. 2020)")),
    defineParameter("popModel", "character", "annualLambda", NA, NA, 
                    paste0("Which population model to use? Options",
                           "are in the file popModels.R in the R folder",
                           " Default is the simplest lamdba model")),
    defineParameter(name = ".growthInterval", class = "numeric", default = 1, 
                    min = NA, max = NA, 
                    desc = paste0("Interval of Population Growth. ",
                                  "The default value is 1 (yearly projections)")),
    defineParameter(name = "recruitmentModelVersion", class = "character",
                    default = "Johnson", min = NA, max = NA, 
                    desc = paste0("Two options available: ",
                                  "'Johnson' (Johnson et al., 2020) ",
                                  "'ECCC' (ECCC, 2011).")),
    defineParameter(name = "femaleSurvivalModelVersion", class = "character",
                    default = "Johnson", min = NA, max = NA, 
                    desc = paste0("Currently, only this option is available",
                                  "'Johnson' (Johnson et al., 2020) ")),
    defineParameter(name = "useFuture", class = "logic",
                    default = TRUE, min = NA, max = NA, 
                    desc = paste0("If useFuture, parallel processing will be",
                                  "tried based on available cores. If running in ",
                                  "RStudio, this will fall back to sequential as ",
                                  "future does NOT yet supports 'multicore' plan in",
                                  " RStudio. If parallelizing is important, please ",
                                  "consider running this module in R Gui")),
    # defineParameter(name = "Type", class = "character", # DEPRECATED
    #                 default = "National", min = NA, max = NA, 
    #                 desc = paste0("Only option available for now")),
    defineParameter(name = "femaleSurvivalModelNumber", class = "character", 
                    default = "M1", min = NA, max = NA, 
                    desc = paste0("Which female survival model should be used for the simulations?",
                                  "Models available are M1:M5_Johnson")),
    defineParameter(name = "recruitmentModelNumber", class = "character", 
                    default = "M4", min = NA, max = NA, 
                    desc = paste0("Which recruitment model should be used for the simulations?",
                                  "Models available are M3, M7 and M8 (ECCC); M1:M5 (Johnson)"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from anthropogenic layer",
                 sourceURL = NA),
    expectsInput(objectName = "currentPop", objectClass = "numeric", 
                 desc = paste0("Caribou population size in the study area.",
                               "Is updated every time step if not using lambda models"),
                 sourceURL = NA),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer",
                 desc = paste0("Map of groups of pixels that share the same info from cohortData (sp, age, biomass, etc).",
                               " Should at some point be genrated by the fire model (i.e. scfmSpread)")),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                 desc = paste0("data.table with information by pixel group of sp, age, biomass, etc")),
    expectsInput(objectName = "bufferedAnthropogenicDisturbance500m", objectClass = "RasterLayer",
                 desc = paste0("Layer that maps the % of anthropogenic disturbance in a 500m buffer.", 
                               "This layer is static if no modules are forecasting anthropogenic disturbances")),
    expectsInput(objectName = "caribouCoefTable", objectClass = "data.table", 
                 desc = "Published caribou coefficients", 
                 sourceURL = NA),
    expectsInput(objectName = "listSACaribou", objectClass = "list", 
                  desc = paste0("List of caribou areas to summarize predictions for",
                                "Defaults to shapefile of polygons in the NWT")),
    expectsInput(objectName = "historicalFires", objectClass = "SpatialPolygonsDataFrame", 
                 desc = paste0("All fires in a year are identified by the year ",
                               " (i.e. sam ID for all). This layer was built by",
                               " James Hodson (GNWT) for NWT)"),
                 sourceURL = "https://drive.google.com/file/d/1WPfNrB-nOejOnIMcHFImvnbouNFAHFv7"),
    expectsInput(objectName = "flammableMap", objectClass = "RasterLayer", 
                 desc = paste0("Flammable map to mask historical and current fireLayer ",
                               "to flammable areas")),
    expectsInput(objectName = "rstCurrentBurnList", objectClass = "list", 
                 desc = paste0("List of fires by year (raster format). These ",
                               "layers are produced by simulation.",
                               "Defaults to dummy data."),
                 sourceURL = ""),
    expectsInput(objectName = "populationGrowthTable", objectClass = "data.table", 
                 desc = paste0("Table with 6 columns:",
                               "1. modelVersion: ECCC or Johnson", # only for recruitment
                               "2. responseVariable: femaleSurvival or recruitment",
                               "3. Type: National", # To keep consistency if we include Regional models
                               "4. modelNumber: M1-M12 (not all are available for both ECCC/Johnson)",
                               "5. Coefficient: name of the coefficient",
                               "6. Value: value of the coefficient",
                               "7. StdErr: Standard error of the coefficient",
                               "rows are different values"),
                 sourceURL = "https://drive.google.com/file/d/1tl0tcvtWVBYXJ5LXS71VmI6HPK7iSkly/view?usp=sharing")
  ), 
  outputObjects = bindrows(
    createsOutput(objectName = "predictedCaribou", objectClass = "list", 
                  desc = paste0("Data.table that contains the total population size ",
                                "per year, as well as other parameters")),
    createsOutput(objectName = "currentPop", objectClass = "numeric", 
                  desc = "Caribou population size in the study area. Is updated every time step"),
    createsOutput(objectName = "plotCaribou", objectClass = "ggplot2", 
                  desc = "Caribou population size through time"),
    createsOutput(objectName = "disturbances", objectClass = "list", 
                  desc = paste0("disturbances is a list of shapefiles that have polygons with ", 
                                "disturbance in the area in percentage (total ). It is ",
                                "created using ..... from .....")),
    createsOutput(objectName = "listSACaribou", objectClass = "list", 
                  desc = paste0("List of caribou areas to predict for",
                                " Currently only takes 3 shapefiles")),
    createsOutput(objectName = "recruitmentModelDT", objectClass = "list", 
                  desc = paste0("List of caribou recruitment table to extract coefficients")),
    createsOutput(objectName = "femaleSurvivalModelDT", objectClass = "list", 
                  desc = paste0("List of caribou fem survival table to extract coefficients")),
    createsOutput(objectName = "femaleSurvivalModel", objectClass = "list", 
                  desc = "List of female survival model equations"),
    createsOutput(objectName = "recruitmentModel", objectClass = "list", 
                  desc = "List of recruitment model equations"),
    createsOutput(objectName = "fireLayerList", objectClass = "list", 
                  desc = paste0("List of fires for each year, from the ",
                                "(currentYear-recoveryTime):currentYear",
                                "This will include historical years when available",
                                " topped-up with simulated fire going forward in future"))
  )
))

doEvent.caribouPopGrowthModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # If model is annualLambda but the user passes .growthInterval, 
      # use timestepLambda instead
      if (all(P(sim)$popModel == "annualLambda",
          P(sim)$.growthInterval != 1)){
        warning(paste0("'annualLambda' defined as popModel but .growthInterval supplied ",
                       "and different than 1. Updating popModel to 'timestepLambda'"), 
                immediate. = TRUE)
        params(sim)[[currentModule(sim)]]$popModel <- "timestepLambda"
      }
      
      sim$predictedCaribou <- list()
      sim$fireLayer <- list()

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "makingModel")
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "gettingData")
      sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "growingCaribou")
      sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "plot", eventPriority = .last())
      #TODO NOT FULLY IMPLEMENTED. CODE NEEDS REVISION.
      # if (!P(sim)$popModel %in% c("annualLambda", "timestepLambda"))
      #   sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "updatingPopulationSize")
    },
    makingModel = {
      # 1. Prepare table based on which models to use
      message("Building recruitment model tables")
      # 1.1. recruitmentModelDT: Table with 3 columns: Coefficient, Value, StdErr
      sim$recruitmentModelDT <- makeDTforPopGrowth(populationGrowthTable = sim$populationGrowthTable, 
                                                   modelVersion = P(sim)[["recruitmentModelVersion"]],
                                                   modelNumber = P(sim)[["recruitmentModelNumber"]],
                                                   responseVariable = "recruitment")
      message("Building female survival model tables")
      # 1.2. femaleSurvivalModelDT: Table with 3 columns: Coefficient, Value, StdErr
      sim$femaleSurvivalModelDT <- makeDTforPopGrowth(populationGrowthTable = sim$populationGrowthTable,
                                                      modelVersion = P(sim)[["femaleSurvivalModelVersion"]],
                                                      modelNumber = P(sim)[["femaleSurvivalModelNumber"]],
                                                      responseVariable = "femaleSurvival")
      
      # 2. Lapply over the models types (modelVersion, modelNumber, Type combinations) 
      # to use and run the buildCoefficientsTable()
      # list (model types) of lists of 2 objects: 
      #   a) matrix with SD x nBootstrap (coeffTable) 
      #   b) averages (coeffValues)
      #   2.1. recruitmentModel
      sim$recruitmentModel <- lapply(names(sim$recruitmentModelDT), FUN = function(modelType){
        message(paste0("Building recruitment models for ", modelType))
        tb <- buildCoefficientsTable(caribouCoefTable = sim$recruitmentModelDT[[modelType]],
                                     nBootstrap = P(sim)$nBootstrap)
                                     })
      names(sim$recruitmentModel) <- names(sim$recruitmentModelDT)
      # 2.2. femaleSurvivalModel
      sim$femaleSurvivalModel <- lapply(names(sim$femaleSurvivalModelDT), FUN = function(modelType){
        message(paste0("Building female survival models for ", modelType))
        tb <- buildCoefficientsTable(caribouCoefTable = sim$femaleSurvivalModelDT[[modelType]],
                                     nBootstrap = P(sim)$nBootstrap)
      })
      names(sim$femaleSurvivalModel) <- names(sim$femaleSurvivalModelDT)
    },
    gettingData = {
      # sim$fireLayerList: a list of years of simulation with one raster composing
      #                    the most recent fires, namely (currentYear-recoveryTime):currentYear
      sim$fireLayerList[[paste0("Year", time(sim))]] <- composeFireRaster(historicalFires = sim$historicalFires,
                                         thisYearsFires = sim$rstCurrentBurnList,
                                         studyArea = sim$studyArea,
                                         recoveryTime = P(sim)$recoveryTime,
                                         currentTime = time(sim),
                                         pathData = dataPath(sim), # To compare to current time. First time needs 
                                         # to be different as we are creating layers, not updating them
                                         rasterToMatch = sim$rasterToMatch)
      # Assertion for changes in rasterToMatch
      if (ncell(sim$rasterToMatch) != ncell(sim$fireLayerList[[paste0("Year", time(sim))]]))
        stop("The number of cells in the RTM does not match the number of cells in the fireLayer. ",
             "Please make sure all layers provided align.")
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.growthInterval, "caribouPopGrowthModel", "gettingData")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "gettingData")
      }
    },
    growingCaribou = {
      if (P(sim)$.useDummyData == TRUE){
        message(crayon::red(paste0("Disturbance total information (pixelGroupMap & cohortData) was not found.", 
                                   "\nGenerating DUMMY DATA to test the module.")))
          sim$disturbances <- list(assign(x = paste0("Year", time(sim)), 
                                          value = data.frame(disturbances = rnorm(n = 1, 
                                                                                  mean = 30.75, 
                                                                                  sd = 10.6))))
          names(sim$disturbances) <- paste0("Year", time(sim))
        params(sim)[[currentModule(sim)]]$.useDummyData <- FALSE # this guarantees that next year is gonna be checked for data 
      } else {
        if (is.null(sim$disturbances)){
          sim$disturbances <- list()
        }
        sim$disturbances[[paste0("Year", time(sim))]] <- getLayersCaribou(currentTime = time(sim),
                                                                   recoveryTime = P(sim)$recoveryTime,
                                                                   listSACaribou = sim$listSACaribou,
                                                                   bufferedAnthropogenicDisturbance500m = sim$bufferedAnthropogenicDisturbance500m,
                                                                   useFuture = P(sim)$useFuture,
                                                                   fireLayer = sim$fireLayerList[[paste0("Year", time(sim))]],
                                                                   waterRaster = sim$waterRaster,
                                                                   rasterToMatch = sim$rasterToMatch,
                                                                   destinationPath = dataPath(sim))
      }
      if (!all(is.na(sim$disturbances[[paste0("Year", time(sim))]])))
        sim$predictedCaribou[[paste0("Year", time(sim))]] <- populationGrowthModel(
                                                      femaleSurvivalModel = sim$femaleSurvivalModel,
                                                      recruitmentModel = sim$recruitmentModel,
                                                      disturbances = sim$disturbances[[paste0("Year", time(sim))]],
                                                      currentTime = time(sim),
                                                      growthInterval = P(sim)$.growthInterval,
                                                      popModel = P(sim)$popModel)
      message(paste0("Caribou growth information for ", time(sim)))
      print(sim$predictedCaribou[[paste0("Year", time(sim))]])
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.growthInterval, "caribouPopGrowthModel", "growingCaribou")
      if (P(sim)$predictLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "growingCaribou")
      }
    },
    updatingPopulationSize = {
      #TODO NOT FULLY IMPLEMENTED. CODE NEEDS REVISION.
      # sim$currentPop <- lapply(X = names(sim$caribouModels), FUN = function(model){
      #   sim$predictedCaribou[[paste0("Year", time(sim))]][[model]]$currentPopUpdated
      # })
      # names(sim$currentPop) <- names(sim$caribouModels)
      # 
      # # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.growthInterval, "caribouPopGrowthModel", "updatingPopulationSize")
      
    },
    plot = {
        sim$plotCaribou <- plotCaribouPopGrowth(startTime = start(sim),
                                                               currentTime = time(sim),
                                                               endTime = end(sim),
                                                               climateModel = P(sim)$climateModel,
                                                               predictedCaribou = sim$predictedCaribou,
                                                               yearSimulationStarts = start(sim),
                                                               whichPolys = P(sim)$polygonsOfInterest,
                                                               outputFolder = Paths$outputPath)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  stepCacheTag <- c("module:caribouPopGrowth",
                    "event:.inputObjects")
  
  # THis is already set in metadata
  # params(sim)[[currentModule(sim)]]$.useDummyData <- FALSE
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("populationGrowthTable", sim)){
    sim$populationGrowthTable <- prepInputs(targetFile = "populationGrowthTable.csv", 
                                       url = extractURL("populationGrowthTable"),
                                       destinationPath = dataPath(sim), 
                                       fun = "data.table::fread", 
                                       omitArgs = "destinationPath", 
                                       overwrite = TRUE)
  }
    
  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- Cache(prepInputs,
                                url = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09/view?usp=sharing",
                                destinationPath = dataPath(sim),
                                omitArgs = "destinationPath")
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/file/d/11yCDc2_Wia2iw_kz0f0jOXrLpL8of2oM/view?usp=sharing", 
                                    studyArea = sim$studyArea,
                                    destinationPath = dataPath(sim),
                                    overwrite = TRUE,
                                    omitArgs = c("destinationPath", "overwrite", "filename2"))
  }
  if (!suppliedElsewhere(object = "listSACaribou", sim = sim)){
    
    caribouArea1 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV",
                                   targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
                                   destinationPath = dataPath(sim), 
                          filename2 = "caribouBOCAunits")
  
    caribouArea2 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO",
                                   targetFile = "NWT_Regions_2015_LCs_DC_SS_combined_NT1_clip_inc_Yukon.shp",
                                   destinationPath = dataPath(sim), 
                          filename2 = "caribouNT1herds")
    
    sim$listSACaribou = list(caribouArea1, caribouArea2)
    names(sim$listSACaribou) <- c("BOCAunits", "NT1herds")
  }

  if (!suppliedElsewhere("currentPop", sim) &
      P(sim)$popModel != "annualLambda"){
    # Currently not used!
    # message(crayon::yellow(paste0("Initial population size not provided.", 
    #                            "\nGenerating a mean population size for the studyArea of Edehzhie (n = 353).")))
    # sim$currentPop <- 353 # [ FIX ] should pass a file that is a list of population sizes for each one of the units/LPU for each studyArea shp 
  }

  if (!suppliedElsewhere("waterRaster", sim)){
  sim$waterRaster <- prepInputs(url = paste0("https://drive.google.com/file/d",
                                              "/1eZdn3kwCWjl3fTQwxnkpQhYmLLuBB",
                                              "gVe/view?usp=sharing"), 
                                 destinationPath = Paths[["inputPath"]],
                                 studyArea = sim$studyArea,
                                 overwrite = TRUE,
                                 rasterToMatch = sim$rasterToMatch,
                                 method = "ngb")
  # If LCC2005, water = 37
  # If LCC2010, water = 18
  sim$waterRaster[!is.na(sim$waterRaster[]) & 
                    sim$waterRaster[] != 37] <- NA
  sim$waterRaster[!is.na(sim$waterRaster[])] <- 1
  # Remove annoying colortable
  wV <- getValues(sim$waterRaster)
  sim$waterRaster <- setValues(x = raster(sim$waterRaster), 
                               values = wV)
  }

  if (!suppliedElsewhere("bufferedAnthropogenicDisturbance500m", sim)){
    bufferedAnthropogenicDisturbance500m <- Cache(prepInputs, targetFile = "buffered500mDisturbancesUnified_NT1_BCR6.shp",
                                                  archive = "buffered500mDisturbancesUnified_NT1_BCR6.zip",
                                                  alsoExtract = "similar",
                                                  url = "https://drive.google.com/file/d/1yz39dGW4XMJk5ox6TuVUOMrU4q3mhfhU/view?usp=sharing",
                                                  destinationPath = Paths$inputPath, 
                                                  studyArea = sim$studyArea,
                                                  rasterToMatch = sim$rasterToMatch,
                                                  userTags = c(stepCacheTag,
                                                               "step:prepAnthropogenicDistLayer", "outFun:Cache"))
    bufferedAnthropogenicDisturbance500mSF <- sf::st_as_sf(bufferedAnthropogenicDisturbance500m)
    bufferedAnthropogenicDisturbance500mSF$fieldSF <- 1
    bufferedAnthropogenicDisturbance500m <- fasterize::fasterize(sf = bufferedAnthropogenicDisturbance500mSF,
                                                                 raster = sim$rasterToMatch, field = "fieldSF", 
                                                                 background = 0)
    buffAnthroDist500m <- Cache(postProcess, x = bufferedAnthropogenicDisturbance500m,
                                destinationPath = Paths[["inputPath"]], 
                                studyArea = sim$studyArea,
                                rasterToMatch = sim$rasterToMatch,
                                userTags = c(stepCacheTag,
                                             "step:maskAnthropogenicDistLayer", "outFun:Cache"))
    sim$bufferedAnthropogenicDisturbance500m <- buffAnthroDist500m
  }
  if (!suppliedElsewhere("historicalFires", sim)){
    sim$historicalFires <- prepInputs(url = paste0("https://drive.google.com/",
                                                   "file/d/1fSsgP1VUgQbhq4GpCE",
                                                   "PZh2yVjh5SN2bv/view?usp=",
                                                   "sharing"),
                                      targetFile = "historicalFires.qs",
                                      destinationPath = Paths[["inputPath"]],
                                      overwrite = TRUE,
                                      fun = "qs::qread")
  }
  if (!suppliedElsewhere("rstCurrentBurnList", sim)){
    warning("rstCurrentBurnList needs to be provided and was not found in the simList. 
             Trying to find it in inputPath", immediate. = TRUE)
    sim$rstCurrentBurnList <- tryCatch(
      readRDS(file.path(Paths[["inputPath"]], 
                        "rstCurrentBurnList_year2100.rds")
      ),
      error = function(e) {
        warning("rstCurrentBurnList was not found. Generating DUMMY fire data ",
                "for the NWT. If your study area does not match this location, ",
                "no fires will be generated!",
                immediate. = TRUE)
        rstCurrentBurnList2 <- Cache(prepInputs, url = "https://drive.google.com/file/d/1PRJYjie5vdsq_4W6WN5zjnnjuBlt8cJY/view?usp=sharing",
                                    destinationPath = Paths[["inputPath"]],
                                    fun = "readRDS",
                                    userTags = c("module:caribouPopGrowthModule",
                                                 "event:.inputObjects"))
        return(rstCurrentBurnList)
      })
    message(crayon::green("rstCurrentBurnList loaded successfully!"))
  }
  return(invisible(sim))
}
