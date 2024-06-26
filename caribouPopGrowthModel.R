defineModule(sim, list(
  name = "caribouPopGrowthModel",
  description = paste("Module to simulate Caribou population growth (lambda)",
                      "using published ECCC data (2011) or Johnson et al. (2020) models."),
  keywords = c("Caribou", "population", "lambda"),
  authors = c(
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
    person("Frances", "Stewart", email = "fstewart@wlu.ca", role = c("aut", "cre")),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(caribouPopGrowthModel = "0.2.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "caribouPopGrowthModel.Rmd"),
  reqdPkgs = list("raster", "data.table", "ggplot2", "sf", "tictoc",
                  "future", "future.apply", "matrixStats",
                  # "simstudy", "LandSciTech/caribouMetrics", 
                  "dplyr"),
  parameters = rbind(
    defineParameter("cropAllRasToSA", "logical", TRUE, NA, NA,
                    paste0("Some rasters might be covering different areas (i.e., ",
                           " when landscape rasters are larger due to fire ",
                           "processes and still named rasterToMatch).",
                           " This parameter ensures all align without compromising ",
                           "the RTM (i.e., by using it in mod). This helps voiding ",
                           "situations with fires being generated in water.")),
    defineParameter("predictLastYear", "logical", TRUE, NA, NA,
                    paste0("Should it schedule events for the last year",
                           " of simulation if this is not a multiple of interval?")),
    defineParameter("whichPolysToIgnore", "character", NULL, NA, NA,
                    paste0("If results should not be presented for a set of polygons",
                           " these should be expressed here")),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should this entire module be run with caching activated?"),
    defineParameter("nBootstrap", "numeric", 1000, NA, NA,
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
    defineParameter(".runName", "character", NULL, NA, NA,
                    paste0("Needs to be provided. Can be a combination of short Province name and ",
                           "replicate")),
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
    defineParameter(name = "useQuantiles", class = "logic",
                    default = TRUE, min = NA, max = NA,
                    desc = paste0("Should the predictions use quantiles (adding uncertainty from ",
                                  "beta distribution)?")),
    defineParameter(name = ".fireLayerListProvided", class = "logical",
                    default = FALSE, min = NA, max = NA,
                    desc = paste0("Do not change this. This parameter is ",
                                  "automatically changed based on provided inputs")),
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
    expectsInput(objectName = "shortProvinceName", objectClass = "character",
                 desc = paste0("Short 2 letter name of the province(s) where",
                               " the simulation is being run, i.e., c('AB', 'SK')"),
                 sourceURL = NA),
    expectsInput(objectName = "waterRaster", objectClass = "RasterLayer",
                 desc = "Wetland raster for excluding water from anthropogenic layer",
                 sourceURL = NA),
    expectsInput(objectName = "currentPop", objectClass = "numeric",
                 desc = paste0("Caribou population size in the study area.",
                               "Is updated every time step if not using lambda models",
                               "Not currently implemented."),
                 sourceURL = NA),
    expectsInput(objectName = "bufferedAnthropogenicDisturbance500m", objectClass = "RasterLayer",
                 desc = paste0("Layer that maps the % of anthropogenic disturbance in a 500m buffer.",
                               "This layer is static if no modules are forecasting anthropogenic disturbances")),
    expectsInput(objectName = "listSACaribou", objectClass = "list",
                 desc = paste0("Named list of caribou areas to summarize predictions for",
                               "Defaults to shapefile of polygons in the NWT")),
    expectsInput(objectName = "historicalFires", objectClass = "SpatialPolygonsDataFrame",
                 desc = paste0("All fires in a year are identified by the year",
                               " (i.e. same ID for all, 'fireYear') WITH minimum date (recoveryTime).",
                               "The original layer is subset to the currentTime - recoveryTime"),
                 sourceURL = "https://drive.google.com/file/d/1WPfNrB-nOejOnIMcHFImvnbouNFAHFv7"),
    expectsInput(objectName = "historicalFiresAll", objectClass = "SpatialPolygonsDataFrame",
                 desc = paste0("All fires in a year are identified by the year",
                               " (i.e. same ID for all, 'fireYear') WITHOUT minimum date"),
                 sourceURL = "https://drive.google.com/file/d/1WPfNrB-nOejOnIMcHFImvnbouNFAHFv7"),
    expectsInput(objectName = "rstCurrentBurnList", objectClass = "list",
                 desc = paste0("List of fires by year (raster format). These ",
                               "layers are produced by simulation. The module needs ALL years ",
                               "between starting and final ones because of the recovery time!",
                               "Defaults to dummy data with warning if data not provided."),
                 sourceURL = "https://drive.google.com/file/d/1PRJYjie5vdsq_4W6WN5zjnnjuBlt8cJY/"),
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
                 sourceURL = "https://drive.google.com/file/d/1tl0tcvtWVBYXJ5LXS71VmI6HPK7iSkly/"),
    expectsInput(objectName = "fireLayerList", objectClass = "list",
                 desc = paste0("List of fires for each year, from the ",
                               "(currentYear-recoveryTime):currentYear",
                               "This will include historical years when available",
                               " topped-up with simulated fire going forward in future",
                               "If not provided, will be automatically created,",
                               "and this process demands at least 32GB of RAM")),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonDataFrame",
                 desc = "Study area for the prediction. Currently only available for NWT",
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "All spatial outputs will be reprojected and resampled to it",
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = ".notRun", objectClass = "logical",
                  desc = paste0("If the caribou herds/management units do not intersect with ",
                                "the province boundaries or the herd is not classified ",
                                "as being administered by the given province, the module ",
                                "will not be run")),
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
    createsOutput(objectName = "demographicCoefficients", objectClass = "list",
                  desc = "List of demographic coefficients for both survival and reproduction"),
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
      # THE CODE BELOW NEEDS TO BE REVISD FOR WHEN WE DECIDE TO CALCULATE THE POPULATION SIZE
      # CURRENTLY, AS WE ARE ONLY DOING SNAPSHOT OF LAMBDA IN A GIVEN YEAR, WE SHOULDN'T
      # EXPONENTIATE THE CALCULATED VALUES
      # # If model is annualLambda but the user passes .growthInterval,
      # # use timestepLambda instead
      # if (all(P(sim)$popModel == "annualLambda",
      #     P(sim)$.growthInterval != 1)){
      #   warning(paste0("'annualLambda' defined as popModel but .growthInterval supplied ",
      #                  "and different than 1. Updating popModel to 'timestepLambda'"),
      #           immediate. = TRUE)
      #   params(sim)[[currentModule(sim)]]$popModel <- "timestepLambda"
      # }
      if (!is(sim$listSACaribou, "list")){
        stop(paste0("listSACaribou needs to be a named list"))
      } else {
        if (is.null(names(sim$listSACaribou)))
          stop(paste0("listSACaribou needs to be a named list"))
      }
      mod$rasterToMatch <- sim$rasterToMatch

      if (P(sim)$cropAllRasToSA){
        mod$rasterToMatch <- maskInputs(x = mod$rasterToMatch,
                                        studyArea = sim$studyArea)
        mod$rasterToMatch[sim$waterRaster[] == 1] <- NA
      }

      sim$predictedCaribou <- list()
      if (!P(sim)$.fireLayerListProvided)
        sim$fireLayerList <- list()

      if (is.null(P(sim)$useQuantiles))
        stop(paste0("useQuantiles should be a length 2 numeric indicating quantiles to use, TRUE",
                    "for default (0.025, 0.975) or FALSE for no quantiles."))
        
      if (!isTRUE(sim$.notRun)) {
        # schedule future event(s)
        sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "makingModel")
        sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "gettingData")
        sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "growingCaribou")
        sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "plot", eventPriority = .last())
        #TODO NOT FULLY IMPLEMENTED. CODE NEEDS REVISION.
        # if (!P(sim)$popModel %in% c("annualLambda", "timestepLambda"))
        #   sim <- scheduleEvent(sim, start(sim), "caribouPopGrowthModel", "updatingPopulationSize")
      }
    },
    makingModel = {
      # 1. Prepare table based on which models to use
      message("Building recruitment model and female survival model tables")
      # if (any(is(P(sim)$useQuantiles, "numeric"),
      #         is(P(sim)$useQuantiles, "logical"))){
      #   usequantiles <- TRUE
      # }
      # if (!usequantiles) {
      #   # 1.1. recruitmentModelDT: Table with 3 columns: Coefficient, Value, StdErr
      #   sim$recruitmentModelDT <- makeDTforPopGrowth(populationGrowthTable = sim$populationGrowthTable,
      #                                                modelVersion = P(sim)[["recruitmentModelVersion"]],
      #                                                modelNumber = P(sim)[["recruitmentModelNumber"]],
      #                                                responseVariable = "recruitment")
      #   message("Building female survival model tables")
      #   # 1.2. femaleSurvivalModelDT: Table with 3 columns: Coefficient, Value, StdErr
      #   sim$femaleSurvivalModelDT <- makeDTforPopGrowth(populationGrowthTable = sim$populationGrowthTable,
      #                                                   modelVersion = P(sim)[["femaleSurvivalModelVersion"]],
      #                                                   modelNumber = P(sim)[["femaleSurvivalModelNumber"]],
      #                                                   responseVariable = "femaleSurvival")

        # 2. Lapply over the models types (modelVersion, modelNumber, Type combinations)
        # to use and run the buildCoefficientsTable()
        # list (model types) of lists of 2 objects:
        #   a) matrix with SD x nBootstrap (coeffTable)
        #   b) averages (coeffValues)
        #   2.1. recruitmentModel
      #   sim$recruitmentModel <- lapply(names(sim$recruitmentModelDT), FUN = function(modelType) {
      #     message(paste0("Building recruitment models for ", modelType))
      #     tb <- buildCoefficientsTable(caribouCoefTable = sim$recruitmentModelDT[[modelType]],
      #                                  nBootstrap = P(sim)$nBootstrap)
      #   })
      #   names(sim$recruitmentModel) <- names(sim$recruitmentModelDT)
      #   # 2.2. femaleSurvivalModel
      #   sim$femaleSurvivalModel <- lapply(names(sim$femaleSurvivalModelDT), FUN = function(modelType) {
      #     message(paste0("Building female survival models for ", modelType))
      #     tb <- buildCoefficientsTable(caribouCoefTable = sim$femaleSurvivalModelDT[[modelType]],
      #                                  nBootstrap = P(sim)$nBootstrap)
      #   })
      #   names(sim$femaleSurvivalModel) <- names(sim$femaleSurvivalModelDT)
      # } else {
      # Not sure the quantiles need to be tracked here... They have no relation to the data,
      # only to the number of bootstraps... 
      sim$demographicCoefficients <- demographicCoefficients(replicates = P(sim)$nBootstrap,
                                                               survivalModelNumber = P(sim)[["femaleSurvivalModelNumber"]],
                                                               modelVersion = P(sim)[["femaleSurvivalModelVersion"]],
                                                               recruitmentModelNumber = P(sim)[["recruitmentModelNumber"]],
                                                               populationGrowthTable = sim$populationGrowthTable,
                                                               useQuantiles = P(sim)$useQuantiles)
          
      # }

    },
    gettingData = {
      if (!P(sim)$.fireLayerListProvided) {
        message("Creating fireLayerList for ", time(sim))
        if (is.null(sim$rstCurrentBurnList))
          stop("rstCurrentBurnList is NULL Please check it is correctly being provided by another module")
        # sim$fireLayerList: a list of years of simulation with one raster composing
        #                    the most recent fires, namely (currentYear-recoveryTime):currentYear
        # To compare to current time. First time needs to be different as we are creating layers, not updating them
        sim$fireLayerList[[paste0("Year", time(sim))]] <- composeFireRaster(historicalFires = sim$historicalFires,
                                                                            thisYearsFires = sim$rstCurrentBurnList,
                                                                            studyArea = sim$studyArea,
                                                                            recoveryTime = P(sim)$recoveryTime,
                                                                            currentTime = time(sim),
                                                                            pathData = dataPath(sim),
                                                                            rasterToMatch = mod$rasterToMatch
        )
      }
      # Assertion for changes in rasterToMatch
      if (ncell(mod$rasterToMatch) != ncell(sim$fireLayerList[[paste0("Year", time(sim))]]))
        stop("The number of cells in the RTM does not match the number of cells in the fireLayer. ",
             "Please make sure all layers provided align.")
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.growthInterval, "caribouPopGrowthModel", "gettingData")
      if (P(sim)$predictLastYear) {
        if (all(time(sim) == start(sim), (end(sim) - start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "gettingData")
      }
    },
    growingCaribou = {
      if (P(sim)$.useDummyData == TRUE) {
        message(crayon::red(paste0("\nGenerating DUMMY DATA to test the module.")))
          sim$disturbances <- list(assign(x = paste0("Year", time(sim)),
                                          value = data.frame(disturbances = rnorm(n = 1,
                                                                                  mean = 30.75,
                                                                                  sd = 10.6))))
          names(sim$disturbances) <- paste0("Year", time(sim))
        params(sim)[[currentModule(sim)]]$.useDummyData <- FALSE # this guarantees that next year is gonna be checked for data
      } else {
        if (is.null(sim$disturbances)) {
          sim$disturbances <- list()
        }
        sim$disturbances[[paste0("Year", time(sim))]] <- getLayersCaribou(currentTime = time(sim),
                                                                   recoveryTime = P(sim)$recoveryTime,
                                                                   listSACaribou = sim$listSACaribou,
                                                                   bufferedAnthropogenicDisturbance500m = sim$bufferedAnthropogenicDisturbance500m,
                                                                   useFuture = P(sim)$useFuture,
                                                                   fireLayer = sim$fireLayerList[[paste0("Year", time(sim))]],
                                                                   waterRaster = sim$waterRaster,
                                                                   rasterToMatch = mod$rasterToMatch,
                                                                   destinationPath = dataPath(sim))
      }

      if (!all(is.na(sim$disturbances[[paste0("Year", time(sim))]]))) {
        usequantiles <- if (isTRUE(P(sim)$useQuantiles)) c(0.025, 0.975) else 
          if (isFALSE(P(sim)$useQuantiles)) c(0, 1) else 
            P(sim)$useQuantiles
           surv <- list(sim$demographicCoefficients$coefSamples_Survival)
           names(surv) <- P(sim)$femaleSurvivalModelVersion
           rec <- list(sim$demographicCoefficients$coefSamples_Recruitment)
           names(rec) <- P(sim)$recruitmentModelVersion

           sim$predictedCaribou[[paste0("Year", time(sim))]] <- populationGrowthModel(femaleSurvivalModel = surv,
                                                                                      recruitmentModel = rec,
                                                                                      disturbances = sim$disturbances[[paste0("Year", time(sim))]],
                                                                                      currentTime = time(sim),
                                                                                      popModel = P(sim)$popModel,
                                                                                      useQuantiles = usequantiles)
      }
      message(paste0("Caribou growth information for ", time(sim)))
      if (length(names(sim$predictedCaribou[[paste0("Year", time(sim))]])[names(sim$predictedCaribou[[paste0("Year", time(sim))]]) == "polygon"]) > 0)
        names(sim$predictedCaribou[[paste0("Year", time(sim))]])[names(sim$predictedCaribou[[paste0("Year", time(sim))]]) == "polygon"] <- "Herd"
      
      summaryToPrint <- tryCatch({
        unique(merge(sim$predictedCaribou[[paste0("Year", time(sim))]],
            digestDisturbances(disturbances = sim$disturbances[[paste0("Year", time(sim))]],
                               Year = time(sim)),
            by = c("Herd", "area")))
        }, error = function(e){
          return(merge(sim$predictedCaribou[[paste0("Year", time(sim))]],
                       digestDisturbances(disturbances = sim$disturbances[[paste0("Year", time(sim))]],
                                          Year = time(sim)),
                       by = c("Herd", "area")))
        })
      
      names(summaryToPrint)[names(summaryToPrint) == "area"] <- "Shapefile"
      setkey(summaryToPrint, "Shapefile", "annualLambda")
      print(summaryToPrint[, c("Shapefile", "Herd", "annualLambda", "average_femaleSurvival", 
                               "average_recruitment", "Anthro", "fire_prop_dist", "Fire", "Total_dist",
                               "fire_excl_anthro")])

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.growthInterval, "caribouPopGrowthModel", "growingCaribou")
      if (P(sim)$predictLastYear) {
        if (all(time(sim) == start(sim), (end(sim) - start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouPopGrowthModel", "growingCaribou")
      }
    },
    updatingPopulationSize = {
      #TODO NOT FULLY IMPLEMENTED. CODE NEEDS REVISION.
      # sim$currentPop <- lapply(X = names(sim$caribouModels), FUN = function(model) {
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
                                                               whichPolysToIgnore = P(sim)$whichPolysToIgnore,
                                                               outputFolder = Paths$outputPath)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  stepCacheTag <- c("module:caribouPopGrowth", "event:.inputObjects")

  if (is.null(P(sim)$.runName)) stop("Please provide a runName")

  # dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  dPath <- dataPath(sim)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (suppliedElsewhere("fireLayerList", sim)) {
    message(crayon::green("fireLayerList detected. The module will skip creating it."))
    params(sim)[[currentModule(sim)]]$.fireLayerListProvided <- TRUE
    }
  if (!suppliedElsewhere("populationGrowthTable", sim)) {
    tableFile <- file.path(dataPath(sim), "caribouCoefficientsTable.csv")
    if (file.exists(tableFile)){
      print("populationGrowthTable exists, loading...")
      sim$populationGrowthTable <- data.table::fread(tableFile) # This table has Precision.
    } else {
      sim$populationGrowthTable <- prepInputs(targetFile = "populationGrowthTable.csv",
                                              url = extractURL("populationGrowthTable"),
                                              destinationPath = dPath,
                                              fun = "data.table::fread",
                                              omitArgs = "destinationPath",
                                              overwrite = TRUE)
    }
  }
  if (!suppliedElsewhere(object = "studyArea", sim = sim)) {
    sim$studyArea <- Cache(prepInputs,
                                url = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09/",
                                destinationPath = dPath,
                                omitArgs = "destinationPath")
  }
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)) {
    sim$rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/file/d/11yCDc2_Wia2iw_kz0f0jOXrLpL8of2oM/",
                                    studyArea = sim$studyArea,
                                    destinationPath = dPath,
                                    overwrite = TRUE,
                                    omitArgs = c("destinationPath", "overwrite", "filename2"))
  }
  if (!suppliedElsewhere(object = "shortProvinceName", sim = sim)) {
    sim$shortProvinceName <- tryCatch({
    shpn <- strsplit(x = sim$runName, split = "_")[[1]][1]
    warning(paste0("shortProvinceName was not supplied. Trying to guess from runName: ",
            paste(sim$shortProvinceName, collapse = ", "),
            ". If this is not correct, please provide the correct object",
            " 'shortProvinceName'"), immediate. = TRUE)
    }, error = function(e){
      stop("shortProvinceName couldn't be guessed. Please provide it.")
    })
  }
  if (!suppliedElsewhere(object = "listSACaribou", sim = sim)) {
    # Shapefile: Boreal_Caribou_Range_Boundaries_AsOfJune62012.shp
    # Anthropogenic disturbance within caribou ranges
    url <- "https://ec.gc.ca/data_donnees/STB-DGST/001/BorealCaribouDisturbanceShapefiles_Updated2012.zip"
    SHP <- "Boreal_Caribou_Range_Boundaries_AsOfJune62012"
    herds <- prepInputs(url = url,
                        destinationPath = checkPath(file.path(dPath, "anthropogenicLayers"),
                                                    create = TRUE),
                        archive = "BorealCaribouDisturbanceShapefiles_Updated2012.zip",
                        alsoExtract = paste0(SHP, c(".dbf", ".shx",
                                                    ".sbn", ".sbx",
                                                    ".shp.xml", ".prj")),
                        targetFile = paste0(SHP, ".shp"))

    # Need to postprocess
    herds <- st_as_sf(herds)
    herds  <- st_set_crs(x = herds,
                         value = paste0("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40",
                                        " +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 ",
                                        "+datum=NAD83 +units=m no_defs"))
    herds <- as_Spatial(herds)
    herds <- projectInputs(x = herds, targetCRS = raster::crs(sim$studyArea))
    herds <- postProcess(x = herds,
                        studyArea = sim$studyArea) # ATTENTION: If passing RTM,
                                                   # it will grid the shp
    # remove any province leftovers from GIS operations. Due to the
    # terrible original projection (aea), we get weird little polygons from other
    # provinces at borders sometimes
    provNameToKeep <- if (sim$shortProvinceName == "NT") "NWT" else sim$shortProvinceName
    provsToKeep <- which(herds[["PROV_TERR"]] %in% unique(herds[["PROV_TERR"]][
      grepl(pattern = provNameToKeep, x = herds[["PROV_TERR"]])]))
    # subset
    herds <- herds[provsToKeep, ]

    sim$listSACaribou <- list(herds = herds)
    if (length(sim$listSACaribou$herds) == 0) {
      warning(crayon::red(paste0("No caribou herds are available for ", sim$shortProvinceName),
                          ". The module will not run for this area"),
              immediate. = TRUE)
      sim$.notRun <- TRUE
    }
    }
  # if (!suppliedElsewhere("currentPop", sim) &
      # P(sim)$popModel != "annualLambda") {
    # Currently not used!
    # message(crayon::yellow(paste0("Initial population size not provided.",
    #                            "\nGenerating a mean population size for the studyArea of Edehzhie (n = 353).")))
    # sim$currentPop <- 353 # [ FIX ] should pass a file that is a list of population sizes for each one of the units/LPU for each studyArea shp
  # }
  if (!suppliedElsewhere("waterRaster", sim)) {
    
    sim$waterRaster <- Cache(makeWaterFromLCC, year = 2019,
                                    destinationPath = dPath,
                                    method = "ngb", 
                                    studyArea = sim$studyArea,
                                    rasterToMatch = sim$rasterToMatch)
  
  }
  if (!suppliedElsewhere("bufferedAnthropogenicDisturbance500m", sim)) {
    warning(paste0("bufferedAnthropogenicDisturbance500m not found. Will use: ",
                   "HERD_Anthropogenic_Disturbance_Footprint_Updated2012", immediate. = TRUE))
    # OBS: The file used for caribou herds also has all fire and anthropogenic
    # disturbance in separated layers
    # I probably shouldn't use the fire because I can't exclude the "older".
    # Atikakibernes_Anthropogenic_and_40YrFire_Disturbance_Footprint_Updated2012.shp
    # I just build new ones.

    # But I can definitely use the anthropo because it is anyway
    # static and up to 2011/2012, which is when our sims start. And are already buffered!
    # PATTERN: HERD_Anthropogenic_Disturbance_Footprint_Updated2012.shp

    # ALTERNATIVE DATA SOURCES
    # Anthropogenic disturbances across the Boreal
    # url2 <- "https://www.ec.gc.ca/data_donnees/STB-DGST/003/Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip"
    # Anthropogenic disturbances across the Boreal digested?! No idea what is different from previous
    # url3 <- "https://ec.gc.ca/data_donnees/STB-DGST/002/BEAD_DATALAYERS.zip"
    # Make this generic based on the study area and caribou herds file
    print("Preparing anthropogenic disturbance layer")
    sim$bufferedAnthropogenicDisturbance500m <- Cache(
      getAnthropoDisturbanceForBorealCaribou,
      studyArea = sim$studyArea,
      rasterToMatch = sim$rasterToMatch,
      pathData = dPath,
      userTags = c(stepCacheTag,
                   "step:prepAnthropogenicDistLayer",
                   "outFun:Cache")
    )

    qs::qsave(sim$bufferedAnthropogenicDisturbance500m,
              file = file.path(dPath, paste0(sim$shortProvinceName,
                                             "_AnthropogenicDisturbances.qs")))
  }
  if (!suppliedElsewhere("historicalFiresAll", sim)) {
    
    sim$historicalFiresAll <- Cache(createHistoricalFilesAll, 
                             url = extractURL("historicalFiresAll"),
                             archive = "NBAC_CAN_1986_2017_NFDB_up_to_1985.zip",
                             targetFile = "NBAC_CAN_1986_2017_NFDB_up_to_1985.shp",
                             destinationPath = dataPath(sim),
                             studyArea = sim$studyArea,
                             userTags = c("objectName:historicalFiresAll",
                                          paste0("extension:", sim$shortProvinceName),
                                          stepCacheTag, "outFun:Cache"))
  }

  if (!suppliedElsewhere("historicalFires", sim)) {
    # Discard fires with more than 60 from starting time
    oldestFireYear <- time(sim) - P(sim)$recoveryTime
    sim$historicalFires <- sim$historicalFiresAll[sim$historicalFiresAll$fireYear >= oldestFireYear,]
  }

  if (!suppliedElsewhere("rstCurrentBurnList", sim)) {
    warning("rstCurrentBurnList needs to be provided and was not found in the simList.
             Trying to find it in inputPath", immediate. = TRUE)
    sim$rstCurrentBurnList <- tryCatch(
      readRDS(file.path(inputPath(sim), "rstCurrentBurnList_year2100.rds")
      ),
      error = function(e) {
        warning("rstCurrentBurnList was not found. Generating DUMMY fire data ",
                "for the NWT. If your study area does not match this location, ",
                "no fires will be generated!",
                immediate. = TRUE)
        rstCurrentBurnList <- Cache(prepInputs,
                                    url = extractURL("rstCurrentBurnList"),
                                    destinationPath = inputPath(sim),
                                    fun = "readRDS",
                                    userTags = c("module:caribouPopGrowthModule",
                                                 "event:.inputObjects"))
        return(rstCurrentBurnList)
      })
    message(crayon::green("rstCurrentBurnList loaded successfully!"))
  }
  
  return(invisible(sim))
}
