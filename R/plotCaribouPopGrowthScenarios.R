plotCaribouPopGrowthScenarios <- function(startTime,
                                 currentTime,
                                 endTime,
                                 studyArea,
                                 resultsMainFolder = NULL, # Pass this if outside of module
                                 climateModel = NULL,
                                 predictedCaribou = NULL,
                                 studyAreaName = "NT",
                                 scenarios = c(0.2, 0.4, 0.6),
                                 runNameShort = "herds",
                                 yearSimulationStarts,
                                 reps = paste0("run", 1:5),
                                 outputFolder,
                                 whichPolysToIgnore = NULL, # Optional to ensure only specific polygons to be plotted
                                 timeSpan = "annual") # Optional = "timeStep" (normally every 10y)
{
 
  library("Require")
  Require("data.table")
  Require("ggplot2")
  if (any(all(is.null(resultsMainFolder),
              is.null(predictedCaribou)),
          all(!is.null(resultsMainFolder),
              !is.null(predictedCaribou))))
    stop("Please provide either predictedCaribou or resultsMainFolder")

  if (is.null(climateModel)){
    message(crayon::red("climateModel is NULL, default is 'CCSM4'"))
    climateModel <- "CCSM4"
  }

  if (!is.null(resultsMainFolder)){

    popQuant <- c("0.9-1", "0-0.1")
    allcombs <- data.table(expand.grid(studyAreaName, climateModel, 
                                       scenarios, runNameShort, reps))
    allcombs[, comb := paste(Var1, Var2, Var3, Var4, Var5, sep = "_")]
    pth <- file.path(resultsMainFolder, allcombs[["comb"]])

    predictedCaribou <- rbindlist(lapply(seq_along(pth), function(filePathIndex){
      tb1 <- readRDS(list.files(path = pth[filePathIndex], 
                                pattern = paste0("predictedCaribou_Q_", popQuant[1], 
                                                "_year2041.rds"),
                                full.names = TRUE, recursive = FALSE))
      tb2 <- readRDS(list.files(path = pth[filePathIndex],
                                pattern = paste0("predictedCaribou_Q_", popQuant[2], 
                                                 "_year2041.rds"),
                                full.names = TRUE, recursive = TRUE))
      popQ <-   c("Top 10% Quantile", 
                  "Bottom 10% Quantile")
      replic <- "run01"
      addedTB1 <- rbindlist(lapply(names(tb1), function(years){
        TB <- tb1[[years]]
        # climMod <- as.character(allcombs[filePathIndex, "Var2"])
        distScen <- as.character(allcombs[filePathIndex, "Var3"])
        booScen <- popQ[1]
        # replic <- as.character(allcombs[filePathIndex, "Var5"])
        TB[, c("climateModel", "Replicate", "Year", 
               "DisturbanceScenario",
               "populationScenario") := list(climateModel, 
                                             replic, 
                                             substrBoth(years, 4, T),
                                             distScen,
                                             booScen)]
        return(TB)
      }))
      addedTB2 <- rbindlist(lapply(names(tb2), function(years){
        TB <- tb2[[years]]
        # climMod <- as.character(allcombs[filePathIndex, "Var2"])
        distScen <- as.character(allcombs[filePathIndex, "Var3"])
        booScen <- popQ[2]
        # replic <- as.character(allcombs[filePathIndex, "Var5"])
        TB[, c("climateModel", "Replicate", "Year", 
               "DisturbanceScenario",
               "populationScenario") := list(climateModel, 
                                             replic, 
                                             substrBoth(years, 4, T),
                                             distScen,
                                             booScen)]
        return(TB)
      }))
      addedTB <- rbind(addedTB1, addedTB2)
      return(addedTB)
    }))
  }

  tableAll <- predictedCaribou

  if (is(tableAll, "list")){ # If this is a list (i.e. if the results are coming from the module), collapse into a data.table
      condTB <- rbindlist(lapply(names(tableAll), function(YYYY){
          y <- as.numeric(strsplit(YYYY, "Year")[[1]][2])
          tb  <- tableAll[[YYYY]]
          tb[, c("Year", "climateModel") := list(y, climateModel)]
          return(tb)
        }))
      tableAll <- condTB
    }

  yaxis <- if (timeSpan == "annual") "annualLambda" else "growth"
  yaxisName <- yaxis

  tableAll[, minRib := min(get(paste0(yaxis, "Min"))), by = c("Year", "Herd", "Replicate", "DisturbanceScenario",
                                                              "populationScenario", 
                                                              "climateModel", "femSurvMod_recrMod")]
  tableAll[, maxRib := max(get(paste0(yaxis, "Max"))), by = c("Year", "Herd", "Replicate", "DisturbanceScenario",
                                                              "populationScenario", 
                                                              "climateModel", "femSurvMod_recrMod")]
  tableAll[, paste0("average", yaxis) := mean(get(yaxis)), by = c("Year", "Herd", "Replicate", "DisturbanceScenario",
                                                                  "populationScenario", 
                                                                  "climateModel", "femSurvMod_recrMod")]
  if (!is.null(whichPolysToIgnore)){
    tableAll <- tableAll[!Herd %in% whichPolysToIgnore, ]
  }

  useReps <- if (length(unique(tableAll$Replicate)) > 1) TRUE else FALSE
  yrReady <- lapply(X = unique(tableAll[["area"]]),
                    FUN = function(shp){
                      polyReady <- lapply(X = unique(tableAll[area == shp, femSurvMod_recrMod]),
                                          FUN = function(mod){
                                            message(paste0("Plotting caribou population growth for ", shp,
                                                           " for ", mod))
                                            DT <- tableAll[area == shp & femSurvMod_recrMod == mod, ]
                                            survMod <- strsplit(strsplit(mod, "::")[[1]][1], "_National")[[1]][1]
                                            recMod <- strsplit(strsplit(mod, "::")[[1]][2], "_National")[[1]][1]

                                            tryCatch(quickPlot::clearPlot(), error = function(e){
                                              message(crayon::red("quickPlot::clearPlot() failed"))
                                            })
                                            if (unique(DT[["area"]]) == "metaHeards"){
                                              DT[Herd == "Dehcho North_v2", Herd := "Dehcho North"]
                                              DT[Herd == "Dehcho South_v2", Herd := "Dehcho South"]
                                              DT[, Herd := factor(Herd,
                                                                  levels = c("GSA North", "GSA South",
                                                                             "Dehcho North", "Dehcho South",
                                                                             "Hay River Lowlands"))]
                                            }
                                            popModelPlot <- ggplot2::ggplot(data = DT, aes(x = Year,
                                                                                           colour = Herd,
                                                                                           group = DisturbanceScenario)) +
                                              geom_line(linewidth = 0.7, aes(y = get(paste0("average", yaxis)),
                                                                        group = DisturbanceScenario,
                                                                        linetype = DisturbanceScenario)) +
                                              facet_grid(rows = vars(Herd), cols = vars(populationScenario)) +
                                              geom_hline(yintercept = 1, linetype = "dotted",
                                                         color = "grey73", linewidth = 1) +
                                              geom_ribbon(aes(ymin = minRib,
                                                              ymax = maxRib,
                                                              group = DisturbanceScenario,
                                                              fill = Herd), alpha = 0.3, colour = NA) +
                                              theme_linedraw() +
                                              # ggtitle(label = paste0("Caribou population dynamics: ", DisturbanceScenario),
                                              #         subtitle = paste0("Female Survival Model: ", survMod,
                                              #                           "\nRecruitment Model: ", recMod)) +
                                              theme(legend.position = "bottom",
                                                    title = element_blank(),
                                                    strip.text.y = element_blank(),
                                                    legend.key = element_blank(),
                                                    legend.title = element_blank(),
                                                    axis.title = element_text(family = "Arial")) +
                                              ylab(expression(Mean~annual~lambda)) +
                                              xlab("year")
                                            if (useReps) {
                                              popModelPlot <- popModelPlot + geom_jitter(data = DT, aes(x = Year,
                                                                                                        y = get(yaxis)),
                                                                                         size = 0.5, colour = "grey40",
                                                                                         width = 0.7)
                                            }

                                            if(currentTime == endTime){
                                              tryCatch(quickPlot::clearPlot(),
                                                       error = function(e){
                                                         message(crayon::red("quickPlot::clearPlot() failed"))
                                                       })
                                              png(file.path(outputFolder,
                                                            paste0("caribou_", shp, "_", paste(scenarios, collapse = "_"),
                                                                   "_", recMod,"_", survMod,
                                                                   ifelse(!is.null(resultsMainFolder), "_reps", ""),
                                                                   ".png")),
                                                  units = "cm", res = 300,
                                                  width = 29, height = 21)
                                              print(popModelPlot)
                                              dev.off()
                                            }
                                            return(popModelPlot)
                                          })
                      names(polyReady) <- unique(tableAll[area == shp, femSurvMod_recrMod])
                      return(polyReady)
                    })
  names(yrReady) <- unique(tableAll[["area"]])
  return(yrReady)
}


#' substrBoth get a sub-string based on the number of characters and the side to start
#'
#' @param strng String from which to grab a subset
#' @param howManyCharacters numeric. How many characters should be returned in the sub-string?
#' @param fromEnd logical. Default is TRUE. Should te subset start in the end of the string?
#'
#' @return character string of the subset.
#'
#' @author Tati Micheletti
#' @export
#'
#' @rdname substrBoth
substrBoth <- function(strng, howManyCharacters, fromEnd = TRUE) {
  if (fromEnd) return(substr(x = strng, start = nchar(strng) - howManyCharacters+1, nchar(strng))) else
    return(substr(x = strng, start = 1, stop = howManyCharacters))
}
