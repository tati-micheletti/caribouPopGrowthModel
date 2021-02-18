plotCaribouPopGrowth <- function(startTime,
                                 currentTime,
                                 endTime,
                                 resultsMainFolder = NULL, # Pass this if outside of module
                                 climateModel = NULL,
                                 predictedCaribou = NULL,
                                 yearSimulationStarts,
                                 reps = paste0("run", 1:5),
                                 outputFolder,
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
    pth <- file.path(resultsMainFolder, paste(climateModel, reps, sep = "_"))
    
    predictedCaribou <- lapply(seq_along(pth), function(filePath){
      tb <- readRDS(list.files(path = pth[filePath], 
                               pattern = paste0("predictedCaribou_year", currentTime), 
                               full.names = TRUE, recursive = TRUE))
      addedTB <- lapply(tb, function(TB){
        TB[, Replicate := paste0("run", 1:5)[filePath]]
        return(TB)
      })
      return(addedTB)
    })
    names(predictedCaribou) <- paste0("run", 1:5)
    
    predictedCaribouOrganized <- lapply(X = 1:unique(lengths(predictedCaribou)), 
                                        FUN = function(index){
                                          sbset <- rbindlist(lapply(predictedCaribou, `[[`, index))
                                          return(sbset)
                                        })
    names(predictedCaribouOrganized) <- paste0("Year", c(seq(startTime, currentTime, by = 10), currentTime))
    predictedCaribou <- predictedCaribouOrganized
  }

  if (is(predictedCaribou, "list")){
    tableAll <- data.table::rbindlist(lapply(X = names(predictedCaribou), FUN = function(yr){
      y <- usefulFuns::substrBoth(strng = yr, howManyCharacters = 4, fromEnd = TRUE)
      predictedCaribou[[yr]][, Year := as.numeric(y)]
      return(predictedCaribou[[yr]])
    }))
  } else {
    tableAll <- predictedCaribou
  }
  
  yaxis <- if (timeSpan == "annual") "annualLambda" else "growth"
  yaxisName <- yaxis
  
  names(tableAll)[names(tableAll) == "polygon"] <- "Polygon"
  tableAll[, minRib := min(get(paste0(yaxis, "Min"))), by = c("Year", "Polygon", "femSurvMod_recrMod")]
  tableAll[, maxRib := max(get(paste0(yaxis, "Max"))), by = c("Year", "Polygon", "femSurvMod_recrMod")]
  tableAll[, paste0("average", yaxis) := mean(get(yaxis)), by = c("Year", "Polygon", "femSurvMod_recrMod")]

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
            
            popModelPlot <- ggplot2::ggplot(data = DT, aes(x = Year,
                                                           y = get(paste0("average", yaxis)),
                                                           colour = Polygon, 
                                                           group = 1)) +
              geom_line(size = 1.2) +
              facet_grid(rows = vars(Polygon)) +
              geom_hline(yintercept = 1, linetype = "dotted", 
                         color = "grey73", size = 1) +
              geom_line(size = 1.2) +
              geom_ribbon(aes(ymin = minRib, 
                              ymax = maxRib,
                              fill = Polygon), alpha = 0.3, colour = NA) +
              ggtitle(label = paste0("Caribou population dynamics: ", climateModel),
                      subtitle = paste0("Female Survival Model: ", survMod,
                                        "\nRecruitment Model: ", recMod)) +
              theme(legend.position = "bottom",
                    strip.text.y = element_blank(),
                    legend.key = element_blank(),
                    legend.title = element_blank()) +
              ylab(yaxisName)
            if ("Replicate" %in% names(DT)){
              popModelPlot <- popModelPlot + geom_jitter(data = DT, aes(x = Year,
                                                                        y = get(yaxis)),
                                                         size = 1, colour = "grey40",
                                                         width = 1)
            }
            
            if(currentTime == endTime){
              tryCatch(quickPlot::clearPlot(), 
                       error = function(e){
                         message(crayon::red("quickPlot::clearPlot() failed"))
                         })
              png(file.path(outputFolder, 
                            paste0("caribou_", shp, "_", climateModel,
                                   "_", recMod,"_", survMod,
                                   ifelse(!is.null(resultsMainFolder), "_reps", ""),
                                   ".png")), 
                  width = 700, height = 480)
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
