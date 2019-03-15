plotCaribou <- function(startTime = start(sim),
                        currentTime = time(sim),
                        endTime = end(sim),
                        predictedCaribou = sim$predictedCaribou){
  
  reproducible::Require(ggplot2)
  
  # Year -> Shapefile -> Polygon -> Model -> results
  tableAll <- rbindlist(lapply(X = 1:length(predictedCaribou), FUN = function(yr){ # here I extract the info for all locations and models, make a big table
    yrReady <- rbindlist(lapply(X = 1:length(predictedCaribou[[yr]]), FUN = function(shp){
      shpReady <- rbindlist(lapply(X = 1:length(predictedCaribou[[yr]][[shp]]), function(ply){
        polyReady <- rbindlist(lapply(X = 1:length(predictedCaribou[[yr]][[shp]][[ply]]), function(mod){
          predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]]$Polygon <- names(predictedCaribou[[yr]][[shp]])[[ply]]
          predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]]$CaribouArea <- names(predictedCaribou[[yr]])[[shp]]
          predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]]$Year <- names(predictedCaribou)[[yr]]
          return(predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]])
        }))
        return(polyReady)
      }))
      return(shpReady)
    }))
  return(yrReady)
  }))
  
  if ((length(unique(tableAll$Year)) != length(startTime:currentTime)) & (startTime == 0)){
    time <- as.integer(startTime + 1:currentTime) 
  } else {
    time <- as.integer(startTime:currentTime)
    }
  
  yearTime <- data.table::data.table(Year = unique(tableAll$Year), 
                                     Time = time)
  tableAll <- merge(tableAll, yearTime)

  tryCatch(quickPlot::clearPlot(), error = function(e){message(crayon::red("quickPlot::clearPlot() failed"))})
  
  plts <- lapply(X = unique(tableAll$CaribouArea), function(shp){
    yaxis <- if (unique(tableAll[["populationModel"]]) == "annualLambda") "lambda" else tableAll[["populationModel"]]
    popModelPlot <- ggplot2::ggplot(data = tableAll[CaribouArea == shp], aes(x = Time,
                                                         y = modelParam, 
                                                         colour = Polygon)) +
      geom_line(size = 1.2) +
      ggtitle(paste0("Caribou population dynamics")) +
      theme(legend.position = "bottom") +
      ylab(yaxis)
    
    if(currentTime == endTime){
      
      tryCatch(quickPlot::clearPlot(), error = function(e){message(crayon::red("quickPlot::clearPlot() failed"))})
      pngPath <- checkPath(file.path(getwd(), "outputs"), create = TRUE)
      png(file.path(pngPath, 
                    paste0("caribou", shp, "_", 
                           toupper(format(Sys.time(), "%d%b%y")),".png")), 
          width = 700, height = 480)
      print(popModelPlot)
      dev.off()
    }
    return(popModelPlot)
  })
  
  return(plts)
}
