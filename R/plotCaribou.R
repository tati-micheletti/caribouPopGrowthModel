plotCaribou <- function(startTime = start(sim),
                        currentTime = time(sim),
                        predictedCaribou = sim$predictedCaribou){
  
  reproducible::Require(ggplot2) # [ FIX ] When we develop the rest of the models: need to lapply through
  
  orderedRasterList <- lapply(X = 1:length(predictedCaribou), FUN = function(index){
    sbset <- unlist(lapply(predictedCaribou[[index]], tail, n=1L), use.names = FALSE)
    return(sbset)
  })
  
  popTable <- unlist(lapply(orderedRasterList, tail, n = 1L))

  if ((length(popTable) != length(startTime:currentTime)) & (startTime == 0)){
    time <- as.integer(startTime + 1:currentTime) 
  } else {
    time <- as.integer(startTime:currentTime)
    }
  
  populationCaribou <- data.frame(Time = time,
                                  CaribouPopulationSize = as.integer(popTable))
  quickPlot::clearPlot()
  
  plotsCarib <- ggplot2::ggplot(data = populationCaribou, aes(x = Time, 
                                                              y = CaribouPopulationSize)) + 
    geom_line()
  
  return(plotsCarib)
  
}