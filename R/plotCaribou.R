plotCaribou <- function(startTime = P(sim)$.plotInitialTime,
                        currentTime = time(sim),
                        predictedCaribou = sim$predictedCaribou){
  
  reproducible::Require(ggplot2) # [ FIX ] When we develop the rest of the models: need to lapply through
  
  orderedRasterList <- lapply(X = 1:length(predictedCaribou), FUN = function(index){
    sbset <- unlist(lapply(predictedCaribou[[index]], tail, n=1L), use.names = FALSE)
    return(sbset)
  })
  
  popTable <- unlist(lapply(orderedRasterList, tail, n = 1L))

  populationCaribou <- data.frame(Time = as.integer(startTime:currentTime),
                                  CaribouPopulationSize = as.integer(popTable))
  quickPlot::clearPlot()
  
  plotsCarib <- ggplot2::ggplot(data = populationCaribou, aes(x = Time, 
                                                              y = CaribouPopulationSize)) + 
    geom_line()
  
  return(plotsCarib)
  
}