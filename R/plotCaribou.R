plotCaribou <- function(startTime = P(sim)$.plotInitialTime,
                        currentTime = time(sim),
                        predictedCaribou = sim$predictedCaribou){
  
  reproducible::Require(ggplot2) # [ FIX ] When we develop the rest of the models: need to lapply through
  orderedRasterList <- lapply(X = 1:length(predictedCaribou), FUN = function(index){
    sbset <- lapply(predictedCaribou[[index]], FUN = function(pop){
      resp <- list(population = pop[["currentPopUpdated"]], lambda = pop[["newLambda"]])
      return(resp)
    })
  
    return(sbset)
  })
  
  popTable <- unlist(lapply(orderedRasterList, FUN = function(pop2) {
    resp <- list(population = pop2[["currentPopUpdated"]], lambda = pop2[["newLambda"]])
    return(popTable)
  }))

  populationCaribou <- data.frame(Time = as.integer(startTime:currentTime),
                                  CaribouPopulationSize = as.integer(popTable))
  quickPlot::clearPlot()
  
  plotsCarib <- ggplot2::ggplot(data = populationCaribou, aes(x = Time, 
                                                              y = CaribouPopulationSize)) + 
    geom_line()
  
  return(plotsCarib)
  
}