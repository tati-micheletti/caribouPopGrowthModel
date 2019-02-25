getDisturbance <- function(currentTime = time(sim),
                           startTime = start(sim),
                           endTime = end(sim),
                           cumulBurn = sim$cumulBurn,
                           recoveryTime = P(sim)$recoveryTime){
                               
  originalTime <- currentTime
  if (startTime > 1){
    relEndTime <- endTime - startTime
    currentTime <- originalTime - startTime
  }
  
  # -9999: year zero from spinnup
  # -8888: year zero from simul. We should use the current year from simul
  currTable <- table(cumulBurn[])
  # We are also using 'recoveryTime - 1' because it is inclusive (i.e. length(-39:0) == 40)
  yearsToUse <- as.character(c((currentTime-(recoveryTime-1)):currentTime))
  yearsToUse[yearsToUse == "0"] <- "-8888"
  vals <- currTable[yearsToUse]
  cummFire <- sum(vals, na.rm = TRUE)
  totPixels <- as.numeric(currTable["0"]) + cummFire
  
  name <- paste0("Year", originalTime)
  value <- 100*(cummFire/totPixels)
  DH_Tot <- list(value) 
  names(DH_Tot) <- name
  # Calc is done in percentage of the area
  return(DH_Tot)
}