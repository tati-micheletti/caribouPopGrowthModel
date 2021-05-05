digestDisturbances <- function(disturbances, Year){
    DTshape <- rbindlist(lapply(names(disturbances), function(shape){
      DTpoly <- rbindlist(lapply(names(disturbances[[shape]]), function(Herd){
        DT <- data.table(disturbances[[shape]][[Herd]])
        DT[, c("Year", "area", "Herd") := list(Year, shape, Herd)]
        return(DT)
      }))
      return(DTpoly)
    }))
  return(DTshape)
}

