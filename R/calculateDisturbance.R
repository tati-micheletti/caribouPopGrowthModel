calculateDisturbance <- function(ras,
                                 rule,
                                 pol,
                                 shp, ...){
  dots <- list(...)
  e <- environment()
  lapply(X = seq_along(dots), FUN = function(each){
    assign(x = names(dots)[each], value = dots[[each]], envir = e)
  })
  browser()
  vals <- raster::getValues(x = ras)
  shp[is.na(shp)] <- -1
  polValues <- vals[shp[] == as.numeric(pol)]
  totPixelsNotNA <- sum(!is.na(polValues))
  isRecentDisturbance <- !is.na(polValues) & eval(parse(text = paste0("polValues", rule)))
  cummDisturbance <- sum(isRecentDisturbance, na.rm = TRUE)
  percentDisturbance <- 100*(cummDisturbance/totPixelsNotNA)
  return(list(percentDisturbance = percentDisturbance, 
              isDisturbance = isRecentDisturbance,
              totPixelsNotNA = totPixelsNotNA))
}
