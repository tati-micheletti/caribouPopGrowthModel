#' Get the necessary layers for predictive modules such as caribouPopGrowth and caribouRSF.
#'
#' @param currentTime numeric. Current time being used (i.e. \code{time(sim)}).
#' @param recoveryTime numeric. Recovery time in years that the forest needs to support Caribou. Default = 40.
#' @param listSACaribou list of shapefiles with polygons for which we want to calculate lambda for the caribou demographic model.
#' @param bufferedAnthropogenicDisturbance500m Anthropogenic disturbance (raster) layer. Currently, 500m buffered anthropogenic disturbance for demographic models.
#' @param waterRaster Raster layer indicating water bodies.
#' @param rasterToMatch RasterLayer template for these layers to match.
#' @param destinationPath character. Destination path to save layers.
#' @param fireLayer Raster of historical year fires (> 40 years) already added with current year's fire.
#' @param useFuture Should use attempt to use parallel processing? Defaults to TRUE; If in RStudio
#'                  is converted to FALSE
#'
#' @return TODO
#'
#' @author Tati Micheletti
#' @export
#' @importFrom crayon red
#' @importFrom data.table data.table setkey
#' @importFrom raster dropLayer extract projectRaster raster stack
#' @importFrom reproducible prepInputs postProcess
#'
#' @rdname getLayersCaribou
getLayersCaribou <- function(currentTime,
                             recoveryTime = 40,
                             listSACaribou,
                             bufferedAnthropogenicDisturbance500m,
                             waterRaster,
                             fireLayer,
                             useFuture = TRUE,
                             rasterToMatch = NULL,
                             destinationPath) {

  threadsDT <- getDTthreads()
  setDTthreads(1)
  on.exit({setDTthreads(threadsDT)}, add = TRUE)

  # Bring layers to memory to speed up process
  tic("Bringing layers to memory for faster processing: ")
  bufferedAnthropogenicDisturbance500m[] <- bufferedAnthropogenicDisturbance500m[]
  fireLayer[] <- fireLayer[]
  waterRaster[] <- waterRaster[]
  rasterToMatch[] <- rasterToMatch[]
  toc()

  numberShapefiles <- length(names(listSACaribou))
  # get the maximum number of polygons in the shapefiles
  numberPolygons <- max(unlist(lapply(X = listSACaribou, length)))
  totalNeededCores <- numberShapefiles*numberPolygons

  # Test if in RStudio. If so, convert use future to FALSE
  useFuture <- if (isRstudio()) FALSE else useFuture

  # Test for number of cores available. If less than needed, second future becomes
  # sequential!
  avCores <- availableCores()

  if (any(avCores < numberShapefiles, !useFuture)){
    # Don't do parallel!
    if (!useFuture){
      message(paste0("useFuture is FALSE. \nSetting sequential process."))
    } else {
      message(paste0("Number of cores detected: ", avCores,
                     " \nNumber of cores needed for level 1 parallel: ", numberShapefiles,
                     ". \nSetting sequential process."))
      useFuture <- FALSE
    }
  } else {
    if (avCores < totalNeededCores){
      # Protect from blasting!
      message(paste0("Number of cores detected: ", avCores,
                     " \nNumber of cores needed for level 2 parallel: ", totalNeededCores,
                     ". \nSetting level 1 parallel process (", numberShapefiles,"cores)."))
      plan(list(tweak(multiprocess, workers = numberShapefiles)),
           tweak(sequential, workers = 1))
    } else {
      # Go full parallel!
      message(paste0("Number of cores detected: ", avCores,
                     " \nNumber of cores needed for level 2 parallel: ", totalNeededCores,
                     ". \nGoing full parallel processing (", totalNeededCores," cores)."))
      plan(list(tweak(multiprocess, workers = numberShapefiles)),
           tweak(multiprocess, workers = numberPolygons))
    }
  }

  funToLapply <- if (!useFuture) lapply else future_lapply
  listDistForEachShpForEachPoly <- do.call(what = funToLapply, args = alist(X = names(listSACaribou),
                                                                           FUN = extractDisturbanceFast,
                                                                           caribouShapefile = listSACaribou,
                                                                           recoveryTime = recoveryTime,
                                                                           currentTime = currentTime,
                                                                           fireLayer = fireLayer,
                                                                           funToLapply = funToLapply,
                                                                           bufferedAnthropogenicDisturbance500m = bufferedAnthropogenicDisturbance500m,
                                                                           waterRaster = waterRaster,
                                                                           rasterToMatch = rasterToMatch,
                                                                           destinationPath = destinationPath))
  plan(sequential)
  names(listDistForEachShpForEachPoly) <- names(listSACaribou)
  return(listDistForEachShpForEachPoly)
}
