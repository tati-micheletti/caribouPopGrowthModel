#' Extracts disturbance of anthropogenic and fire
#'
#' @param caribouShapefile Shapefile with polygons for which we want to calculate lambda for the
#'                         caribou demographic model.
#' @param recoveryTime numeric. Recovery time in years that the forest needs to support Caribou.
#'                     Default = 40.
#' @param bufferedAnthropogenicDisturbance500m Anthropogenic disturbance (raster) layer. Currently, road density layer
#'                           used for both RSF and demographic models.
#' @param waterRaster Raster layer indicating water bodies.
#' @param fireLayer Raster of historical year fires (> 40 years) already added with current year's fire.
#' @param rasterToMatch RasterLayer template for these layers to match.
#' @param destinationPath TODO
#'
#' @return A list of the anthropogenic and fire disturbances as percent (0-100)
#'
#' @author Tati Micheletti
#' @export
#' @include calculatePixelsInaRule.R
#' @importFrom crayon red
#' @importFrom raster raster stack
#' @importFrom reproducible postProcess
#' @rdname extractDisturbanceFast
extractDisturbanceFast <- function(shapefileName,
                                   caribouShapefile,
                                   recoveryTime = 40,
                                   currentTime,
                                   bufferedAnthropogenicDisturbance500m = NULL,
                                   waterRaster,
                                   funToLapply,
                                   fireLayer,
                                   makeAssertions = TRUE,
                                   rasterToMatch,
                                   destinationPath) {
  
  message("Calculating disturbance for ", shapefileName)
  caribouShapefile <- caribouShapefile[[shapefileName]]

  # Make sure that the layers align. This will only happen once until the anthropogenic layer becomes dynamic.
  areStackable <- tryCatch({
    invisible(raster::stack(waterRaster, bufferedAnthropogenicDisturbance500m, rasterToMatch))
    TRUE
  }, error = function(e){
    return(FALSE)
  })
  if (!areStackable){
    waterRaster <- reproducible::postProcess(x = waterRaster,
                                             rasterToMatch = rasterToMatch,
                                             destinationPath = destinationPath,
                                             filename2 = NULL,
                                             userTags = c("module:caribouPopGrowthModel",
                                                          "objectName:waterRaster",
                                                          "outterFun:postProcess"))
    if (!is.null(bufferedAnthropogenicDisturbance500m)){
      bufferedAnthropogenicDisturbance500m <- reproducible::postProcess(x = bufferedAnthropogenicDisturbance500m,
                                                                        rasterToMatch = rasterToMatch,
                                                                        maskWithRTM = TRUE,
                                                                        destinationPath = destinationPath,
                                                                        filename2 = NULL,
                                                                        userTags = c("module:caribouPopGrowthModel",
                                                                                     "objectName:bufferedAnthropogenicDisturbance500m",
                                                                                     "outterFun:postProcess"))
    }
    areStackable <- tryCatch({
      invisible(raster::stack(waterRaster, bufferedAnthropogenicDisturbance500m, rasterToMatch))
      TRUE
    }, error = function(e){
      return(FALSE)
    })
    if (!areStackable) stop(paste0("Something went wrong with the layers for extraction",
                                   " of disturbance (caribouPopGrowthModel). Please debug."))
  }
  # Check for anthropogenic layer and make sure it aligns with the others
  if (!is.null(bufferedAnthropogenicDisturbance500m)){
    # Convert NA background of lineDensity/anthropogenic disturbance to 0, while water stays as NA

#### NOTE:  ON 26JAN21 CHERYL JOHNSON CONFIRMED THEY DO NOT REMOVE ANYTHING FROM THE LAYERS,
#### SO WE SHOULD NOT MASK EITHER
    # backgroundWithoutWater <- rasterToMatch
    # backgroundWithoutWater[waterRaster[] == 1] <- NA 
    # backgroundWithoutWater[!is.na(backgroundWithoutWater)] <- 0
    
    # Fix anthropogenic layer that is NA/1 only. Add zeros to all pixels that are water, 
    # basically (which is where we can't build anything)
    # backgroundWithoutWater[bufferedAnthropogenicDisturbance500m[] == 1] <- 1
    # bufferedAnthropogenicDisturbance500m <- backgroundWithoutWater
    bufferedAnthropogenicDisturbance500m[is.na(rasterToMatch[])] <- NA # REMOVING ONLY OUTSIDE SA
  } else {
    message(crayon::red("bufferedAnthropogenicDisturbance500m is NULL. 
                        The prediction will assume anthropogenic disturbances do not exist"))
    bufferedAnthropogenicDisturbance500m[rasterToMatch[] == 1] <- 0
  }
  
  # Extract the caribou shapefile values by fasterizing it. Way faster than raster::extract
  message(crayon::blue("Fasterizing caribou shapefile..."))
  caribouShapefile <- Cache(reproducible::postProcess, x = caribouShapefile,
                            rasterToMatch = rasterToMatch,
                            destinationPath = destinationPath,
                            filename2 = "caribouShapefile",
                            userTags = c("module:caribouPopGrowthModel",
                                         "objectName:caribouShapefile",
                                         "outFun:Cache"))
  caribouShapefileSF <- sf::st_as_sf(caribouShapefile)
  nm <- if (!is.null(caribouShapefile$NAME)){
    "NAME"
  } else {
    if (!is.null(caribouShapefile$Name)) {
      "Name"
    } else {
      if (!is.null(caribouShapefile$HERD)){
        "HERD"
      } else {
NULL
    }
  }
  }
  if (is.null(nm))
    stop(paste0("The shapefile ", shapefileName, " does not have a field named ",
                "'NAME' or 'Name'. Please add that to it and run the ",
                "simulation again"))

  caribouShapefileSF$ID <- as.numeric(seq(1:length(caribouShapefileSF[[nm]])))
  caribouShapefileRas <- fasterize::fasterize(sf = caribouShapefileSF,
                                              raster = rasterToMatch,
                                              field = "ID")

  # Remove the ID's that are not in the rasterized version of the sA (because they are too small)
  availableInRas <- na.omit(unique(caribouShapefileRas[]))
  polsToRemove <- setdiff(caribouShapefileSF[["ID"]], availableInRas)
  caribouShapefileSF <- caribouShapefileSF[!caribouShapefileSF$ID %in% polsToRemove,]

  ########### START EXTRACTION OF DATA ##################### 

  listExtr <- do.call(what = funToLapply, args = alist(X = caribouShapefileSF[["ID"]],
                                                       FUN = .extractDisturbancePolygon,
                                                       caribouShapefile = caribouShapefile,
                                                       bufferedAnthropogenicDisturbance500m = bufferedAnthropogenicDisturbance500m,
                                                       makeAssertions = makeAssertions,
                                                       fireLayer = fireLayer,
                                                       caribouShapefileRas = caribouShapefileRas,
                                                       nm = nm
                                                       ))
  #Naming both fire and anthro disturbances
  names(listExtr) <- caribouShapefileSF[[nm]]
  return(listExtr)
}
