.extractDisturbancePolygon <- function(pol, caribouShapefile,
                                       bufferedAnthropogenicDisturbance500m,
                                       makeAssertions,
                                       fireLayer,
                                       caribouShapefileRas,
                                       nm){
  polygonName <- caribouShapefile[[nm]][pol]
  print(paste0("Calculating disturbances for polygon ", polygonName))
  # 1. Make sure the layers are all masked for the study area to avoid problems
  # with NA's. The anthropogenic layer has been treated to remove water. We will
  # use it as template for the fire layer
  templateRaster <- bufferedAnthropogenicDisturbance500m
  templateRaster[!is.na(templateRaster[])] <- 0
  # Data sanity check: do I have burns where it is not possible (i.e. water)?
  if (makeAssertions) {
    templateRasterWrongFires <- templateRaster
    templateRasterWrongFires[fireLayer[] > 0 & is.na(templateRaster[])] <- 1
    tb <- table(templateRasterWrongFires[])
    if (!is.na(tb["1"])){
      percWrongFires <- 100*(tb["1"]/(tb["1"]+tb["0"]))
      if (percWrongFires > 3){
        warning(paste0("Wrong fire percentage (i.e. fires in",
                       " water pixels) is higher than 3% (", percWrongFires,
                       ") for ", polygonName,". Make sure fire data is correct"),
                immediate. = TRUE)
      }
    }
  }
  fireLayer[is.na(templateRaster)] <- NA
  
  #~~~~~~~~~~~~~~~~ ANTHROPOGENIC ~~~~~~~~~~~~~~~~#
  #
  # ::: 1. Anthro: % of non-overlapping anthropogenic disturbance
  #                (buffered by 500 m; reservoirs removed)
  #
  # From the anthropo layer we calculate for each polygon the total amount of pixels that had disturbances
  # over the total number of pixels "available" to have it (non-NA when na is JUST WATER).
  # Then multiply by 100 to work with %.

  percentAnthopo <- calculatePixelsInaRule(ras = bufferedAnthropogenicDisturbance500m,
                                           rule = "> 0", # Need to be a character string of the rule
                                           pol = pol,
                                           shp = caribouShapefileRas)
  
  if (percentAnthopo$percentDisturbance < 0 | percentAnthopo$percentDisturbance > 100){
    print("Something went wrong with the anthropogenic distubance calculation. 
            Value is either negative or above 100%. Please debug.")
    browser()
  }
  
  #~~~~~~~~~~~~~~~~ FIRE ~~~~~~~~~~~~~~~~#
  #
  # ::: 2. Fire: % of non-overlapping fires ≤40 years old
  #              (unbuffered)
  #              
  # From the burn polygons map calculate for each polygon the total amount of pixels that had fires
  # over the total number of pixels "available" to burn (non-NA, as NA can be cities/water/etc).
  # Then multiply by 100 to work with %.
  percentFire <- calculatePixelsInaRule(ras = fireLayer,
                                        rule = "> 0", # Need to be a character string of the rule
                                        pol = pol,
                                        shp = caribouShapefileRas)
  
  if (percentFire$percentDisturbance < 0 | percentFire$percentDisturbance > 100){
    print("Something went wrong with the fire distubance calculation. 
            Value is either negative or above 100%. Please debug.")
    browser() # Check pol
  }  # Data sanity check
  if (is.na(percentFire$percentDisturbance)){
    stop(paste0("Percent disturbance from fire is NA. Something went wrong.",
                "Please investigate the creation of fireLayers (module:caribouPopGrowthModel; ",
                "function: composeFireRaster())"))
  }
  
  # Data sanity check
  # The number of NA's in fire needs to be the same or smaller than anthropo layers
  testthat::expect_true(round(percentFire[["totPixelsNotNA"]], 5) <= round(percentAnthopo[["totPixelsNotNA"]], 5))
  totPixelsNotNADist <- percentFire[["totPixelsNotNA"]] # Total number of pixels that
  
  # ::: 3. fire_excl_anthro: Fire - Anthro
  
  # I will sum all the pixels that are fire disturbed paired with not anthropogenically disturbed
  # and I will divide by the minimum number of not NA because the fire layer is NOT masked to studyArea
  # which makes it has no NA's. Outside of the sA doesn't count, so we need to exclude those pixels.
  trueFireFalseAnthro <- percentFire$isDisturbance & !percentAnthopo$isDisturbance
  fire_excl_anthro <- 100*(sum(trueFireFalseAnthro)/
                             totPixelsNotNADist)
  # Data sanity check
  if (makeAssertions)
    testthat::expect_true(round(fire_excl_anthro, 5) <= round(percentFire$percentDisturbance, 5))
  
  # ::: 4. Total_dist: Fire + Anthro, % of total disturbance (non-overlapping buffered
  #                    anthro. + unbuffered ≤ 40 year old fires as a single
  #                    variable)
  
  # Need to overlay the disturbances and extract the total
  isDistrubance <- percentFire$isDisturbance | percentAnthopo$isDisturbance
  Total_dist <- 100*(sum(isDistrubance, na.rm = TRUE)/totPixelsNotNADist)
  
  if (makeAssertions){
    if (Total_dist > 100){
      stop("Total disturbance is higher than 100%, something went wrong")
    }  
    if (Total_dist < 0 ){
      print("Something went wrong with the total distubance calculation.
Value is negative. Please debug.")
      browser()
    }
    
    testthat::expect_true(round(Total_dist, 5) <= round(percentFire$percentDisturbance + 
                                           percentAnthopo$percentDisturbance + 0.01, 5))
    # Added a 0.01 because in some cases, when there is vritually no overlap  
    # between the two disturbances, because fire is the one driving the number of non-NA's
    # (i.e. so we don't underestimate burned area ==> overestimate caribou!) and this might be
    # lower than anthropogenic (i.e. some places can't burn, like rocks/ice, but you might be
    # able to have development!), you end up getting the Total_dist being a centesimal larger 
    # than the sum of the percents.
  }
  
  # ::: 5. fire_prop_dist: ≤40 year old fires as a proportion of total
  #                        disturbance
  # This assumes that a pixel that has both fire and anthropogenic disturbance was not
  # considered to be a fire pixel. (i.e. if an area burned and then we had development,
  # for caribou the area is development, not burned -- it can't/won't use it)
  fire_prop_dist <- 100*(sum(trueFireFalseAnthro)/sum(isDistrubance, na.rm = TRUE))
  
  # The alternative way would likely overestimate caribou (as it would say more are is
  # burned when it is actually development)
  fireOnTotal <- 100*(percentFire$percentDisturbance/Total_dist)
  # This happens if you don't have any disturbance
  if (!any(is.na(fire_prop_dist), is.na(fireOnTotal)))
    testthat::expect_true(round(fire_prop_dist, 5) <= round(fireOnTotal, 5))
  
  # Making the data.frame
  df <- data.frame(Fire = percentFire$percentDisturbance,
                   Anthro = percentAnthopo$percentDisturbance,
                   Total_dist = Total_dist,
                   fire_excl_anthro = fire_excl_anthro,
                   fire_prop_dist = fire_prop_dist,
                   polygonSizeInPix = percentAnthopo$totPixelsNotNA)
  return(df)
}