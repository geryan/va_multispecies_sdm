get_soil_af <- function(
    var = "clay"
){

  soil_raw <- geodata::soil_af(
    var = var,
    depth = 5,
    path = "data/raster/geodata"
  )

  srvals <- values(soil_raw)

  soil_raw_filled <- soil_raw

  soil_raw_filled[] <- ifelse(is.na(srvals), 0, srvals)

  soil_raw_filled

}
