make_expert_offset_maps <- function(
    expert_maps,
    project_mask,
    buffer_km = 1000,
    filename = "outputs/rasters/expert_offset_aggregated.tif"
  ){

  # make aggregated mask because the distance calculation takes effing ages
  # maskagg <- aggregate(project_mask, fact = 10)

  maskagg <- project_mask

  # get species
  spp <- expert_maps$species

  # calculate the distance from the expert map layers across the
  aggdist <- sapply(
    X = spp,
    FUN = function(spp, expert_maps, maskagg){
      terra::distance(
        x = maskagg,
        y = expert_maps |>
          filter(species == spp),
        rasterize = TRUE
      )
    },
    expert_maps,
    maskagg,
    simplify = FALSE
  )

  # convert buffer to m
  buffer_m <- buffer_km * 1e3

  # calculate offset value such that anything outside buffer is 0
  # scaling to 1 inside the expert opinion map
  aggoffset <- sapply(
    aggdist,
    FUN = function(aggdist){
      z <- 1 - aggdist/buffer_m

      z[which(values(z) < 0)] <- 0

      z
    }
  )

  aggoffset <- rast(aggoffset) #|>
    #resample(project_mask)

  writeRaster(
    aggoffset,
    filename = filename,
    overwrite = TRUE
  )

  rast(x = filename)

}
