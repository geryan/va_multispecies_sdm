#' Add one species' range map to the expert maps
#'
#' Everything downstream of `expert_maps` treats it as one feature per species,
#' named in a `species` column: make_expert_offset_maps() computes one distance
#' surface per ROW and names the layer after that row's species, and
#' add_expert_offset() finds a species' offset by that layer name. A map with
#' several features would give several identically named layers, each the full
#' distance calculation over again, so `new_map` is dissolved to a single
#' feature first, then given the same columns as `expert_maps` so the two bind.
#'
#' @param expert_maps SpatVector, one feature per species, as from
#'   get_expert_maps()
#' @param new_map SpatVector range map for one species, any number of features
#' @param species name to give it; must match the name used in the model data
#' @return `expert_maps` with one feature added
#' @author geryan
#' @export
add_expert_map <- function(
    expert_maps,
    new_map,
    species
){

  if (species %in% expert_maps$species) {
    stop(
      sprintf(
        "add_expert_map(): %s already has an expert map",
        species
      ),
      call. = FALSE
    )
  }

  if (!terra::same.crs(expert_maps, new_map)) {
    new_map <- terra::project(
      x = new_map,
      y = expert_maps
    )
  }

  new_map <- terra::aggregate(
    x = new_map,
    dissolve = TRUE
  )

  terra::values(new_map) <- NULL

  # `ID` is the source id of the Sinka maps, which this one does not have
  new_map$ID <- NA_real_
  new_map$species <- species

  rbind(
    expert_maps,
    new_map[, names(expert_maps)]
  )

}
