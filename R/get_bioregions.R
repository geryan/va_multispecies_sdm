get_bioregions <- function(project_mask, vect = FALSE){

  # opens OneEarth Bioregions downloaded from
  # https://www.oneearth.org/datasets/
  # One Earth Bioregions Framework, One Earth. {2023} Los Angeles, CA. oneearth.org

  v <- vect("data/one_earth-bioregions-2023.geojson") |>
    crop(project_mask)

  if(vect){
    return(v)
  }

  v |>
    rasterize(
      y = project_mask,
      field = "Bioregions"
    ) |>
    mask(project_mask)

}
