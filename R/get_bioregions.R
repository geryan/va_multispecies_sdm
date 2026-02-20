get_bioregions <- function(project_mask){

  # opens OneEarth Bioregions downloaded from
  # https://www.oneearth.org/datasets/
  # One Earth Bioregions Framework, One Earth. {2023} Los Angeles, CA. oneearth.org

  vect("data/one_earth-bioregions-2023.geojson") |>
    crop(project_mask) |>
    rasterize(
      y = project_mask,
      field = "Bioregions"
    ) |>
    mask(project_mask)

}
