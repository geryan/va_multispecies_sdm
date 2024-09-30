library(targets)
library(geotargets)

tar_option_set(
  packages = c(
    "tibble",
    "dplyr",
    "sdmtools",  # remotes::install_github("idem-lab/sdmtools)
    "readr",
    "tidyr",
    "terra",
    "ggplot2",
    "geotargets", # install.packages("geotargets", repos = c("https://njtierney.r-universe.dev", "https://cran.r-project.org"))
    "multispeciesPP", # remotes::install_github("wfithian/multispeciesPP")
    "idpalette", # remotes::install_github("idem-lab/idpalette)
    "rasterVis",
    "tidyterra",
    "geodata"
  ),
  workspace_on_error = TRUE
)

tar_source(files = "R")


list(
  #########################
  # spatial data
  ########################

  # spatial data preprocessing in
  # https://github.com/geryan/africa_spatial_data


  # aggregated predictor variables, i.e., lower resolution images for quicker processting
  # but use the high res ones for final product
  # # mech layer is set to the minimum value above zero
  tar_terra_rast(
    static_vars_agg_mech_nonzero,
    rast("data/raster/static_vars_agg_mech_nonzero.tif")
  ),

  tar_terra_rast(
    new_mask,
    rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/new_mask.tif")
  ),


  # tar_terra_rast(
  #   static_vars_standardised,
  #   rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars_std.tif")
  # ),
  #
  #
  # tar_terra_rast(
  #   unstandardised_layers,
  #   rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars.tif")
  # ),

  tar_seed_set(
    tar_seed_create("bg_points")
  ),

  tar_target(
    bg_points,
    terra::spatSample(
      x = static_vars_agg_mech_nonzero[[1]],
      size = 30000,
      na.rm = TRUE,
      as.points = TRUE
    ) %>%
      crds()
  ), #mfing slow but setting of seed stops it rerunning # upped number and not that mfing slow???



  ####################################
  # data wrangling and cleaning
  ###################################
 tar_target(
   raw_data_file,
   "data/tabular/final_species_20240314.csv",
   format = "file"
 ),
 tar_target(
   raw_data,
   read_csv(
    file = raw_data_file,
    guess_max = 30000
   )
 ),

 tar_target(
   data_records,
   make_data_records(
     raw_data,
     static_vars_agg_mech_nonzero[[1]]
   )
 ),

 tar_target(
   target_species,
   c(
     "gambiae_complex",
     "arabiensis",
     "funestus",
     "gambiae",
     "pharoensis",
     "coluzzii",
      "melas",
      "nili",
      "merus",
      "moucheti"
   )
 ),


 tar_target(
   model_data_ragged,
   make_model_data_ragged(
     data_records,
     bg_points,
     target_species
   )
 ),

 tar_target(
   model_notna_idx_pa,
   get_notna_idx(
     model_data_ragged,
     type = "pa"
   )
 ),

 tar_target(
   model_notna_idx_po,
   get_notna_idx(
     model_data_ragged,
     type = "po"
   )
 ),

 tar_target(
   spatial_values,
   get_spatial_values(
     lyrs = static_vars_agg_mech_nonzero,
     dat = model_data_ragged,
     bgs = bg_points
   )
 ),

 ## plots

 tar_target(
   covs_plots,
   make_covariate_plots(
     spatial_values,
     target_species,
     model_notna_idx_pa,
     model_notna_idx_po
   )
 )


 ## models




)





# tar_load_everything()
# tar_load_globals()
