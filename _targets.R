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

  # data wrangling and cleaning
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
   va_data,
   tidy_va_data(raw_data)
 ),
 tar_target(
   record_data_all,
   records_from_va(va_data)
 ),
 tar_target(
   data_records,
   filter_few(record_data_all) |>
     filter(species != "gambiae_complex")
   # potentially remove later or put elsewhere
 ),


 # summary stats and figures
 tar_target(
   records_table,
   make_records_table(data_records)
 ),
 tar_target(
   record_plot_data,
   make_record_plot_data(data_records)
 ),
 tar_target(
   record_plot,
   make_record_plot(record_plot_data)
 ),
 tar_target(
   record_plot_file,
   ggsave(
     filename = "outputs/figures/record_count_plot.png",
     plot = record_plot,
     width = 2400,
     height = 1500,
     units = "px"
   )
 ),


 # spatial data prep done in separate project africa_spatial_data.Rproj
 # tar_target(
 #   mask_name,
 #   "data/raster/africa_mask.tif",
 #   format = "file"
 # ),
 # geotargets::tar_terra_rast(
 #   africa_mask,
 #   prepare_mask(mask_name)
 # ),
 # geotargets::tar_terra_rast(
 #   covariate_rasters,
 #   prepare_covariates(africa_mask)
 # ),
 # tar_terra_rast(
 #   new_mask,
 #   make_new_mask(covariate_rasters)
 # ),
 # tar_target(
 #   bias_name,
 #   "/Users/gryan/Documents/tki_work/vector_atlas/africa_anopheles_sampling_bias/outputs/travel_time.tif", # travel time to research station
 #   #"~/Documents/tki_work/vector_atlas/vector_sdm_course/data/downloads/travel_time_to_cities_2.tif", # travel time to city
 #   format = "file"
 # ),
 # geotargets::tar_terra_rast(
 #   bias,
 #   prepare_bias(
 #     new_mask,
 #     bias_name
 #   ) |>
 #     standardise_rast()
 # ),
 # tar_terra_vect(
 #   new_mask_v,
 #   as.polygons(new_mask)
 # ),
 #
 # geotargets::tar_terra_rast(
 #   model_layers,
 #   c(covariate_rasters, bias)
 # ),

 tar_terra_rast(
   new_mask,
   rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/new_mask.tif")
 ),

 tar_terra_rast(
   model_layers,
   rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars_std.tif")
 ),

 tar_terra_rast(
   unstandardised_layers,
   rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars.tif")
 ),

 tar_seed_set(
   tar_seed_create("bg_points")
 ),

 tar_target(
   bg_points,
   terra::spatSample(
     x = new_mask,
     size = 30000,
     na.rm = TRUE,
     as.points = TRUE
   ) %>%
     crds()
 ), #mfing slow but setting of seed stops it rerunning # upped number and not that mfing slow???


 # model data collation and fitting
 tar_target(
   mpp_data,
   format_mpp_data(
     records = data_records,
     background = bg_points,
     modlyr = model_layers
   )
 ),

 tar_target(
   region_size,
   sum(!is.na(values(model_layers[[1]])))
 ),


  ### Model 1
  tar_target(
    mpp_fit_1,
    multispeciesPP(
      sdm.formula = ~ arid +
        built_volume +
        cropland +
        elevation +
        evi_mean +
        footprint +
        lst_day_mean +
        lst_night_mean +
        pop +
        pressure_mean +
        rainfall_mean +
        soil_clay +
        solrad_mean +
        surface_water +
        tcb_mean +
        tcw_mean +
        windspeed_mean +
        easting +
        northing,
      bias.formula = ~ research_tt_by_country,
      PA = mpp_data$pa,
      PO = mpp_data$po,
      BG = mpp_data$bg,
      region.size = region_size,
      quadrat.size = 1
    )
  ),
  tar_terra_rast(
    preds_1,
    predict_mpp_rast_all(
      model = mpp_fit_1,
      data = model_layers,
      filename = "outputs/preds_1.tif",
      overwrite = TRUE
    )
  ),
  tar_target(
    pred_plot_1,
    levelplot(
      preds_1,
      col.regions = idpalette("idem", 20),
      layout = c(2,2)
    )
  ),
  tar_target(
    pred_plot_file_1,
    sdmtools::save_plot(
      p = pred_plot_1,
      filename = "outputs/figures/pred_plot_1.png",
      width = 2400,
      #height = 1500,
      units = "px",
      res = 300
    )
  )



)





# tar_load_everything()
# tar_load_globals()
