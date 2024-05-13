library(targets)
library(geotargets)

tar_option_set(
  packages = c(
    "tibble",
    "dplyr",
    "sdmtools",
    "readr",
    "tidyr",
    "terra",
    "ggplot2",
    "geotargets", # install.packages("geotargets", repos = c("https://njtierney.r-universe.dev", "https://cran.r-project.org"))
    "multispeciesPP" # install_github("wfithian/multispeciesPP")
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
     filter(species != "GAMBIAE COMPLEX")
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
     filename = "figures/record_count_plot.png",
     plot = record_plot,
     width = 2400,
     height = 1500,
     units = "px"
   )
 ),


 # spatial data prep
 tar_target(
   mask_name,
   "data/raster/africa_mask.tif",
   format = "file"
 ),
 geotargets::tar_terra_rast(
   africa_mask,
   prepare_mask(mask_name)
 ),
 geotargets::tar_terra_rast(
   covariate_rasters,
   prepare_covariates(africa_mask)
 ),
 tar_target(
   bias_name,
   "/Users/gryan/Documents/tki_work/vector_atlas/africa_anopheles_sampling_bias/outputs/travel_time.tif", # travel time to research station
   #"~/Documents/tki_work/vector_atlas/vector_sdm_course/data/downloads/travel_time_to_cities_2.tif", # travel time to city
   format = "file"
 ),
 geotargets::tar_terra_rast(
   bias,
   prepare_bias(
     africa_mask,
     bias_name
   ) |>
     standardise_rast()

 ),
 geotargets::tar_terra_rast(
   model_layers,
   c(covariate_rasters, bias)
 ),
 tar_target(
   bg_points,
   terra::spatSample(
     x = africa_mask,
     size = 10000,
     na.rm = TRUE,
     as.points = TRUE
   ) %>%
     crds()
 ),


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
   mpp_fit,
   multispeciesPP(
     sdm.formula = ~ tcw + tcb + built_volume,
     bias.formula = ~ bias,
     PA = mpp_data$pa,
     PO = mpp_data$po,
     BG = mpp_data$bg,
     region.size = sum(is.na(values(africa_mask))),
     inverse.hessian = FALSE,
     penalty.l2.sdm = 0.5,
     penalty.l2.bias = 0.5,
     penalty.l2.intercept = 1e-04
   )
   # Error in -2 * wt * y : non-numeric argument to binary operator
   # failure here is caused by this line
   # devold <- sum(dev.resids(y, mu, weights)[good.resp]) in
   # block.glm.fit call within multispeciesPP
 )

)


# tar_load_everything()
