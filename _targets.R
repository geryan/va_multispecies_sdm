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
    "geotargets"
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

 # geotargets::tar_terra_rast(
 #   africa_mask,
 #   sdmtools::make_africa_mask(
 #     file_name = "data/raster/africa_mask.tif",
 #   )
 # ),

 geotargets::tar_terra_rast(
   covariate_rasters,
   prepare_covariates()
 ),
 geotargets::tar_terra_rast(
   bias,
   prepare_bias(covariate_rasters[[1]])
 ),
 geotargets::tar_terra_rast(
   model_layers,
   c(covariate_rasters, bias)
 )

)


# tar_load_everything()
