library(targets)

tar_option_set(
  packages = c(
    "tibble",
    "dplyr",
    "sdmtools",
    "readr",
    "tidyr",
    "terra"
  ),
  workspace_on_error = TRUE
)

tar_source(files = "R")


list(
 tar_target(
   raw_data_file,
   "data/final_species_20240314.csv",
   format = "file"
 ),
 tar_target(
   raw_data,
   read_csv(raw_data_file)
 ),
 tar_target(
   va_data,
   tidy_va_data(raw_data)
 )
)
