
# Load packages required to define the pipeline:
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
  worspace_on_error = TRUE
)

# tar_load_globals()
# tar_load_everything()

# Run the R scripts in the R/ folder
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
 )
)

