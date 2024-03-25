library(targets)

tar_option_set(
  packages = c(
    "tibble",
    "dplyr",
    "sdmtools",
    "readr",
    "tidyr",
    "terra",
    "ggplot2"
  ),
  workspace_on_error = TRUE
)

tar_source(files = "R")


list(
  # data wrangling and cleaning
 tar_target(
   raw_data_file,
   "data/final_species_20240314.csv",
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
   pa_data_all,
   pa_from_va(va_data)
 ),
 tar_target(
   pa_data_records,
   filter_few(pa_data_all) |>
     filter(species != "GAMBIAE COMPLEX")
   # potentially remove later or put elsewhere
 ),
 # summary stats and figures
 tar_target(
   pa_records_table,
   make_records_table(pa_data_records)
 ),
 tar_target(
   pa_plot_data,
   make_pa_plot_data(pa_data_records)
 ),
 tar_target(
   pa_plot,
   make_pa_plot(pa_plot_data)
 ),
 tar_target(
   pa_plot_file,
   ggsave(
     filename = "figures/pa_po_count_plot.png",
     plot = pa_plot,
     width = 2400,
     height = 1500,
     units = "px"
   )
 )
)


# tar_load_everything()
