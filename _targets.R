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
    "tidyterra"
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
 tar_terra_rast(
   new_mask,
   make_new_mask(covariate_rasters)
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
     new_mask,
     bias_name
   ) |>
     standardise_rast()
 ),
 tar_terra_vect(
   new_mask_v,
   as.polygons(new_mask)
 ),

 geotargets::tar_terra_rast(
   model_layers,
   c(covariate_rasters, bias)
 ),
 tar_seed_set(
   tar_seed_create("bg_points")
 ),
 tar_target(
   bg_points,
   terra::spatSample(
     x = new_mask,
     size = 10000,
     na.rm = TRUE,
     as.points = TRUE
   ) %>%
     crds()
 ), #mfing slow but setting of seed stops it rerunning


 # model data collation and fitting
 tar_target(
   mpp_data,
   format_mpp_data(
     records = data_records,
     background = bg_points,
     modlyr = model_layers
   )
 ),

#
#
#  ### Model 1
#  tar_target(
#    mpp_fit_1,
#    multispeciesPP(
#      sdm.formula = ~ tcw + tcb + built_volume,
#      bias.formula = ~ bias,
#      PA = mpp_data$pa,
#      PO = mpp_data$po,
#      BG = mpp_data$bg
#    )
#  ),
#  tar_terra_rast(
#    preds_1,
#    predict_mpp_rast_all(
#      model = mpp_fit_1,
#      data = model_layers,
#      filename = "outputs/preds_1.tif",
#      overwrite = TRUE
#    )
#  ),
#  tar_target(
#    pred_plot_1,
#    levelplot(
#      preds_1,
#      col.regions = idpalette("idem", 20),
#      layout = c(2,2)
#    )
#  ),
#  tar_target(
#    pred_plot_file_1,
#    sdmtools::save_plot(
#      p = pred_plot_1,
#      filename = "outputs/figures/pred_plot_1.png",
#      width = 2400,
#      #height = 1500,
#      units = "px",
#      res = 300
#    )
#  ),
#
#
#  ### Model 2
#  tar_target(
#    mpp_fit_2,
#    multispeciesPP(
#      sdm.formula =  ~ tcw + tcb + built_volume + landcover + lst_day + lst_night + evi + rainfall,
#      bias.formula = ~ bias,
#      PA = mpp_data$pa,
#      PO = mpp_data$po,
#      BG = mpp_data$bg
#    )
#  ),
#  tar_terra_rast(
#    preds_2,
#    predict_mpp_rast_all(
#      model = mpp_fit_2,
#      data = model_layers,
#      filename = "outputs/preds_2.tif",
#      overwrite = TRUE
#    )
#  ),
#  tar_target(
#    pred_plot_2,
#    levelplot(
#      preds_2,
#      col.regions = idpalette("idem", 20),
#      layout = c(2,2)
#    )
#  ),
#  tar_target(
#    pred_plot_file_2,
#    sdmtools::save_plot(
#      p = pred_plot_2,
#      filename = "outputs/figures/pred_plot_2.png",
#      width = 2400,
#      #height = 1500,
#      units = "px",
#      res = 300
#    )
#  ),
#
#  ### Model 3
#  tar_target(
#    mpp_fit_3,
#    multispeciesPP(
#      sdm.formula =  ~ tcw + tcb + built_volume + landcover + lst_day + lst_night + evi + rainfall + mech,
#      bias.formula = ~ bias,
#      PA = mpp_data$pa,
#      PO = mpp_data$po,
#      BG = mpp_data$bg
#    )
#  ),
#  tar_terra_rast(
#    preds_3,
#    predict_mpp_rast_all(
#      model = mpp_fit_3,
#      data = model_layers,
#      filename = "outputs/preds_3.tif",
#      overwrite = TRUE
#    )
#  ),
#  tar_target(
#    pred_plot_3,
#    levelplot(
#      preds_3,
#      col.regions = idpalette("idem", 20),
#      layout = c(2,2)
#    )
#  ),
#  tar_target(
#    pred_plot_file_3,
#    sdmtools::save_plot(
#      p = pred_plot_3,
#      filename = "outputs/figures/pred_plot_3.png",
#      width = 2400,
#      #height = 1500,
#      units = "px",
#      res = 300
#    )
#  ),
#
#  ### Model 4
#  tar_target(
#    mpp_fit_4,
#    multispeciesPP(
#      sdm.formula =  ~ tcw + built_volume + landcover + evi + lst_night + rainfall + mech,
#      bias.formula = ~ bias,
#      PA = mpp_data$pa,
#      PO = mpp_data$po,
#      BG = mpp_data$bg
#    )
#  ),
#  tar_terra_rast(
#    preds_4,
#    predict_mpp_rast_all(
#      model = mpp_fit_4,
#      data = model_layers,
#      filename = "outputs/preds_4.tif",
#      overwrite = TRUE
#    )
#  ),
#  tar_target(
#    pred_plot_4,
#    levelplot(
#      preds_4,
#      col.regions = idpalette("idem", 20),
#      layout = c(2,2)
#    )
#  ),
#  tar_target(
#    pred_plot_file_4,
#    sdmtools::save_plot(
#      p = pred_plot_4,
#      filename = "outputs/figures/pred_plot_4.png",
#      width = 2400,
#      #height = 1500,
#      units = "px",
#      res = 300
#    )
#  ),
#
#  ### Model 5
#
#  tar_target(
#    mpp_data_f,
#    format_mpp_data_fac(
#      records = data_records,
#      background = bg_points,
#      modlyr = model_layers
#    )
#  ),
#  tar_target(
#    mpp_fit_5,
#    multispeciesPP(
#      sdm.formula =  ~ tcw + built_volume + landcover + evi + lst_night + rainfall + mech,
#      bias.formula = ~ bias,
#      PA = mpp_data$pa,
#      PO = mpp_data$po,
#      BG = mpp_data$bg
#    )
#  ),
#  tar_terra_rast(
#    preds_5,
#    predict_mpp_rast_all(
#      model = mpp_fit_5,
#      data = model_layers,
#      filename = "outputs/preds_5.tif",
#      overwrite = TRUE
#    )
#  ),
#  tar_target(
#    pred_plot_5,
#    levelplot(
#      preds_5,
#      col.regions = idpalette("idem", 20),
#      layout = c(2,2)
#    )
#  ),
#  tar_target(
#    pred_plot_file_5,
#    sdmtools::save_plot(
#      p = pred_plot_5,
#      filename = "outputs/figures/pred_plot_5.png",
#      width = 2400,
#      #height = 1500,
#      units = "px",
#      res = 300
#    )
#  ),


# alt data prep

#  tar_target(
#    pa_dat_plot,
#    ggplot() +
#      geom_spatraster(
#        data = africa_mask
#      ) +
#      geom_point(
#        data = data_records |> mutate(presence = as.character(presence)),
#        aes(
#          x = lon,
#          y = lat,
#          colour = presence,
#          shape = presence,
#
#        )
#      ) +
#      facet_wrap(~species) +
#      theme_minimal() +
#      scale_fill_viridis_c(
#        option = "G",
#        begin = 1,
#       end = 0.5,
#        na.value = "white"
#      ) +
#      scale_colour_viridis_d(
#        option = "A"
#      )
#  ),
#  tar_target(
#    rcrds,
#    prep_rcrds(va_data)
#  ),
#
#
# tar_target(
#   mpp_rcrds,
#   format_mpp_data(
#     records = rcrds,
#     background = bg_points,
#     modlyr = model_layers
#   )
# ),
#
#  tar_target(
#    mpp_fit_6,
#    multispeciesPP(
#      sdm.formula =  ~ tcw + built_volume + landcover + evi + lst_night + rainfall,
#      bias.formula = ~ bias,
#      PA = mpp_rcrds$pa,
#      PO = mpp_rcrds$po,
#      BG = mpp_rcrds$bg
#    )
#  ),
#  tar_terra_rast(
#    preds_6,
#    predict_mpp_rast_all(
#      model = mpp_fit_6,
#      data = model_layers,
#      filename = "outputs/preds_6.tif",
#      overwrite = TRUE
#    )
#  ),
#  tar_target(
#    pred_plot_6,
#    levelplot(
#      preds_6,
#      col.regions = idpalette("idem", 20),
#      layout = c(2,2)
#    )
#  ),
#  tar_target(
#    pred_plot_file_6,
#    sdmtools::save_plot(
#      p = pred_plot_6,
#      filename = "outputs/figures/pred_plot_6.png",
#      width = 2400,
#      #height = 1500,
#      units = "px",
#      res = 300
#    )
#  ),
#
#  # model 7
# tar_target(
#   mpp_fit_7,
#   multispeciesPP(
#     sdm.formula =  ~ tcw + built_volume + landcover + evi + lst_night + rainfall + offset(mech),
#     bias.formula = ~ bias,
#     PA = mpp_rcrds$pa,
#     PO = mpp_rcrds$po,
#     BG = mpp_rcrds$bg
#   )
# ),
# tar_terra_rast(
#   preds_7,
#   predict_mpp_rast_all(
#     model = mpp_fit_7,
#     data = model_layers,
#     filename = "outputs/preds_7.tif",
#     overwrite = TRUE
#   )
# ),
# tar_target(
#   pred_plot_7,
#   levelplot(
#     preds_7,
#     col.regions = idpalette("idem", 20),
#     layout = c(2,2)
#   )
# ),
#
# tar_target(
#   pred_plot_file_7,
#   sdmtools::save_plot(
#     p = pred_plot_7,
#     filename = "outputs/figures/pred_plot_7.png",
#     width = 2400,
#     #height = 1500,
#     units = "px",
#     res = 300
#   )
# ),


 ## new approach to buffering absences

 tar_target(
   rcrds_2,
   prep_rcrds_2(va_data, new_mask_v)
 ),


 tar_target(
   mpp_rcrds_2,
   format_mpp_data(
     records = rcrds_2,
     background = bg_points,
     modlyr = model_layers
   )
 ),

  # model 8
  tar_target(
    mpp_fit_8,
    multispeciesPP(
      sdm.formula =  ~ tcw + built_volume + landcover + evi + lst_night + rainfall,
      bias.formula = ~ bias,
      PA = mpp_rcrds_2$pa,
      PO = mpp_rcrds_2$po,
      BG = mpp_rcrds_2$bg
    )
  ),
  tar_terra_rast(
    preds_8,
    predict_mpp_rast_all(
      model = mpp_fit_8,
      data = model_layers,
      filename = "outputs/preds_8.tif",
      overwrite = TRUE
    )
  ),
  tar_target(
    pred_plot_8,
    levelplot(
      preds_8,
      col.regions = idpalette("idem", 20),
      layout = c(2,2)
    )
  ),

  tar_target(
    pred_plot_file_8,
    sdmtools::save_plot(
      p = pred_plot_8,
      filename = "outputs/figures/pred_plot_8.png",
      width = 2400,
      #height = 1500,
      units = "px",
      res = 300
    )
    ),


 # model _9
 tar_target(
   mpp_fit_9,
   multispeciesPP(
     sdm.formula =  ~ tcw + built_volume + landcover + evi + lst_night + rainfall + offset(mech),
     bias.formula = ~ bias,
     PA = mpp_rcrds_2$pa,
     PO = mpp_rcrds_2$po,
     BG = mpp_rcrds_2$bg
   )
 ),
 tar_terra_rast(
   preds_9,
   predict_mpp_rast_all(
     model = mpp_fit_9,
     data = model_layers,
     filename = "outputs/preds_9.tif",
     overwrite = TRUE
   )
 ),
 tar_target(
   pred_plot_9,
   levelplot(
     preds_9,
     col.regions = idpalette("idem", 20),
     layout = c(2,2)
   )
 ),

 tar_target(
   pred_plot_file_9,
   sdmtools::save_plot(
     p = pred_plot_9,
     filename = "outputs/figures/pred_plot_9.png",
     width = 2400,
     #height = 1500,
     units = "px",
     res = 300
   )
 )

)





# tar_load_everything()
# tar_load_globals()
