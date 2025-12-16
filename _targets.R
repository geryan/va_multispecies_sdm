library(targets)
library(geotargets)
library(targets.utils) # remotes::install_github("geryan/targets.utils")

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
    #"multispeciesPP", # remotes::install_github("wfithian/multispeciesPP")
    "idpalette", # remotes::install_github("idem-lab/idpalette")
    #"rasterVis",
    "tidyterra",
    "geodata",
    "coda",
    "greta",
    "DHARMa",
    "lubridate",
    "magrittr",
    "stringr",
    "bayesplot",
    "patchwork",
    "see"
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


  # aggregated predictor variables, i.e., lower resolution images for quicker processing
  # but use the high res ones for final product
  # # mech layer is set to the minimum value above zero
  tar_target(
    covrastfile,
    "data/raster/static_vars_agg_mech_nonzero_dist_from_sea.tif",
    format = "file"
  ),

  tar_terra_rast(
    covariate_rast_all,
    rast(covrastfile)
  ),

  tar_target(
    target_covariate_names,
    c(
      #ag_microclim,
      #research_tt_by_country,
      "arid",
      "built_volume",
      # cropland,
      "elevation",
      "evi_mean", # correlates with pressure_mean rainfall_mean and solrad_mean
      "footprint", # correlates with built_volume and cropland
      #"lst_day_mean"#,
      "lst_night_mean",
      # # pressure_mean,
      # # rainfall_mean,
      "soil_clay",
      # # solrad_mean,
      # # surface_water, remove and replace with distance to surface water
      "tcb_mean", #, # strongly correlates with tcw
      # # tcw_mean,
      # windspeed_mean,
      #"easting",
      #"northing",
      "distance_from_sea"
    )
  ),

  tar_target(
    offset_names,
    c("ag_microclim")
  ),

  tar_target(
    bias_names,
    "research_tt_by_country"
  ),

  tar_terra_rast(
    covariate_rast,
    subset_covariate_rast(
      covariate_rast_all,
      target_covariate_names,
      offset_names,
      bias_names
    )
  ),


  tar_terra_rast(
    project_mask,
    make_project_mask(covariate_rast)
  ),

  ## specific regions/ countries of interest for close-up plots

  tar_target(
    west_africa_extent,
    ext(
      -17.9218578749435,
      14.1872874163643,
      4.00610661292353,
      25.6346066145936
    )
  ),

  tar_terra_vect(
    nga,
    gadm(
      country = "NGA",
      level = 0,
      path = "data/raster/geodata/"
    )
  ),

  tar_terra_vect(
    cod,
    gadm(
      country = "COD",
      level = 0,
      path = "data/raster/geodata/"
    )
  ),

  tar_terra_vect(
    kentzauga,
    gadm(
      country = c("KEN", "TZA", "UGA"),
      level = 0,
      path = "data/raster/geodata/"
    ) |>
      aggregate()
  ),

  # expert maps from
  # Sinka, M.E., Bangs, M.J., Manguin, S. et al.
  # The dominant Anopheles vectors of human malaria in Africa, Europe and
  # the Middle East: occurrence data, distribution maps and bionomic précis.
  # Parasites Vectors 3, 117 (2010). https://doi.org/10.1186/1756-3305-3-117
  # supp file: 13071_2010_245_MOESM1_ESM.ZIP

  tar_terra_vect(
    expert_maps,
    get_expert_maps(
      sp = c(
        "arabiensis",
        # "atroparvus",
        "funestus",
        "gambiae",
        # "labranchiae",
        "melas",
        "merus",
        # "messeae",
        "moucheti",
        "nili"#,
        # "sacharovi",
        # "sergentii",
        # "superpictus",
      )
    )
  ),

  # extent of this is too small?
  tar_terra_rast(
    expert_offset_maps,
    make_expert_offset_maps(
      expert_maps,
      project_mask,
      buffer_km = 1000
    )
  ),

  tar_terra_rast(
    expert_offset_maps_500,
    make_expert_offset_maps(
      expert_maps,
      project_mask,
      buffer_km = 500
    )
  ),

  tar_seed_set(
    tar_seed_create("bg_points")
  ),

  tar_target(
    n_bg,
    250
  ),

  tar_target(
    bg_points,
    terra::spatSample(
      x = covariate_rast[[1]],
      size = n_bg,
      na.rm = TRUE,
      as.points = TRUE
    ) %>%
      crds()
  ),

  # tar_target(
  #   bg_kmeans_list_env,
  #   bg_points_kmeans_env(
  #     n_bg,
  #     covariate_rast,
  #     n_samples_per_bg = 200
  #   )
  # ),

  tar_target(
    bg_kmeans_list_spatial,
    bg_points_kmeans_spatial(
      n_bg,
      covariate_rast,
      n_samples_per_bg = 200
    )
  ),

  tar_target(
    bg_kmeans_df,
    frame_bg_kmeans(bg_kmeans_list_spatial)
  ),

  ####################################
  # data wrangling and cleaning
  ###################################

 ## All VA data 20250716

 tar_target(
   raw_data_file,
   "data/tabular/VA_FULL_DATA_20250716.csv",
   format = "file"
 ),

 tar_target(
   raw_data,
   read_csv(
     file = raw_data_file,
     guess_max = 30000
   )
 ),

 #########################

 # process data

 # clean and make a set of records that are tidy and complete
 # but not excluding anything yet
 tar_target(
   full_data_records,
   clean_full_data_records(raw_data)
 ),


 # some exploration of the full data set
 # this does not return anything
 tar_target(
   exploration_full_data_records,
   explore_full_data_records(full_data_records)
 ),

 # need to refine this list
 tar_target(
   target_species,
   target_spp()
   #target_spp_test_only()
 ),

 tar_target(
   model_data_records,
   generate_model_data_records(
     full_data_records,
     target_species = target_species
   )
 ),

 # tar_target(
 #   model_data_all,
 #   bind_rows(
 #     model_data_records,
 #     bg_points |>
 #       as_tibble() |>
 #       rename(
 #         latitude = y,
 #         longitude = x
 #       ) |>
 #       mutate(
 #         data_type = "bg",
 #         n = 0
 #       )
 #   )
 # ),

 tar_target(
   record_data_spatial_all,
   get_spatial_values(
     lyrs = covariate_rast_all,
     dat = model_data_records,
     project_mask
   )
 ),

 tar_target(
   record_data_spatial,
   record_data_spatial_all |>
     select(
       - all_of(
         names(covariate_rast_all)[
           !names(covariate_rast_all) %in%
             c(
               target_covariate_names,
               offset_names,
               bias_names
             )
         ]
       ),
     )
 ),

 tar_target(
   model_data_spatial,
   bind_rows(
     record_data_spatial |>
       mutate(weight = 1),
     bg_kmeans_df |>
       mutate(
         data_type = "bg",
         presence = 0,
         n = 0
       )
   )
 ),


 tar_target(
   pa_data_table,
   model_data_spatial |>
     group_by(
       species,
       presence
     ) |>
     summarise(
       n = n(),
       .groups = "drop"
     ) |>
     pivot_wider(
       names_from = presence,
       names_prefix = "p",
       values_from = n
     ) |>
     rename(
       detected = p1,
       undetected = p0
     )
 ),

 ## plots before modelling

 tar_target(
   covs_plots,
   make_covariate_plots(
     model_data_spatial,
     cvnames = target_covariate_names
   )
 ),

 tar_target(
   bias_offset_plots,
   make_covariate_plots(
     model_data_spatial,
     cvnames = c(
       offset_names,
       bias_names
     )
   )
 ),

 # tar_target(
 #   covs_plots_all,
 #   make_covariate_plots(
 #     record_data_spatial_all,
 #     target_species,
 #     target_covariate_names, # this needs tweaking as it
 #     # won't currently get the non-target covs which is the whole point - need to extract from covariate rasters
 #     offset_names,
 #     bias_names
 #   )
 # ),


 tar_target(
   simple_point_plots,
   make_point_plots(
     model_data_spatial,
     expert_maps,
     project_mask
   )
 ),

 # tar_target(
 #   expert_map_plots,
 #   make_expert_map_plots(
 #     expert_maps,
 #     new_mask
 #   )
 # ),

 ######
 # PCA Covariate layers
 ######
 tar_terra_rast(
   pca_covariate_layers,
   make_pca_covariate_layers(
     covariate_rast,
     target_covariate_names,
     model_data_spatial
   )
 ),

 tar_target(
   model_data_spatial_pca,
   get_spatial_values(
     pca_covariate_layers,
     model_data_spatial,
     project_mask
   ) |>
     select(
       - all_of(target_covariate_names),
     )
 ),

 tar_target(
   covs_plots_pca,
   make_covariate_plots(
     model_data_spatial_pca,
     cvnames = names(pca_covariate_layers),
     fname = "outputs/figures/cov_violins_pca.png"
   )
 ),


 # one sp add at a time
 # add absence vs presence at a time
 # consider various weighting of the expert layers and
 #  distance of buffer from edge (linear weighting drop off)

 ################
 ## models
 ################

 ## multispecies pp count
 ##

 # fit the model in greta
 # save an image of the environment within function
 # otherwise the greta model nodes become disconnected and buggered up
 # because of some R6 nonsense with greta or whatever and the usual targets
 # shenanigans
 tar_target(
   model_fit_image_multisp_pp_count,
   fit_model_multisp_pp_count(
     model_data_spatial,
     target_covariate_names,
     target_species,
     project_mask,
     image_name = "outputs/images/multisp_pp_count.RData",
     n_burnin = 3000,
     n_samples = 1000,
     n_chains = 50
   )
 ),

 # # read in image and predict out raster as a tif
 tar_target(
   pred_file_multisp_pp_count,
   predict_greta_mspp_count(
     image_filename = model_fit_image_multisp_pp_count,
     prediction_layer = covariate_rast,
     target_species,
     output_file_prefix = "outputs/rasters/multisp_pp_count"
   )
 ),

 tar_target(
   pred_file_multisp_pp_count_pa_expoff,
   add_expert_offset(
     predfilelist = pred_file_multisp_pp_count,
     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
     expert_offset_maps = expert_offset_maps_500
   )
 ),

 tar_target(
   pred_file_multisp_pp_count_count_expoff,
   add_expert_offset_count(
     predfilelist = pred_file_multisp_pp_count,
     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
     expert_offset_maps = expert_offset_maps_500
   )
 ),


 #
 # #####
 #
 # distribution plots

 # this is the temporary thang until the above are tidied
 tar_terra_rast(
   pred_dist,
   rast(x = pred_file_multisp_pp_count_pa_expoff)
 ),

 tar_target(
   distribution_plots,
   make_distribution_plots(
     pred_dist,
     model_data_spatial,
     plot_dir = "outputs/figures/distribution_plots/distn_20251030"
   )
 ),

 ## relative abundance


 # this is making the gambiae-coluzzii layer inside this function
 # if we get the hierarchical species complex model in, be sure to still do this
 # as the gam-col layer would (should :grimace:) only be the shared complex
 # thingies, not the joint species level ones
 tar_terra_rast(
   rel_abund_rgb,
   make_rel_abund_rgb(
     #x = pred_dist_rgb,
     x = pred_dist,
     threshold = 0.05
   ),
   datatype = "INT1U"
 ),

 tar_target(
   rel_abund_plots,
   make_rel_abund_rgb_plot(
     rel_abund_rgb,
     project_mask,
     filename = "outputs/figures/rgb_relative_abundance_20251030.png"
   )
 ),

 ## variance scaling

 tar_terra_rast(
   pred_dist_scale,
   scale_predictions(
     lambda_file = pred_file_multisp_pp_count_count_expoff
   )
 ),

 tar_target(
   distribution_plots_scale,
   make_distribution_plots(
     pred_dist_scale,
     model_data_spatial,
     plot_dir = "outputs/figures/distribution_plots/distn_20251030_scale"
   )
 ),


 ###########
 ## multispecies pp count with sampling method
 ##

  tar_target(
   model_fit_image_multisp_pp_count_sm,
   fit_model_multisp_pp_count_sm(
     model_data_spatial,
     target_covariate_names,
     target_species,
     project_mask,
     image_name = "outputs/images/multisp_pp_count_sm.RData",
     n_burnin = 3000,
     n_samples = 1000,
     n_chains = 50
   )
 ),

 # # # read in image and predict out raster as a tif
 tar_target(
   pred_file_multisp_pp_count_sm,
   predict_greta_mspp_count(
     image_filename = model_fit_image_multisp_pp_count_sm,
     prediction_layer = covariate_rast,
     target_species,
     output_file_prefix = "outputs/rasters/multisp_pp_count_sm"
   )
 ),

 tar_target(
   pred_file_multisp_pp_count_expoff_sm,
   add_expert_offset(
     predfilelist = pred_file_multisp_pp_count_sm,
     expert_offset_maps = expert_offset_maps_500
     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
   )
 ),

 tar_target(
   pred_file_multisp_pp_count_expoff_sm_count,
   add_expert_offset_count(
     predfilelist = pred_file_multisp_pp_count_sm,
     expert_offset_maps = expert_offset_maps_500
     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
   )
 ),
#
#  tar_target(
#    pred_file_multisp_pp_count_expoff_sm_500,
#    add_expert_offset(
#      predfilelist = pred_file_multisp_pp_count_sm,
#    )
#  ),

 #
 # #####
 #
 # distribution plots

 # this is the temporary thang until the above are tidied
 tar_terra_rast(
   pred_dist_sm,
   rast(x = pred_file_multisp_pp_count_expoff_sm)
 ),

 tar_target(
   distribution_plots_sm,
   make_distribution_plots(
     pred_dist_sm,
     model_data_spatial,
     plot_dir = "outputs/figures/distribution_plots/distn_20251030_sm"
   )
 ),

 ## relative abundance

 tar_terra_rast(
   rel_abund_rgb_sm,
   make_rel_abund_rgb(
     #x = pred_dist_rgb,
     x = pred_dist_sm,
     threshold = 0.05
   ),
   datatype = "INT1U"
 ),

 tar_target(
   rel_abund_plots_sm,
   make_rel_abund_rgb_plot(
     rel_abund_rgb_sm,
     project_mask,
     filename = "outputs/figures/rgb_relative_abundance_20251030_sm.png"
   )
 ),

 ## variance scaling

 tar_terra_rast(
   pred_dist_scale_sm,
   scale_predictions(
     pred_file_multisp_pp_count_expoff_sm_count
   )
 ),

 tar_target(
   distribution_plots_scale_sm,
   make_distribution_plots(
     pred_dist_scale_sm,
     model_data_spatial,
     plot_dir = "outputs/figures/distribution_plots/distn_20251030_sm_scale"
   )
 ),

 #############################################################################
 #############################################################################
 # 4 species only analysis
# need to refine this list
tar_target(
  target_species_4,
  target_spp_test_only()
),

tar_target(
  model_data_records_4,
  generate_model_data_records(
    full_data_records,
    target_species = target_species_4
  )
),


tar_target(
  record_data_spatial_all_4,
  get_spatial_values(
    lyrs = covariate_rast_all,
    dat = model_data_records_4,
    project_mask
  )
),

tar_target(
  record_data_spatial_4,
  record_data_spatial_all_4 |>
    select(
      - all_of(
        names(covariate_rast_all)[
          !names(covariate_rast_all) %in%
            c(
              target_covariate_names,
              offset_names,
              bias_names
            )
        ]
      ),
    )
),

tar_target(
  model_data_spatial_4,
  bind_rows(
    record_data_spatial_4 |>
      mutate(weight = 1),
    bg_kmeans_df |>
      mutate(
        data_type = "bg",
        presence = 0,
        n = 0
      )
  )
),

## multispecies pp count
##

# fit the model in greta
# save an image of the environment within function
# otherwise the greta model nodes become disconnected and buggered up
# because of some R6 nonsense with greta or whatever and the usual targets
# shenanigans
tar_target(
  model_fit_image_multisp_pp_count_4,
  fit_model_multisp_pp_count(
    model_data_spatial_4,
    target_covariate_names,
    target_species_4,
    project_mask,
    image_name = "outputs/images/4_multisp_pp_count.RData",
    n_burnin = 2000,
    n_samples = 1000,
    n_chains = 50
  )
),

# # read in image and predict out raster as a tif
tar_target(
  pred_file_multisp_pp_count_4,
  predict_greta_mspp_count(
    image_filename = model_fit_image_multisp_pp_count_4,
    prediction_layer = covariate_rast,
    target_species,
    output_file_prefix = "outputs/rasters/4_multisp_pp_count"
  )
),

tar_target(
  pred_file_multisp_pp_count_pa_expoff_4,
  add_expert_offset(
    predfilelist = pred_file_multisp_pp_count_4,
    #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
    expert_offset_maps = expert_offset_maps_500
  )
),

tar_target(
  pred_file_multisp_pp_count_count_expoff_4,
  add_expert_offset_count(
    predfilelist = pred_file_multisp_pp_count_4,
    #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
    expert_offset_maps = expert_offset_maps_500
  )
),




 #####################

 tar_target(
   so_i_dont_have_to_go_backward_and_add_commas,
   print("Targets great in theory but kinda annoying to work with")
 )

)


# tar_load_everything()
# tar_load_globals()
