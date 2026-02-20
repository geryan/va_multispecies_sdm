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
    "patchwork"
    # "see"
  ),
  workspace_on_error = TRUE
)

tar_source(files = "R")


list(
  #########################
  # spatial data
  ########################

  # get the user, for switching paths
  tar_target(
    user_is_nick,
    Sys.info()[["user"]] == "nick"
  ),

  # read in offset layers

  # all raw layers
  tar_terra_rast(
    offsets_raw,
    read_offset_data(
      odir = ifelse(
        user_is_nick,
        "../mosmicrosim/processing/vector_rasters",
        "/Users/gryan/Dropbox/vector_rasters/"
      )
    )
  ),

  # generate mask from offset layers
  # some of these contain NAs within continent
  # these are filled in the mask
  tar_terra_rast(
    project_mask_5_outline,
    make_mask_from_offsets(offsets_raw) |>
      set_layer_names("project_mask")
  ),

  # read in other layers and match to offset size shape and extent
  #
  tar_terra_rast(
    landcover_raw,
    get_landcovers(
      vars = c(
        "trees",
        "grassland",
        "shrubs",
        "cropland",
        "built",
        #"bare",
        "water",
        "wetland",
        "mangroves"
      ),
      path = "data/raster/geodata/"
    )
  ),

  tar_terra_rast(
    water_mask_5,
    make_water_mask(
      water = landcover_raw[["water"]],
      proj_mask = project_mask_5_outline
    )
  ),

  tar_terra_rast(
    project_mask_5,
    project_mask_5_outline |>
      mask(mask = water_mask_5)
  ),

  # distance from sea
  tar_terra_rast(
    dist_from_sea,
    make_distance_from_sea(
      project_mask_5,
      project_mask_5_outline
    )
  ),

  # proximity to sea (1 is close, 0 is far)
  tar_terra_rast(
    prox_to_sea,
    make_proximity_to_sea(
      dist_from_sea
    )
  ),

  # cleaning to fills NAs within continent with very small number
  tar_terra_rast(
    offsets_5,
    clean_offsets(
      offsets_raw,
      project_mask_5,
      replacement = .Machine$double.eps
    )
  ),

  ## landcover vars from worldcover
  tar_terra_rast(
    landcover_covs,
    landcover_raw |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      mask(mask = project_mask_5) #|> scale()
    # don't scale landcover types, so they remain 0-1, and a
    # positive-constrained coefficient enforces that more habitat corresponds
    # with more mosquitoes

  ),

  # this reads in the bioregions as a single file
  # but it's useless for passing on the categories
  # only good if want to make a pretty picture of it
  tar_terra_rast(
    bioregions_all,
    get_bioregions(project_mask = project_mask_5)
  ),

  # this returns a stack of layers 1 bioregion each
  tar_terra_rast(
    bioregion_stack,
    split_bioregions(project_mask = project_mask_5)
  ),


  # layers from Malaria Atlas Project
  #

  # built volume

  tar_terra_rast(
    built_volume_raw,
    rast("/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/GHSL_2023/GHS_BUILT_V_R23A.2020.Annual.Data.1km.Data.tif") |>
    set_layer_names("built_volume")
  ),

  tar_terra_rast(
    built_volume_5,
    built_volume_raw |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      mask(mask = project_mask_5) |>
      scale()
  ),

  # EVI
  # monthly EVI available from MAP, need to work out some code to process this
  # stuff on MAP workbench so that I don't need to download all the enormous
  # layers
  tar_terra_rast(
    evi_raw,
    prepare_multi_layer(
      data_dir ="/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/EVI/"#,
      # layer_prefix = "evi",
      # file_id_prefix = ".*v6\\.",
      # file_id_suffix = "\\.Annual.*"
    )
  ),

  tar_terra_rast(
    evi_5,
    evi_raw |>
      mean() |>
      set_layer_names("evi") |>
      crop(y = project_mask_5) |>
      aggregate(fact = 5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      mask(mask = project_mask_5) |>
      scale()
  ),


  # Tasseled Cap Brightness
  # same for
  # TCB re monthly data
  tar_terra_rast(
    tcb_raw,
    prepare_multi_layer(
      data_dir ="/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/TCB/"
    )
  ),

  tar_terra_rast(
    tcb_5,
    tcb_raw |>
      mean() |>
      set_layer_names("tcb") |>
      crop(y = project_mask_5) |>
      aggregate(fact = 5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      mask(mask = project_mask_5) |>
      scale()
  ),


  # land surface temperature at night annual mean
  # also same for lst re monthly data
  tar_terra_rast(
    lst_night_raw,
    prepare_multi_layer(
      data_dir ="/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/LST_Night/"
    )
  ),

  tar_terra_rast(
    lst_night_5,
    lst_night_raw |>
      mean() |>
      set_layer_names("lst_night") |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      mask(mask = project_mask_5) |>
      scale()
  ),

  #
  # via `geodata`
  #

  # elevation

  tar_terra_rast(
    elevation_raw,
    global_regions |>
      filter(continent == "Africa") |>
      pull(iso3) |>
      lapply(
        FUN = function(x){
          elevation_30s(
            country = x,
            path = "data/raster/geodata"
          )
        }
      ) |>
      sprc() |>
      merge() |>
      set_layer_names("elevation")
  ),

  tar_terra_rast(
    elevation_5,
    elevation_raw |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      mask(mask = project_mask_5) |>
      scale()
  ),

  # soil clay

  tar_terra_rast(
    soil_clay_filled,
    get_soil_af() |>
      set_layer_names("soil_clay")
  ),

  tar_terra_rast(
    soil_clay_5,
    soil_clay_filled |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      mask(mask = project_mask_5) |>
      scale()
  ),

  # footprint

  tar_terra_rast(
    footprint_raw,
    footprint(
      year = 2009,
      path = "data/raster/geodata"
    ) |>
      set_layer_names("footprint")
  ),

  tar_terra_rast(
    footprint_5,
    footprint_raw |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      mask(mask = project_mask_5) |>
      scale()
  ),

  # tar_terra_rast(
  #   built_volume_5,
  #   built_volume_raw |>
  #     aggregate(fact = 5) |>
  #     crop(y = project_mask_5) |>
  #     resample(y = project_mask_5) |>
  #     fill_na_with_nearest_mean(maxRadiusCell = 50) |>
  #     mask(mask = project_mask_5) |>
  #     scale()
  # ),

  #
  # bias
  # travel time from research facilities
  # layer created in
  # https://github.com/geryan/africa_anopheles_sampling_bias

  tar_terra_rast(
    bias_tt_raw,
    if (user_is_nick) {
      rast("data/raster/tt_by_country.tif")
    } else{
      rast("/Users/gryan/Documents/tki_work/vector_atlas/africa_anopheles_sampling_bias/outputs/tt_by_country.tif")
    }
    # rast("/Users/gryan/Documents/tki_work/vector_atlas/africa_anopheles_sampling_bias/outputs/tt_by_country.tif")
  ),

  # that this doesn't quite match the mask layer suggests I need to redo
  # the bias analysis with a different layer
  # grouch
  tar_terra_rast(
    bias_tt_5,
    bias_tt_raw |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      scale_rast_to_1(reverse = TRUE) |>
      mask(mask = project_mask_5) |>
      `names<-`("travel_time")
  ),


  #
  # Tidy spatial data
  #

  # check that there are not NAs hanginig around in layers that don't
  # match the mask
  tar_target(
    mismatched_nas, # should be zero length if all NAs aligned
    check_no_mismatched_nas(
      proj_mask = project_mask_5,
      offsets_5[[1]],
      # built_volume_5,
      # evi_5,
      # tcb_5,
      # lst_night_5,
      # elevation_5,
      # soil_clay_5,
      # footprint_5,
      landcover_covs,
      prox_to_sea,
      bias_tt_5
    )
  ),

  tar_target(
    target_covariate_names,
    c(
      #"built_volume",
      #"evi", # correlates with pressure_mean rainfall_mean and solrad_mean
      #"tcb",
      #"lst_night",
      # "elevation",
      # "footprint", # correlates with built_volume and cropland
      #"soil_clay",

      # only use the worldcover landcover classes, in fractional cover (0-1)
      # form, excluding the obviously unsuitable habitat (bare, snow, etc)
      "trees",
      "grassland",
      "shrubs",
      "cropland",
      "built",
      "water",
      "wetland",
      "mangroves",

      # and proximity to sea, for merus and melas
      "prox_to_sea",

      # bioregions - hashed out ones unlikely to use
      "AT02",
      "AT05",
      "AT06",
      "AT07",
      "AT08",
      "AT09",
      "AT10",
      "AT11",
      "AT12",
      "AT13",
      "AT14",
      "AT15",
      "AT16",
      "AT17",
      #"AT18",
      "AT19",
      "AT20",
      "AT21",
      "AT22",
      "AT23"#,
      #"PA23",
      #"PA24",
      #"PA25",
      #"PA26"
    )
  ),

  tar_target(
    bias_names,
    "travel_time"
  ),

  tar_terra_rast(
    covariate_rast_5_all,
    c(
      # built_volume_5,
      # evi_5,
      # tcb_5,
      # lst_night_5,
      # elevation_5,
      # soil_clay_5,
      # footprint_5,
      landcover_covs,
      prox_to_sea,
      bioregion_stack,
      bias_tt_5
    )
  ),

  tar_terra_rast(
    covariate_rast_5,
    subset_covariate_rast(
      covariate_rast_5_all,
      target_covariate_names,
      bias_names
    )
  ),

  tar_terra_rast(
    covariate_rast_10,
    aggregate(
      covariate_rast_5,
      fact = 2,
      fun = "mean",
      cores = 4,
      na.rm = TRUE
    )
  ),

  tar_terra_rast(
    offsets_avg_5,
    offsets_5[[289:300]] |>
      mean() |>
      set_layer_names(layernames = "offset")
  ),

  tar_terra_rast(
    offsets_avg_10,
    aggregate(
      offsets_avg_5,
      fact = 2,
      fun = "mean",
      cores = 4,
      na.rm = TRUE
    )
  ),

  tar_terra_rast(
    offsets_avg_p_5,
    1 - exp(-offsets_avg_5)
  ),

  tar_terra_rast(
    offsets_avg_p_10,
    1 - exp(-offsets_avg_10)
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
      project_mask_5,
      buffer_km = 1000
    )
  ),

  tar_terra_rast(
    expert_offset_maps_500,
    make_expert_offset_maps(
      expert_maps,
      project_mask_5,
      buffer_km = 500
    )
  ),

  tar_seed_set(
    tar_seed_create("bg_points")
  ),

  tar_target(
    n_bg,
    2000
  ),

  tar_target(
    bg_points,
    terra::spatSample(
      x = covariate_rast_5[[1]],
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
      covariate_rast_5,
      n_samples_per_bg = 200
    )
  ),

  tar_target(
    bg_kmeans_df,
    frame_bg_kmeans(
      bg_kmeans_list_spatial,
      project_mask = project_mask_5
    )
  ),

  ####################################
  # data wrangling and cleaning
  ###################################

 ## All VA data 20250716

 tar_target(
   raw_data_file,
   #"data/tabular/VA_FULL_DATA_20250716.csv",
   "data/tabular/VA_DATA_20260202.csv",
   format = "file"
 ),

 tar_target(
   raw_data,
   read_csv(
     file = raw_data_file,
     guess_max = 100000 # some cols are largely empty until end, so needs this
     # or will assume they are logical and break when they are not
     # NB this is longer than the entire data set (for now...)
   )
 ),

 #########################

 # process data

 # clean and make a set of records that are tidy and complete
 # removes data where insecticides were used
 # and larval bioassay data
 #
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
   # c(
   #   "arabiensis", # in sinka 2010
   #   "coluzzii",
   #   "funestus", # in sinka 2010
   #   "gambiae",  # in sinka 2010
   #   "moucheti", # in sinka 2010
   #   "nili"#,  # in sinka 2010
   # )
 ),

 tar_target(
   model_data_records,
   generate_model_data_records(
     full_data_records,
     target_species = target_species
   )
 ),

 tar_target(
   model_data_records_ni,
   generate_model_data_records_no_impute(
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

 # This target extracts the spatial data from the covariate rasts
 # but also removes points that are outside of the project mask
 # this gets rid of ~10k points (some of which will be imputed)
 #
 tar_target(
   record_data_spatial_all,
   get_spatial_values(
     lyrs = covariate_rast_5_all,
     dat = model_data_records,
     #dat = model_data_records_ni,
     project_mask = project_mask_5
   )
 ),

 tar_target(
   record_data_spatial,
   record_data_spatial_all |>
     select(
       - all_of(
         names(covariate_rast_5_all)[
           !names(covariate_rast_5_all) %in%
             c(
               target_covariate_names,
               #offset_names,
               bias_names
             )
         ]
       ),
     )
 ),

 # subsample the data to speed up model fitting, while iterating model
 # development
 tar_target(
   record_data_spatial_subsample,
   record_data_spatial |>
     group_by(
       species,
       data_type,
       sampling_method
     ) |>
     slice_sample(prop = 0.5) |>
     ungroup()
 ),

 tar_target(
   model_data_spatial_no_offset,
   bind_rows(
     record_data_spatial_subsample |>
       mutate(weight = 1),
     bg_kmeans_df |>
       mutate(
         data_type = "bg",
         presence = 0,
         n = 0
       )
   )
 ),


 ##############
 #
 # Read in indexed data from offset layers
 #
 ##############

 tar_target(
   model_data_spatial,
   match_offset_data(
     model_data_spatial_no_offset,
     offsets_5
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
       # offset_names,
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
     project_mask_5
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
     covariate_rast_5,
     target_covariate_names,
     model_data_spatial
   )
 ),

 tar_target(
   model_data_spatial_pca,
   get_spatial_values(
     pca_covariate_layers,
     model_data_spatial,
     project_mask_5
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


 # # write out to process from monthly on MAP AWS
 # tar_target(
 #   writemds,
 #   write_csv(
 #     mod_dat_spat,
 #     file = "data/processed/mod_dat_spat.csv"
 #   )
 # ),
 #
 # # read back in from AWS
 # tar_target(
 #   mds,
 #   read_csv(file = "data/processed/mod_dat_spat_updated.csv")
 # ),

 #
 # # plots of offset layers against presence and absence
 # tar_target(
 #   offset_pa_plots,
 #   make_offset_pa_plots(
 #     mod_dat_spat,
 #     target_species,
 #   )
 # ),

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
     model_data_spatial = model_data_spatial,
     target_covariate_names = target_covariate_names,
     target_species = target_species,
     project_mask = project_mask_5,
     image_name = "outputs/images/multisp_pp_count.RData",
     n_burnin = 1000,
     n_samples = 500,
     n_chains = 20
   )
 ),

 # # read in image and predict out raster as a tif
 tar_target(
   pred_file_multisp_pp_count,
   predict_greta_mspp_count(
     image_filename = model_fit_image_multisp_pp_count,
     prediction_layer = covariate_rast_10,
     offset = offsets_avg_10,
     target_species,
     target_covariate_names,
     output_file_prefix = "outputs/rasters/multisp_pp_count"
   )
 ),

 # tar_target(
 #   pred_file_multisp_pp_count_pa_expoff,
 #   add_expert_offset(
 #     predfilelist = pred_file_multisp_pp_count,
 #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
 #     expert_offset_maps = expert_offset_maps_500,
 #     pred_type = "pa"
 #   )
 # ),
 #
 # tar_target(
 #   pred_file_multisp_pp_count_count_expoff,
 #   add_expert_offset(
 #     predfilelist = pred_file_multisp_pp_count,
 #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
 #     expert_offset_maps = expert_offset_maps_500,
 #     pred_type = "count"
 #   )
 # ),


 #
 # #####
 #
 # distribution plots

 # this is the temporary thang until the above are tidied
 tar_terra_rast(
   pred_dist,
   rast(x = pred_file_multisp_pp_count$pa)
 ),

 tar_target(
   distribution_plots,
   make_distribution_plots(
     pred_dist,
     model_data_spatial,
     plot_dir = "outputs/figures/distribution_plots/distn_20260211"
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
     project_mask_5,
     filename = "outputs/figures/rgb_relative_abundance_20251219.png"
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
     plot_dir = "outputs/figures/distribution_plots/distn_20251219_scale"
   )
 ),


 ###########
 ## multispecies pp count with sampling method
 ##

  tar_target(
   model_fit_image_multisp_pp_count_sm,
   fit_model_multisp_pp_count_sm(
     model_data_spatial = model_data_spatial,
     target_covariate_names = target_covariate_names,
     target_species = target_species,
     project_mask = project_mask_5,
     image_name = "outputs/images/multisp_pp_count_sm.RData",
     n_burnin = 1000,
     n_samples = 500,
     n_chains = 50
   )
 ),

 # tar_target(
 #   pred_file_multisp_pp_count_sm,
 #   predict_greta_mspp_count_sm(
 #     image_filename = model_fit_image_multisp_pp_count_sm,
 #     prediction_layer = covariate_rast_10,
 #     offset = offsets_avg_10,
 #     target_species,
 #     output_file_prefix = "outputs/rasters/multisp_pp_count_sm"
 #   )
 # ),
 #
 # # # # read in image and predict out lambda in the absence of offset
 # # median and lower and higher bounds (2.5 and 95%iles)
 # tar_target(
 #   pred_file_lambda_no_offset_sm,
 #   pred_lambda_no_offset(
 #     image_name = model_fit_image_multisp_pp_count_sm,
 #     prediction_layer = covariate_rast_5, # use 10k for faster preds
 #     target_species,
 #     output_file_prefix = "outputs/rasters/multisp_pp_lambda_no_offset_sm",
 #     sm = TRUE, # if predict survey method
 #     nsims = 50 # lower for faster preds
 #   )
 # ),

 tar_target(
   preds_sm,
   predict_lambda(
     image_name = model_fit_image_multisp_pp_count_sm,
     prediction_layer = covariate_rast_10, # use 10k for faster preds
     target_species,
     output_file_prefix = "outputs/rasters/multisp_pp_sm",
     offset = offsets_avg_10,
     sm = TRUE, # if predict survey method
     nsims = 50 # lower for faster preds
   )
 ),

 tar_terra_rast(
   pred_dist_sm,
   rast(preds_sm$p)
 ),

 tar_terra_rast(
   pred_dcv_sm,
   rast(preds_sm$p_cv)
 ),

 # scale predicted distribution
 tar_terra_rast(
   pred_dist_scale_sm,
   scale_predictions(lambda_rast = pred_dist_sm)
 ),

 # tar_target(
 #   pred_file_multisp_pp_count_expoff_sm,
 #   add_expert_offset(
 #     predfilelist = pred_file_multisp_pp_count_sm,
 #     expert_offset_maps = expert_offset_maps_500
 #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
 #   )
 # ),
 #
 # tar_target(
 #   pred_file_multisp_pp_count_expoff_sm_count,
 #   add_expert_offset_count(
 #     predfilelist = pred_file_multisp_pp_count_sm,
 #     expert_offset_maps = expert_offset_maps_500
 #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
 #   )
 # ),
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
 # tar_terra_rast(
 #   pred_dist_sm,
 #   rast(x = pred_file_multisp_pp_count_sm$pa)
 # ),

 tar_target(
   distribution_plots_sm,
   make_distribution_plots(
     pred_dist_sm,
     model_data_spatial,
     plot_dir = "outputs/figures/distribution_plots/distn_20260220_sm"
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
     project_mask_5,
     filename = "outputs/figures/rgb_relative_abundance_20251219_sm.png"
   )
 ),

 tar_target(
   abundance_cubes,
   make_abundance_cubes(
     lambda_no_offset_file = preds_sm$lambda_no_offset,
     offset_stack = aggregate(
       offsets_5[[277:300]],
       fact = 2,
       fun = "mean",
       cores = 4,
       na.rm = TRUE
     ), #subset for purposes of iteration, use full for final
     target_species,
     write_dir = "outputs/rasters/abundance_cubes/"
   )
 ),

 ## variance scaling
 #
 # tar_terra_rast(
 #   pred_dist_scale_sm,
 #   scale_predictions(
 #     pred_file_multisp_pp_count_expoff_sm_count
 #   )
 # ),
 #
 # tar_target(
 #   distribution_plots_scale_sm,
 #   make_distribution_plots(
 #     pred_dist_scale_sm,
 #     model_data_spatial,
 #     plot_dir = "outputs/figures/distribution_plots/distn_20251219_sm_scale"
 #   )
 # ),

 #############################################################################
 #############################################################################
 # 4 species only analysis
  # need to refine this list
  # tar_target(
  #   target_species_4,
  #   target_spp_test_only()
  # ),
  #
  # tar_target(
  #   model_data_records_4,
  #   generate_model_data_records(
  #     full_data_records,
  #     target_species = target_species_4
  #   )
  # ),
  #
  #
  # tar_target(
  #   record_data_spatial_all_4,
  #   get_spatial_values(
  #     lyrs = covariate_rast_5_all,
  #     dat = model_data_records_4,
  #     project_mask_5
  #   )
  # ),
  #
  # tar_target(
  #   record_data_spatial_4,
  #   record_data_spatial_all_4 |>
  #     select(
  #       - all_of(
  #         names(covariate_rast_5_all)[
  #           !names(covariate_rast_5_all) %in%
  #             c(
  #               target_covariate_names,
  #               #offset_names,
  #               bias_names
  #             )
  #         ]
  #       ),
  #     )
  # ),
  #
  # tar_target(
  #   model_data_spatial_4,
  #   bind_rows(
  #     record_data_spatial_4 |>
  #       mutate(weight = 1),
  #     bg_kmeans_df |>
  #       mutate(
  #         data_type = "bg",
  #         presence = 0,
  #         n = 0
  #       )
  #   )
  # ),

  # ## multispecies pp count
  # ##
  #
  # # fit the model in greta
  # # save an image of the environment within function
  # # otherwise the greta model nodes become disconnected and buggered up
  # # because of some R6 nonsense with greta or whatever and the usual targets
  # # shenanigans
  # tar_target(
  #   model_fit_image_multisp_pp_count_4,
  #   fit_model_multisp_pp_count_4spp(
  #     model_data_spatial = model_data_spatial_4,
  #     target_covariate_names = target_covariate_names,
  #     target_species = target_species_4,
  #     project_mask_5,
  #     image_name = "outputs/images/4_multisp_pp_count.RData",
  #     n_burnin = 2000,
  #     n_samples = 1000,
  #     n_chains = 50
  #   )
  # ),
  #
  # # # read in image and predict out raster as a tif
  # tar_target(
  #   pred_file_multisp_pp_count_4,
  #   predict_greta_mspp_count(
  #     image_filename = model_fit_image_multisp_pp_count_4,
  #     prediction_layer = covariate_rast_5,
  #     target_species,
  #     output_file_prefix = "outputs/rasters/4_multisp_pp_count"
  #   )
  # ),
  #
  # tar_target(
  #   pred_file_multisp_pp_count_pa_expoff_4,
  #   add_expert_offset(
  #     predfilelist = pred_file_multisp_pp_count_4,
  #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
  #     expert_offset_maps = expert_offset_maps_500,
  #     pred_type = "pa"
  #   )
  # ),
  #
  # tar_target(
  #   pred_file_multisp_pp_count_count_expoff_4,
  #   add_expert_offset(
  #     predfilelist = pred_file_multisp_pp_count_4,
  #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
  #     expert_offset_maps = expert_offset_maps_500,
  #     pred_type = "count"
  #   )
  # ),




 #####################

  tar_target(
    so_i_dont_have_to_go_backward_and_add_commas,
    print("Targets great in theory but kinda annoying to work with")
  )



)


# tar_load_everything()
# tar_load_globals()
