library(targets)
library(geotargets)
library(targets.utils) ## pak::pak("geryan/targets.utils")

tar_option_set(
  packages = c(
    #"tibble",
    "dplyr",
    "sdmtools",  # remotes::install_github("idem-lab/sdmtools@no_malariaAtlas") #this version is lighter and does everything needed without importing malariaAtlas and multispeciesPP
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
    # "see"
    "MCMCvis"
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

  tar_target(
    user_is_gerry_spartan,
    Sys.info()[["user"]] == "ryange"
  ),

  # read in offset layers

  # all raw layers
  tar_terra_rast(
    offsets_raw,
    read_offset_data(
      odir = ifelse(
        user_is_nick,
        "../mosmicrosim/processing/vector_rasters",
        ifelse(
          user_is_gerry_spartan,
          "/data/gpfs/projects/punim1422/vector_rasters/",
          "/Users/gryan/Dropbox/vector_rasters/"
        )
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

  tar_terra_rast(
    landcover_raw,
    gt_lndcvr(
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
      path = ifelse(
        user_is_gerry_spartan,
        "/data/gpfs/projects/punim1422/va_multispecies_sdm/data/raster/geodata/",
        "data/raster/geodata/"
      )
    )
  ),


  tar_terra_rast(
    water_mask_5,
    make_water_mask(
      water = landcover_raw[["water"]],
      proj_mask = project_mask_5_outline
    )
  ),

  # generate project mask based on water mask and outline of landform from offset layers
  tar_terra_rast(
    project_mask_5,
    project_mask_5_outline |>
      mask(mask = water_mask_5)
  ),

  # bare landcover
  tar_terra_rast(
    landcover_bare_raw,
    gt_lndcvr(
      vars = c(
        "bare"
      ),
      ifelse(
        user_is_gerry_spartan,
        "/data/gpfs/projects/punim1422/va_multispecies_sdm/data/raster/geodata/",
        "data/raster/geodata/"
      )
    )
  ),

  tar_terra_rast(
    landcover_bare,
    landcover_bare_raw |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      fill_na_with_nearest_mean(maxRadiusCell = 50) |>
      mask(mask = project_mask_5)
  ),

  tar_terra_rast(
    landcover_bare_10,
    landcover_bare |>
      aggregate(
        fact = 2,
        fun = mean,
        na.rm = TRUE
      )
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

  # One Earth Bioregions SpatVector file
  tar_terra_vect(
    oneearth_vect,
    make_oneearth(
      mask = project_mask_5
    )
  ),

  # multiband raster of smoothed dummy variables for bioregions
  tar_terra_rast(
    bioregion_layers,
    make_smooth_dummies(
      oneearth_spatvector = oneearth_vect,
      mask_lyr = project_mask_5,
      level = "bioregion"
    )
  ),

  # multiband raster of smoothed dummy variables for subrealms
  tar_terra_rast(
    subrealm_layers,
    make_smooth_dummies(
      oneearth_vect,
      mask = project_mask_5,
      level = "subrealm"
    )
  ),

  # the names of the dummy variables
  tar_target(
    subrealm_names,
    names(subrealm_layers)  |>
      setdiff(
        c(
          "Greater Arabian Peninsula",
          "North Africa",
          "Southern Afrotropics"
        )
      )
  ),

  # set the bioregion names, excluding some bioregions that are outside the area
  # we are modelling
  tar_target(
    bioregion_names,
    names(bioregion_layers) |>
      setdiff(
        c(
          #"Sahel Acacia Savannas", # points inthis region for arabiensis so shoudld include it
          "Southern Sahara Deserts & Mountain Woodlands",
          "South Mediterranean Mixed Woodlands & Forests",
          "Northern Sahara Deserts, Savannas, & Marshes",
          "Red Seas, Arabian Deserts & Salt Marshes",
          "Seychelles & Comoros Tropical Islands", # such a small area probably not worth estimating
          "St Helena & Ascension Islands",
          "South African Cape Shrublands & Mountain Forests"
        )
      )
  ),

  #
  tar_target(
    bioregion_names_mask,
    names(bioregion_layers) |>
      setdiff(
        c(
          #"Sahel Acacia Savannas",
          "Southern Sahara Deserts & Mountain Woodlands",
          "South Mediterranean Mixed Woodlands & Forests",
          "Northern Sahara Deserts, Savannas, & Marshes",
          "Red Seas, Arabian Deserts & Salt Marshes",
          #"Seychelles & Comoros Tropical Islands", # want to plot comoros
          "St Helena & Ascension Islands",
          "South African Cape Shrublands & Mountain Forests"
        )
      )
  ),

  # create a mask of only the areas we wish to model in
  tar_terra_rast(
    bioregion_mask_5,
    make_bioregion_mask(
      oneearth_vect,
      bioregion_names_mask,
      project_mask_5
    )
  ),

  tar_terra_rast(
    bioregion_mask_10,
    aggregate(
      x = bioregion_mask_5,
      fact = 2,
      fun = "max",
      na.rm = TRUE
    )
  ),

  # combine bare ground and bioregions of interest into landscape mask
  # that represents area we believe is extent of boundary of these species
  tar_terra_rast(
    landscape_mask_10,
    bioregion_mask_10 * (1 - landcover_bare_10)
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

  tar_terra_rast(
    offsets_10,
    aggregate(
      offsets_5,
      fact = 2,
      fun = "mean",
      na.rm = TRUE,
      cores = 4
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

  tar_terra_vect(
    bioregions_v,
    get_bioregions(
      project_mask = project_mask_5,
      vect = TRUE
    )
  ),

  # this returns a stack of layers 1 bioregion each
  tar_terra_rast(
    bioregion_stack,
    split_bioregions(project_mask = project_mask_5)
  ),

  # layers from Malaria Atlas Project
  #

  # # built volume
  #
  # tar_terra_rast(
  #   built_volume_raw,
  #   rast("/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/GHSL_2023/GHS_BUILT_V_R23A.2020.Annual.Data.1km.Data.tif") |>
  #   set_layer_names("built_volume")
  # ),
  #
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
  # # EVI
  # # monthly EVI available from MAP, need to work out some code to process this
  # # stuff on MAP workbench so that I don't need to download all the enormous
  # # layers
  # tar_terra_rast(
  #   evi_raw,
  #   prepare_multi_layer(
  #     data_dir ="/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/EVI/"#,
  #     # layer_prefix = "evi",
  #     # file_id_prefix = ".*v6\\.",
  #     # file_id_suffix = "\\.Annual.*"
  #   )
  # ),
  #
  # tar_terra_rast(
  #   evi_5,
  #   evi_raw |>
  #     mean() |>
  #     set_layer_names("evi") |>
  #     crop(y = project_mask_5) |>
  #     aggregate(fact = 5) |>
  #     resample(y = project_mask_5) |>
  #     fill_na_with_nearest_mean(maxRadiusCell = 50) |>
  #     mask(mask = project_mask_5) |>
  #     scale()
  # ),
  #
  #
  # # Tasseled Cap Brightness
  # # same for
  # # TCB re monthly data
  # tar_terra_rast(
  #   tcb_raw,
  #   prepare_multi_layer(
  #     data_dir ="/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/TCB/"
  #   )
  # ),
  #
  # tar_terra_rast(
  #   tcb_5,
  #   tcb_raw |>
  #     mean() |>
  #     set_layer_names("tcb") |>
  #     crop(y = project_mask_5) |>
  #     aggregate(fact = 5) |>
  #     resample(y = project_mask_5) |>
  #     fill_na_with_nearest_mean(maxRadiusCell = 50) |>
  #     mask(mask = project_mask_5) |>
  #     scale()
  # ),
  #
  #
  # # land surface temperature at night annual mean
  # # also same for lst re monthly data
  # tar_terra_rast(
  #   lst_night_raw,
  #   prepare_multi_layer(
  #     data_dir ="/Users/gryan/Documents/tki_work/vector_atlas/africa_spatial_data/data/raster/MAP_covariates/LST_Night/"
  #   )
  # ),
  #
  # tar_terra_rast(
  #   lst_night_5,
  #   lst_night_raw |>
  #     mean() |>
  #     set_layer_names("lst_night") |>
  #     aggregate(fact = 5) |>
  #     crop(y = project_mask_5) |>
  #     resample(y = project_mask_5) |>
  #     fill_na_with_nearest_mean(maxRadiusCell = 50) |>
  #     mask(mask = project_mask_5) |>
  #     scale()
  # ),
  #
  # #
  # # via `geodata`
  # #
  #
  # # elevation
  #
  # tar_terra_rast(
  #   elevation_raw,
  #   global_regions |>
  #     filter(continent == "Africa") |>
  #     pull(iso3) |>
  #     lapply(
  #       FUN = function(x){
  #         elevation_30s(
  #           country = x,
  #           path = "data/raster/geodata"
  #         )
  #       }
  #     ) |>
  #     sprc() |>
  #     merge() |>
  #     set_layer_names("elevation")
  # ),
  #
  # tar_terra_rast(
  #   elevation_5,
  #   elevation_raw |>
  #     aggregate(fact = 5) |>
  #     crop(y = project_mask_5) |>
  #     resample(y = project_mask_5) |>
  #     fill_na_with_nearest_mean(maxRadiusCell = 50) |>
  #     mask(mask = project_mask_5) |>
  #     scale()
  # ),
  #
  # soil clay

  tar_terra_rast(
    soiltype_clay_filled,
    get_soil_af(var = "clay") |>
      set_layer_names("clay")
  ),

  tar_terra_rast(
    soiltype_clay_5,
    soiltype_clay_filled |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      mask(mask = project_mask_5) |>
      scale()
  ),

  tar_terra_rast(
    soiltype_silt_filled,
    get_soil_af(var = "silt") |>
      set_layer_names("silt")
  ),

  tar_terra_rast(
    soiltype_silt_5,
    soiltype_silt_filled |>
      aggregate(fact = 5) |>
      crop(y = project_mask_5) |>
      resample(y = project_mask_5) |>
      mask(mask = project_mask_5) |>
      scale()
  ),

  tar_terra_rast(
    soiltype_layers,
    c(
      soiltype_clay_5,
      soiltype_silt_5
    )
  ),

  tar_target(
    soiltype_names,
    names(soiltype_layers)
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
      #scale()
      scale_rast_to_1()
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

  # tar_terra_rast(
  #   offset_temp_5,
  #   make_temperature_offset(project_mask_5)
  # ),

  #
  # bias
  # travel time from research facilities
  # layer created in
  # https://github.com/geryan/africa_anopheles_sampling_bias

  tar_terra_rast(
    bias_tt_raw,
    if (user_is_nick | user_is_gerry_spartan) {
      rast("data/raster/tt_by_country.tif")
    } else{
      rast("/Users/gryan/Documents/tki_work/vector_atlas/africa_anopheles_sampling_bias/outputs/tt_by_country.tif")
    }
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
      landcover_covs,
      prox_to_sea,
      bias_tt_5
    )
  ),

  tar_target(
    target_covariate_names,
    c(

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

      # also footprint for anthropophilic/anthropophagic spp
      "footprint",

      # and proximity to sea, for merus and melas
      "prox_to_sea"

    )
  ),

  tar_target(
    bias_names,
    "travel_time"
  ),

  tar_target(
    offset_names,
    #"offset_temp"
    NULL
  ),

  tar_terra_rast(
    covariate_rast_5_all,
    c(
      landcover_covs,
      prox_to_sea,

      soiltype_layers,

      footprint_5,

      subrealm_layers,
      bioregion_layers,

      #offset_temp_5,

      bias_tt_5
    )
  ),

  tar_terra_rast(
    covariate_rast_5,
    subset_covariate_rast(
      covariate_rast_5_all,
      target_covariate_names = target_covariate_names,
      subrealm_names = subrealm_names,
      bioregion_names = bioregion_names,
      soiltype_names = soiltype_names,
      offset_names = offset_names,
      bias_names = bias_names
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

  tar_terra_vect(
    pharoensis_expert_map,
    get_pharoensis_expert_map()
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
    expert_offset_maps_10,
    expert_offset_maps |>
      aggregate(
        fact = 2,
        fun = mean,
        na.rm = TRUE
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

  tar_terra_rast(
    expert_offset_maps_10_500,
    expert_offset_maps_500 |>
      aggregate(
        fact = 2,
        fun = mean,
        na.rm = TRUE
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
   #"data/tabular/VA_DATA_20260218.csv",
   "data/tabular/va.data_20260427.csv",
   format = "file"
 ),

 tar_target(
   raw_data,
   read_csv(
     file = raw_data_file,
     guess_max = 100000 # some cols are largely empty until end, so needs this
     # or will assume they are logical and break when they are not
     # NB this is longer than the entire data set (for now...)
   ) |>
     filter(
       !(source_id == 1002611 & species == "moucheti"),
       !(source_id == 4682 & species == "merus")
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

 tar_terra_vect(
   presences_outside_expert_range,
   get_presences_outside_expert_range(
     full_data_records,
     expert_maps
   )
 ),

 tar_target(
   presences_outside_expert_range_plot,
   plot_presences_outside_expert_range(
     presences_outside_expert_range,
     expert_maps
   )
 ),


 tar_target(
   species_unique_location_records,
   full_data_records |>
     select(species, latitude, longitude) |>
     distinct() |>
     group_by(species) |>
     summarise(n = n()) |>
     arrange(desc(n)) |>
     print(n = 999)
 ),

 # need to refine this list
 tar_target(
   target_species,
   target_spp()
 ),


 tar_target(
   model_data_records,
   generate_model_data_records(
     full_data_records,
     target_species = target_species
   )
 ),

 tar_target(
   species_unique_presence_location_records,
   model_data_records |>
     filter(!inferred) |>
     select(species, presence, latitude, longitude) |>
     distinct() |>
     group_by(species, presence) |>
     summarise(n = n()) |>
     mutate(
       presence = ifelse(
         presence == 1,
         "present",
         "absent"
       )
     ) |>
     pivot_wider(
       names_from = "presence",
       values_from = "n"
     ) |>
     arrange(species) |>
     print(n = 999)

 ),




 # tar_target(
 #   model_data_records_ni,
 #   generate_model_data_records_no_impute(
 #     full_data_records,
 #     target_species = target_species
 #   )
 # ),

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

 # get only the variables of interest
 tar_target(
   record_data_spatial,
   record_data_spatial_all |>
     select(
       - all_of(
         names(covariate_rast_5_all)[
           !names(covariate_rast_5_all) %in%
             c(
               target_covariate_names,
               subrealm_names,
               bioregion_names,
               soiltype_names,
               offset_names,
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
   record_data_spatial #|>
     # group_by(
     #   species,
     #   data_type,
     #   sampling_method
     # ) |>
     # slice_sample(prop = 0.5) |>
     # ungroup()
 ),

 # put together with background data
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

 # table 2 in manuscript
 tar_target(
   data_summary_table,
   make_data_summary_table(model_data_spatial),
 ),

 # table summary of number of records where each species
 # was detected or not detected
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

 # make buffered convex hull for species without expert opn layer
 tar_terra_vect(
   convex_hulls,
   # consider adding buffer to hull before buffering
   make_convex_hull(
     record_data_spatial,
     expert_maps
   )
 ),

 tar_terra_vect(
   bioregion_hulls,
   make_bioregion_hull(
     record_data_spatial,
     expert_maps,
     bioregions_v
   )
 ),

 tar_terra_vect(
   point_hulls_100,
   make_point_hull(
     record_data_spatial,
     expert_maps,
     buffer_width = 1e5
   )
 ),

 tar_terra_vect(
   point_hulls_500,
   make_point_hull(
     record_data_spatial,
     expert_maps,
     buffer_width = 5e5
   )
 ),

 tar_terra_vect(
   point_hulls_1000,
   make_point_hull(
     record_data_spatial,
     expert_maps,
     buffer_width = 1e6
   )
 ),



 # make hulls with buffer around points and polygonise
 # send vector / mapped versions with no fuzzy buffer
 # send to MS with maps of points

 # make offset layers from these hulls
 #
 #
 tar_terra_rast(
   convex_offset_maps,
   make_expert_offset_maps(
     convex_hulls,
     project_mask_5,
     buffer_km = 1000
   )
 ),

 tar_terra_rast(
   bioregion_offset_maps,
   make_expert_offset_maps(
     bioregion_hulls,
     project_mask_5,
     buffer_km = 1000
   )
 ),

 tar_terra_rast(
   range_offsets,
   combine_range_maps(
     expert_offset_maps,
     non_expert_offset_maps = bioregion_offset_maps,
     target_species,
     project_mask = project_mask_5
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

 # ######
 # # PCA Covariate layers
 # ######
 # tar_terra_rast(
 #   pca_covariate_layers,
 #   make_pca_covariate_layers(
 #     covariate_rast_5,
 #     target_covariate_names,
 #     model_data_spatial
 #   )
 # ),
 #
 # tar_target(
 #   model_data_spatial_pca,
 #   get_spatial_values(
 #     pca_covariate_layers,
 #     model_data_spatial,
 #     project_mask_5
 #   ) |>
 #     select(
 #       - all_of(target_covariate_names),
 #     )
 # ),
 #
 # tar_target(
 #   covs_plots_pca,
 #   make_covariate_plots(
 #     model_data_spatial_pca,
 #     cvnames = names(pca_covariate_layers),
 #     fname = "outputs/figures/cov_violins_pca.png"
 #   )
 # ),


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

 # ## multispecies pp count
 # ##
 #
 # # fit the model in greta
 # # save an image of the environment within function
 # # otherwise the greta model nodes become disconnected and buggered up
 # # because of some R6 nonsense with greta or whatever and the usual targets
 # # shenanigans
 # tar_target(
 #   model_fit_image_multisp_pp_count,
 #   fit_model_multisp_pp_count(
 #     model_data_spatial = model_data_spatial,
 #     target_covariate_names = target_covariate_names,
 #     target_species = target_species,
 #     project_mask = project_mask_5,
 #     image_name = "outputs/images/multisp_pp_count.RData",
 #     n_burnin = 1000,
 #     n_samples = 500,
 #     n_chains = 20
 #   )
 # ),
 #
 # # # read in image and predict out raster as a tif
 # tar_target(
 #   pred_file_multisp_pp_count,
 #   predict_greta_mspp_count(
 #     image_filename = model_fit_image_multisp_pp_count,
 #     prediction_layer = covariate_rast_10,
 #     offset = offsets_avg_10,
 #     target_species,
 #     target_covariate_names,
 #     output_file_prefix = "outputs/rasters/multisp_pp_count"
 #   )
 # ),
 #
 # # tar_target(
 # #   pred_file_multisp_pp_count_pa_expoff,
 # #   add_expert_offset(
 # #     predfilelist = pred_file_multisp_pp_count,
 # #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
 # #     expert_offset_maps = expert_offset_maps_500,
 # #     pred_type = "pa"
 # #   )
 # # ),
 # #
 # # tar_target(
 # #   pred_file_multisp_pp_count_count_expoff,
 # #   add_expert_offset(
 # #     predfilelist = pred_file_multisp_pp_count,
 # #     #expert_offset_maps = rast("outputs/rasters/va_plots_20250718/expert_offset_aggregated.tif")
 # #     expert_offset_maps = expert_offset_maps_500,
 # #     pred_type = "count"
 # #   )
 # # ),
 #
 #
 # #
 # # #####
 # #
 # # distribution plots
 #
 # # this is the temporary thang until the above are tidied
 # tar_terra_rast(
 #   pred_dist,
 #   rast(x = pred_file_multisp_pp_count$pa)
 # ),
 #
 # tar_target(
 #   distribution_plots,
 #   make_distribution_plots(
 #     pred_dist,
 #     model_data_spatial,
 #     plot_dir = "outputs/figures/distribution_plots/distn_20260211"
 #   )
 # ),
 #
 # ## relative abundance
 #
 #
 # # this is making the gambiae-coluzzii layer inside this function
 # # if we get the hierarchical species complex model in, be sure to still do this
 # # as the gam-col layer would (should :grimace:) only be the shared complex
 # # thingies, not the joint species level ones
 # tar_terra_rast(
 #   rel_abund_rgb,
 #   make_rel_abund_rgb(
 #     #x = pred_dist_rgb,
 #     x = pred_dist,
 #     threshold = 0.05
 #   ),
 #   datatype = "INT1U"
 # ),
 #
 # tar_target(
 #   rel_abund_plots,
 #   make_rel_abund_rgb_plot(
 #     rel_abund_rgb,
 #     project_mask_5,
 #     filename = "outputs/figures/rgb_relative_abundance_20251219.png"
 #   )
 # ),
 #
 # ## variance scaling
 #
 # tar_terra_rast(
 #   pred_dist_scale,
 #   scale_predictions(
 #     lambda_file = pred_file_multisp_pp_count_count_expoff
 #   )
 # ),
 #
 # tar_target(
 #   distribution_plots_scale,
 #   make_distribution_plots(
 #     pred_dist_scale,
 #     model_data_spatial,
 #     plot_dir = "outputs/figures/distribution_plots/distn_20251219_scale"
 #   )
 # ),


 ###########
 ## multispecies pp count with sampling method
 ##

  tar_target(
   model_fit_image_multisp_pp_count_sm,
   fit_model_multisp_pp_count_sm(
     model_data_spatial = model_data_spatial,
     target_covariate_names = target_covariate_names,
     target_species = target_species,
     # subrealm_names = subrealm_names,
     bioregion_names = bioregion_names,
     soiltype_names = soiltype_names,
     project_mask = project_mask_5,
     image_name = "outputs/images/multisp_pp_count_sm.RData",
     n_burnin = 5000,
     n_samples = 1000,
     n_chains = 50
   )
 ),

 tar_target(
   resids_multisp_pp_count_sm,
   validation_and_checking(
     model_fit_image_multisp_pp_count_sm,
     nsims = 100
   )
 ),

 tar_target(
   m6_fit,
   fit_m6(
     image_name = "m6_fit.RData",
     model_data_spatial = model_data_spatial,
     target_covariate_names = target_covariate_names,
     target_species = target_species,
     subrealm_names = subrealm_names,
     bioregion_names = bioregion_names,
     soiltype_names = soiltype_names,
     project_mask = project_mask_5,
     n_burnin = 2000,
     n_samples = 1000,
     n_chains = 50,
     n_cores = 6
   )
 ),

 tar_target(
   resids_m6,
   validation_and_checking(
     m6_fit,
     nsims = 100,
     plotdir = "outputs/figures/validation/20260605/"
   )
 ),


 # tar_target(
 #   pred_file_multisp_pp_count_sm,
 #   predict_greta_mspp_count_sm(
 #     image_filename = model_fit_image_multisp_pp_count_sm,
 #     prediction_layer = covariate_rast_10,
 #     offset = offsets_avg_10,
 #     target_species = target_species,
 #     # NB the image above is loaded with load()
 #     # which may cause problems with these _names args if they are
 #     # different from in the fit function.
 #     # Though they should be the same anyway - can't think of a situation
 #     # where we could provide different ones to predict from fit anyway
 #     # leaving with this note but commentint out this function as
 #     # predict_lambda is the better one to use.
 #     target_covariate_names = target_covariate_names,
 #     # subrealm_names = subrealm_names,
 #     bioregion_names = bioregion_names,
 #     soiltype_names = soiltype_names,
 #     output_file_prefix = "outputs/rasters/multisp_pp_count_sm"
 #   )
 # ),

 # tar_target(
 #   preds_sm,
 #   predict_lambda_m6(
 #     #image_name = model_fit_image_multisp_pp_count_sm,
 #     image_name = m6_fit,
 #     prediction_layer = covariate_rast_10, # use 10k for faster preds
 #     target_species,
 #     output_file_prefix = "outputs/rasters/multisp_pp_sm",
 #     offset = offsets_avg_10,
 #     sm = TRUE, # if predict survey method
 #     nsims = 100 # lower for faster preds
 #   )
 # ),

 tar_target(
   preds_sm,
   predict_lambda_m6(
     #image_name = model_fit_image_multisp_pp_count_sm,
     image_name = m6_fit,
     prediction_layer = covariate_rast_10, # use 10k for faster preds
     target_species,
     output_file_prefix = "outputs/rasters/multisp_pp_sm",
     offset = offsets_avg_10,
     sm = TRUE, # if predict survey method
     nsims = 100 # lower for faster preds
   )
 ),



 tar_terra_rast(
   pred_dist_not_masked,
   # rast(preds_sm$p)
   rast("spartan_model_comparison/m6/m6_p.tif")
 ),

 tar_terra_rast(
   pred_p,
   mask_landcover_and_expert_offset(
     p = pred_dist_not_masked,
     expert = expert_offset_maps_10,
     bare = landcover_bare_10
   )
 ),

 tar_target(
   plot_pred_p,
   make_distribution_plots(
     pred_p,
     model_data_spatial,
     plot_dir = "outputs/figures/distribution_plots/distn_20260601",
     cola = "yellow"
   )
 ),

 tar_terra_rast(
   pred_pcv_not_masked,
   # rast(preds_sm$p_cv)
   rast("spartan_model_comparison/m6/m6_p_cv.tif")
 ),

 tar_terra_rast(
   pred_pcv,
   mask_landcover_and_expert_offset(
     p = pred_pcv_not_masked,
     expert = expert_offset_maps_10,
     bare = landcover_bare_10
   )
 ),


 tar_terra_rast(
   pred_lambda_mean_not_masked,
   rast("spartan_model_comparison/m6/m6.tif") * offsets_avg_10
   #rast("outputs/rasters/via_spartan/multisp_pp_sm.tif") * offsets_avg_10
 ),

 tar_terra_rast(
   pred_gr1,
   app(
     x = pred_lambda_mean,
     fun = function(x){
       any(x >= 0.1)
     }
   )
 ),

 tar_terra_rast(
   pred_dominant,
   get_dominant_species(
     pred_lambda_mean,
     target_species
   ),
   #datatype = "INT1U"
   #filetype = "GTiff",
   preserve_metadata = "zip"
 ),

 # scale predicted distribution
 tar_terra_rast(
   pred_dist_scale_sm,
   scale_predictions(
     lambda_rast = pred_lambda_mean
   )
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
     plot_dir = "outputs/figures/distribution_plots/distn_20260310_sm"
   )
 ),

 ## relative abundance

 tar_terra_rast(
   rel_abund_rgb_sm,
   make_rel_abund_rgb(
     #x = pred_dist_rgb,
     x = pred_p,
     threshold = 0.05
   ),
   datatype = "INT1U"
 ),

 tar_target(
   rel_abund_plots_sm,
   make_rel_abund_rgb_plot(
     rel_abund_rgb_sm,
     project_mask_5,
     filename = "outputs/figures/rgb_relative_abundance_20260601.png"
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


 #####################

  tar_target(
    so_i_dont_have_to_go_backward_and_add_commas,
    print("Targets great in theory but kinda annoying to work with")
  )



)


# tar_load_everything()
# tar_load_globals()
