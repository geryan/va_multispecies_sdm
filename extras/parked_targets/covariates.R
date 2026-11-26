# Parked targets: Covariate layers that are not in the model: the WorldCover
# land cover that the ESA CCI classes replaced on 2026-09-03, the Malaria Atlas
# Project layers, and a few other candidates.
#
# Moved verbatim, still commented out, from _targets.R on 2026-11-24, so the
# pipeline file stays short. Nothing here runs: tar_source() reads only R/.
# Each block left a pointer comment where it was in _targets.R; to restore
# one, paste it back at its pointer and uncomment it.
#
# Blocks:
#   1. WorldCover bare ground (was lines 306-329, just before `dist_from_sea`)
#   2. WorldCover land-cover covariates (was lines 531-546, just before
#      `bioregions_all`)
#   3. Malaria Atlas Project layers (was lines 570-700, just before
#      `soiltype_clay_filled`)
#   4. Other unused covariates (was lines 865-892, just before `bias_tt_raw`)


# ---------------------------------------------------------------------------
# 1. WorldCover bare ground
# ---------------------------------------------------------------------------
  ## bare landcover from worldcover -- REPLACED by the ESA version above
  # tar_terra_rast(
  #   landcover_bare_raw,
  #   gt_lndcvr(
  #     vars = c(
  #       "bare"
  #     ),
  #     ifelse(
  #       user_is_gerry_spartan,
  #       "/data/gpfs/projects/punim1422/va_multispecies_sdm/data/raster/geodata/",
  #       "data/raster/geodata/"
  #     )
  #   )
  # ),
  #
  # tar_terra_rast(
  #   landcover_bare,
  #   landcover_bare_raw |>
  #     aggregate(fact = 5) |>
  #     crop(y = project_mask_5) |>
  #     resample(y = project_mask_5) |>
  #     fill_na_with_nearest_mean(maxRadiusCell = 50) |>
  #     mask(mask = project_mask_5)
  # ),


# ---------------------------------------------------------------------------
# 2. WorldCover land-cover covariates
# ---------------------------------------------------------------------------
  ## landcover vars from worldcover -- REPLACED by landcover_esa_5 above.
  # `landcover_raw` itself is still needed: water_mask_5, and so project_mask_5,
  # are built from it.
  # tar_terra_rast(
  #   landcover_covs,
  #   landcover_raw |>
  #     aggregate(fact = 5) |>
  #     crop(y = project_mask_5) |>
  #     resample(y = project_mask_5) |>
  #     fill_na_with_nearest_mean(maxRadiusCell = 50) |>
  #     mask(mask = project_mask_5) #|> scale()
  #   # don't scale landcover types, so they remain 0-1, and a
  #   # positive-constrained coefficient enforces that more habitat corresponds
  #   # with more mosquitoes
  #
  # ),


# ---------------------------------------------------------------------------
# 3. Malaria Atlas Project layers
# ---------------------------------------------------------------------------
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


# ---------------------------------------------------------------------------
# 4. Other unused covariates
# ---------------------------------------------------------------------------
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


  # # irrigation
  # # The map shows the amount of area equipped for irrigation around the year 2005 in percentage of the total area on a raster with a resolution of 5 minutes.
  # # Stefan Siebert, Verena Henrich, Karen Frenken and Jacob Burke (2013). Global Map of Irrigation Areas version 5. Rheinische Friedrich-Wilhelms-University, Bonn, Germany / Food and Agriculture Organization of the United Nations, Rome, Italy
  # # https://www.fao.org/aquastat/en/geospatial-information/global-maps-irrigated-areas/latest-version
  # tar_terra_rast(
  #   irrigation_raw,
  #   rast(x = "data/raster/gmia_v5_aei_pct.asc") |>
  #     crop(project_mask_10) |>
  #     resample(covariate_rast_10[[1]]) |>
  #     mask(covariate_rast_10[[1]])
  # ),
