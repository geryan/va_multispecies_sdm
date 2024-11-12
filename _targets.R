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
    "tidyterra",
    "geodata",
    "greta",
    "DHARMa"
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


  # aggregated predictor variables, i.e., lower resolution images for quicker processting
  # but use the high res ones for final product
  # # mech layer is set to the minimum value above zero
  tar_terra_rast(
    static_vars_agg_mech_nonzero,
    rast("data/raster/static_vars_agg_mech_nonzero.tif")
  ),

  tar_terra_rast(
    new_mask,
    rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/new_mask.tif")
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


  # tar_terra_rast(
  #   static_vars_standardised,
  #   rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars_std.tif")
  # ),
  #
  #
  # tar_terra_rast(
  #   unstandardised_layers,
  #   rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars.tif")
  # ),

  tar_seed_set(
    tar_seed_create("bg_points")
  ),

  tar_target(
    bg_points,
    terra::spatSample(
      x = static_vars_agg_mech_nonzero[[1]],
      size = 30000,
      na.rm = TRUE,
      as.points = TRUE
    ) %>%
      crds()
  ), #mfing slow but setting of seed stops it rerunning # upped number and not that mfing slow???



  ####################################
  # data wrangling and cleaning
  ###################################
 tar_target(
   old_raw_data_file,
   "data/tabular/update_speciescols_6_FINAL_with_absences_20241105_oldMAP.csv",
   format = "file"
 ),

 tar_target(
   old_raw_data,
   read_csv(
    file = old_raw_data_file,
    guess_max = 30000
   )
 ),

 tar_target(
   new_raw_data_file,
   "data/tabular/va_data_merged_20241105.csv",
   format = "file"
 ),

 tar_target(
   new_raw_data,
   read_csv(
     file = new_raw_data_file,
     guess_max = 30000
   )
 ),

 tar_target(
   raw_data,
   combine_raw_data(
     old_raw_data,
     new_raw_data
   )
 ),


 tar_target(
   data_records,
   make_data_records(
     raw_data,
     static_vars_agg_mech_nonzero[[1]]
   )
 ),

 tar_target(
   record_table,
   table(
     data_records$species,
     data_records$pa
   )
 ),

 tar_target(
   record_tbl,
   tibble(
     species = rownames(record_table),
     pa = record_table[,1],
     po = record_table[,2]
   ) |>
     mutate(
       n = pa + po
     ) |>
     arrange(desc(n))
 ),

 tar_target(
   target_species,
   target_spp()
 ),

 tar_target(
   plot_points,
   make_plot_points(
     data_records,
     target_species
   )
 ),


 tar_target(
   model_data_ragged,
   make_model_data_ragged(
     data_records,
     bg_points,
     target_species
   )
 ),

 tar_target(
   model_notna_idx_pa,
   get_notna_idx(
     model_data_ragged,
     type = "pa"
   )
 ),

 tar_target(
   model_notna_idx_po,
   get_notna_idx(
     model_data_ragged,
     type = "po"
   )
 ),

 tar_target(
   spatial_values,
   get_spatial_values(
     lyrs = static_vars_agg_mech_nonzero,
     dat = model_data_ragged,
     bgs = bg_points
   )
 ),


 ## detailed data records

 tar_target(
   detailed_data_records,
   make_detailed_data_records(
     raw_data,
     static_vars_agg_mech_nonzero[[1]]
   )
 ),


 ## plots before modelling

 tar_target(
   covs_plots,
   make_covariate_plots(
     spatial_values,
     target_species,
     model_notna_idx_pa,
     model_notna_idx_po
   )
 ),

 tar_target(
   simple_point_plots,
   make_point_plots(
     plot_points,
     expert_maps,
     new_mask
   )
 ),

 ################
 ## models
 ################

 ##
 ## multispecies pp with biophysical offset
 ##

 tar_target(
   model_fit_image_multisp_pp_with_offset,
   fit_model_multisp_pp_with_offset(
     model_data_ragged,
     spatial_values,
     model_notna_idx_pa,
     model_notna_idx_po,
     image_name = "outputs/images/multisp_pp_with_offset.RData",
     n_burnin = 1000,
     n_samples = 1000,
     n_chains = 4
   )
 ),

 tar_target(
   pred_file_multisp_pp_with_offset,
   predict_greta_mspp_with_offset(
     image_filename = model_fit_image_multisp_pp_with_offset,
     prediction_layer = static_vars_agg_mech_nonzero,
     target_species,
     output_file = "outputs/rasters/multisp_pp_with_offset.tif"
   )
 ),

 tar_terra_rast(
   pred_multisp_pp_with_offset,
   rast(pred_file_multisp_pp_with_offset)
 ),

 tar_target(
   posterior_multisp_pp_with_offset,
   calculate_posterior_multisp_pp_with_offset(
     image_filename = model_fit_image_multisp_pp_with_offset
   )
 ),

 ##
 ## multispecies pp with biophysical offset count
 ##

 tar_target(
   model_fit_image_multisp_pp_with_offset_count,
   fit_model_multisp_pp_with_offset_count(
     model_data_ragged,
     spatial_values,
     model_notna_idx_pa,
     model_notna_idx_po,
     image_name = "outputs/images/multisp_pp_with_offset_count.RData",
     n_burnin = 1000,
     n_samples = 1000,
     n_chains = 4
   )
 ),

 tar_target(
   pred_file_multisp_pp_with_offset_count,
   predict_greta_mspp_with_offset_count(
     image_filename = model_fit_image_multisp_pp_with_offset_count,
     prediction_layer = static_vars_agg_mech_nonzero,
     target_species,
     output_file = "outputs/rasters/multisp_pp_with_offset_count.tif"
   )
 ),

 tar_terra_rast(
   pred_multisp_pp_with_offset_count,
   rast(pred_file_multisp_pp_with_offset_count)
 ),

 tar_terra_rast(
   pred_multisp_pp_with_offset_count_p_presence,
   poisson_to_prob(
     pred_multisp_pp_with_offset_count,
     filename = "outputs/rasters/multisp_pp_with_offset_count_p_presence.tif"
   )
 ),



 # tar_target(
 #   posterior_multisp_pp_with_offset,
 #   calculate_posterior_multisp_pp_with_offset(
 #     image_filename = model_fit_image_multisp_pp_with_offset
 #   )
 # ),


 ##
 ## multispecies pp (no offset)
 ##

 tar_target(
   model_fit_image_multisp_pp,
   fit_model_multisp_pp(
     model_data_ragged,
     spatial_values,
     model_notna_idx_pa,
     model_notna_idx_po,
     image_name = "outputs/images/multisp_pp.RData",
     n_burnin = 1000,
     n_samples = 1000,
     n_chains = 4
   )
 ),

 tar_target(
   pred_file_multisp_pp,
   predict_greta_mspp(
     image_filename = model_fit_image_multisp_pp,
     prediction_layer = static_vars_agg_mech_nonzero,
     target_species,
     output_file = "outputs/rasters/multisp_pp.tif"
   )
 ),

 tar_terra_rast(
   pred_multisp_pp,
   rast(pred_file_multisp_pp)
 ),

 # tar_target(
 #   plots_pred_multisp_pp,
 #   multitude_of_plots(
 #
 #   )
 # ),

 ##
 ## multispecies pp (no offset) with count
 ##
 tar_target(
   model_fit_image_multisp_pp_count,
   fit_model_multisp_pp_count(
     model_data_ragged,
     spatial_values,
     model_notna_idx_pa,
     model_notna_idx_po,
     image_name = "outputs/images/multisp_pp_count.RData",
     n_burnin = 1000,
     n_samples = 1000,
     n_chains = 4
   )
 ),

 tar_target(
   pred_file_multisp_pp_count,
   predict_greta_mspp_count(
     image_filename = model_fit_image_multisp_pp_count,
     prediction_layer = static_vars_agg_mech_nonzero,
     target_species,
     output_file = "outputs/rasters/multisp_pp_count.tif"
   )
 ),

 tar_terra_rast(
   pred_multisp_pp_count,
   rast(pred_file_multisp_pp_count)
 ),

 tar_terra_rast(
   pred_multisp_pp_count_p_presence,
   poisson_to_prob(
     pred_multisp_pp_count,
     filename = "outputs/rasters/multisp_pp_count_p_presence.tif"
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
