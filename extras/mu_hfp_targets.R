# Time-varying Human Footprint targets.
#
# Replaces the static `footprint_raw` / `footprint_5` pair (geodata, 2009) with
# the Mu et al. (2022) annual series. Written as a standalone list() for
# splicing into _targets.R.
#
# Mu, H., Li, X., Wen, Y., Huang, J., Du, P., Su, W., Miao, S., Geng, M. (2022)
# A global record of annual terrestrial Human Footprint dataset from 2000 to
# 2018. Scientific Data 9:176. doi:10.1038/s41597-022-01284-8
# Data: figshare doi:10.6084/m9.figshare.16571064

list(

  # figshare deposit. The version is pinned deliberately: `mu_hfp_manifest` is
  # a dependency of every download branch, so if it tracked "current" it would
  # change the moment the depositor added a year and re-download all 11 GiB.
  # Raise it on purpose to take up new years.
  tar_target(
    mu_hfp_article,
    16571064L
  ),

  tar_target(
    mu_hfp_version,
    8L
  ),

  # DO NOT hardcode the year range. The paper and the deposit's own description
  # both say 2000-2018; version 8 actually holds 2000-2024. The years come off
  # the API.
  tar_target(
    mu_hfp_manifest,
    mu_hfp_figshare_manifest(
      article = mu_hfp_article,
      version = mu_hfp_version
    )
  ),

  # split out of the manifest so each download branch depends only on its own
  # year, url and checksum
  tar_target(
    mu_hfp_years,
    mu_hfp_manifest$year
  ),

  tar_target(
    mu_hfp_urls,
    mu_hfp_manifest$download_url
  ),

  tar_target(
    mu_hfp_md5s,
    mu_hfp_manifest$supplied_md5
  ),

  # Download, verify against supplied_md5, reproject from the native Mollweide
  # 1 km grid onto project_mask_5, write the small African raster, and delete
  # the zip and the global GeoTIFF.
  #
  # Neither source is kept: the 25 zips are 11 GiB and inflate to 45 GiB of
  # global raster that is almost entirely ocean and other continents. The
  # target's value is the ~7 MB output file, so deleting the sources does not
  # invalidate anything -- download_mu_hfp() returns an existing valid output
  # immediately, without touching figshare. That also makes an interrupted run
  # resumable, and lets files fetched outside the pipeline be adopted by it.
  #
  # The cost of that choice: changing `project_mask_5` (or the manifest) means
  # re-downloading. Pass `keep_source = TRUE` if the mask is in flux.
  tar_target(
    mu_hfp_year,
    download_mu_hfp(
      year = mu_hfp_years,
      url = mu_hfp_urls,
      md5 = mu_hfp_md5s,
      new_mask = project_mask_5,
      outputdir = "outputs/raster/mu_hfp",
      workdir = "data/raw/mu_hfp",
      keep_source = FALSE
    ),
    pattern = map(mu_hfp_years, mu_hfp_urls, mu_hfp_md5s),
    format = "file"
  ),

  # the deliverable: one file, one layer per year, with a time axis.
  #
  # This is also where the series is scaled, and it has to be here, because
  # this is the first point at which all the years are in hand. scale_rast_to_1()
  # divides each layer by its OWN maximum, which on a 25-year stack would give
  # every year a different divisor and turn a constant cell into a spurious
  # trend. stack_mu_hfp() divides the whole series by one constant instead.
  tar_target(
    mu_hfp_all,
    stack_mu_hfp(
      paths = mu_hfp_year,
      varname = "footprint",
      scale_to_1 = TRUE,
      outputdir = "outputs/raster/mu_hfp",
      filename = "mu_hfp_all.tif"
    ),
    format = "file"
  ),

  # the year the static prediction surface is taken from, and the year given to
  # records with no date of their own -- every background point. The land cover
  # counterpart is `landcover_prediction_year`, which is 2022 because that is
  # where ESA CCI ends; the Human Footprint runs to 2024, so the prediction
  # surface is not a single instant in time.
  tar_target(
    footprint_prediction_year,
    2024
  ),

  # keeps the name `footprint_5`, so covariate_rast_5_all, subset_covariate_rast
  # and target_covariate_names need no change
  tar_terra_rast(
    footprint_5,
    mu_hfp_model_layer(
      hfp_path = mu_hfp_all,
      year = footprint_prediction_year,
      varname = "footprint",
      project_mask = project_mask_5
    )
    # already written on the project grid by download_mu_hfp(), and already
    # scaled by stack_mu_hfp(), so unlike the geodata layer it replaces there
    # is no aggregate / crop / resample / scale step here
  )

)
