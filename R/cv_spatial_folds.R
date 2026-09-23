# Assign spatial cross-validation folds.
#
# THE BLOCKING UNIT IS THE BACKGROUND VORONOI CELL, NOT THE HEXAGON.
#
# The presence-only / background likelihood is a Berman-Turner quadrature: each of the
# ~250 background points carries the area of its Voronoi cell as `weight`, and those
# weights tile the masked continent exactly (they sum to 29,553,040 km2). Holding out a
# region means dropping both the presences in it and the quadrature that integrates over
# it, and those two have to be the SAME region -- an inhomogeneous Poisson process
# restricted to a sub-domain has a likelihood that factorises across disjoint regions, but
# only if the split is clean.
#
# Cutting hexagons over the data coordinates does not give a clean split: presences would
# leave by hexagon while quadrature leaves by which side of the boundary a centroid falls.
# At a mean Voronoi cell width of ~344 km the mismatched area is comparable to the block
# itself, and it biases in both directions at once -- area whose presences were removed but
# whose integral was kept pulls the intensity down, and the reverse pulls it up.
#
# So blocks are cut over the background centroids, and every data coordinate takes the fold
# of its nearest centroid, i.e. of the Voronoi cell it sits in. The removed domain is then
# exactly the union of the removed cells. It also makes fold assignment a function of the
# coordinate alone, which is what lets the production fit function be called unmodified on
# a row subset -- see cv_designmat().
#
# Returns paths: c(<cv_folds.csv>, <cv_blocks.gpkg>, <cv_autocor.csv>).
cv_spatial_folds <- function(
    model_data_spatial,
    bg_kmeans_df,
    covariate_rast,
    target_covariate_names,
    k = 5,
    iteration = 500,
    max_range_m = 4e6,
    num_sample = 5000,
    seed = 20260909,
    output_dir = "outputs/cv"
){

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ############
  # block size, from the covariates' own autocorrelation
  ############

  # cv_spatial_autocor() takes no seed of its own and subsamples pixels, so seed here
  set.seed(seed)

  autocor <- blockCV::cv_spatial_autocor(
    r = covariate_rast[[target_covariate_names]],
    num_sample = num_sample,
    plot = FALSE,
    progress = FALSE
  )

  # Do NOT take autocor$range at face value. It is the median across layers, and several
  # of these variograms never reach a sill, so their fitted range runs away: on the
  # 2026-09 covariate set `tree` came back at 287,142 km and `water` at 126,624 km, both
  # larger than the Earth's circumference, while `urban` fitted a sill of exactly 0. The
  # median then lands on whichever layer happens to sit in the middle of a contaminated
  # set. Keep only the layers that actually fitted.
  range_table <- autocor$range_table |>
    as_tibble() |>
    mutate(
      keep = range < max_range_m & sill > 0
    )

  if (!any(range_table$keep)) {
    stop(
      "cv_spatial_folds(): no covariate variogram fitted a usable range",
      call. = FALSE
    )
  }

  size <- range_table |>
    filter(keep) |>
    pull(range) |>
    median()

  readr::write_csv(
    range_table |>
      mutate(
        chosen_size_m = size,
        max_range_m = max_range_m
      ),
    file.path(output_dir, "cv_autocor.csv")
  )

  ############
  # sites, and the Voronoi cell each one falls in
  ############

  sites <- model_data_spatial |>
    group_by(latitude, longitude) |>
    summarise(
      n_count = sum(data_type == "count"),
      n_pa = sum(data_type == "pa"),
      n_po = sum(data_type == "po"),
      n_bg = sum(data_type == "bg"),
      .groups = "drop"
    ) |>
    mutate(
      has_count = as.integer(n_count > 0),
      coord_key = cv_coord_key(latitude, longitude)
    )

  sites_sf <- sf::st_as_sf(
    sites,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  bg_sf <- bg_kmeans_df |>
    select(latitude, longitude, weight) |>
    mutate(
      bg_cluster = row_number()
    ) |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )

  # which Voronoi cell each data coordinate sits in
  sites$bg_cluster <- sf::st_nearest_feature(
    sites_sf,
    bg_sf
  )

  # a cell is "a count cell" if any count site falls in it -- this is what blockCV
  # balances on, so that the thing being scored is spread across folds
  cell_has_count <- sites |>
    group_by(bg_cluster) |>
    summarise(
      has_count = as.integer(any(has_count == 1)),
      .groups = "drop"
    )

  bg_sf <- bg_sf |>
    left_join(
      cell_has_count,
      by = "bg_cluster"
    ) |>
    mutate(
      has_count = tidyr::replace_na(has_count, 0L)
    )

  ############
  # the blocks
  ############

  # blockCV balances the `column` it is given, which is cell-level count PRESENCE; the
  # number of count RECORDS per fold is not what it optimises and cannot be, since counts
  # are very unevenly spread over the 786 count sites. More random restarts do help --
  # median max/min record ratio across four seeds was 5.0x at 50 iterations, 3.6x at 200
  # and 2.6x at 500 -- and 500 restarts over ~250 points costs nothing, so that is the
  # default. The residual imbalance is a property of the data: report per-fold n and use
  # the across-fold SE, do not tune the folds until the metric looks tidy.
  set.seed(seed)

  cv <- blockCV::cv_spatial(
    x = bg_sf,
    column = "has_count",
    k = k,
    size = size,
    hexagon = TRUE,
    selection = "random",
    iteration = iteration,
    seed = seed,
    plot = FALSE,
    report = FALSE,
    progress = FALSE
  )

  bg_sf$fold <- cv$folds_ids

  sites <- sites |>
    left_join(
      bg_sf |>
        sf::st_drop_geometry() |>
        select(bg_cluster, fold),
      by = "bg_cluster"
    )

  ############
  # assertions -- leakage here would invalidate everything downstream silently
  ############

  if (anyNA(sites$fold)) {
    stop(
      "cv_spatial_folds(): some coordinates were not assigned a fold",
      call. = FALSE
    )
  }

  if (nrow(distinct(sites, latitude, longitude)) != nrow(sites)) {
    stop(
      "cv_spatial_folds(): a coordinate appears more than once, so it could be split across folds",
      call. = FALSE
    )
  }

  if (anyDuplicated(sites$coord_key) > 0) {
    stop(
      "cv_spatial_folds(): two distinct coordinates collide at the 9 dp join key",
      call. = FALSE
    )
  }

  if (length(unique(sites$fold)) != k) {
    stop(
      sprintf(
        "cv_spatial_folds(): expected %i folds, got %i",
        k,
        length(unique(sites$fold))
      ),
      call. = FALSE
    )
  }

  empty_count_folds <- sites |>
    group_by(fold) |>
    summarise(
      n_count = sum(n_count),
      .groups = "drop"
    ) |>
    filter(n_count == 0)

  if (nrow(empty_count_folds) > 0) {
    stop(
      sprintf(
        "cv_spatial_folds(): fold(s) %s hold no count records, so there is nothing to score",
        paste(empty_count_folds$fold, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  ############
  # write
  ############

  folds_file <- file.path(output_dir, "cv_folds.csv")
  blocks_file <- file.path(output_dir, "cv_blocks.gpkg")

  readr::write_csv(
    sites,
    folds_file
  )

  sf::st_write(
    cv$blocks,
    blocks_file,
    delete_dsn = TRUE,
    quiet = TRUE
  )

  c(
    folds_file,
    blocks_file,
    file.path(output_dir, "cv_autocor.csv")
  )

}
