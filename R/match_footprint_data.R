#' Append the year-matched Human Footprint to the model data
#'
#' The Human Footprint analogue of `match_landcover_data()`, and built on the
#' same `extract_year_indexed_layer_data()`: for every record in
#' `model_data_spatial`, take the year of that record and read the Human
#' Footprint at its location from *that year's* layer.
#'
#' Input is the `mu_hfp_all` target -- one file, one layer per year, written by
#' `stack_mu_hfp()`, which is also where the single scaling constant for the
#' whole series is applied. The file is opened lazily and only the cells needed
#' are read, so memory does not scale with the length of the series.
#'
#' The extraction is done once per distinct location-and-date and joined back
#' on, as in `match_offset_data()` and `match_landcover_data()`, because the
#' records sit on far fewer distinct locations than there are rows.
#'
#' ## Replacing the static column
#'
#' `get_spatial_values()` has already put a `footprint` column on these records,
#' extracted from the single-year `footprint_5` in `covariate_rast_5_all`.
#' `replace = TRUE` (the default) swaps those static values for each record's
#' own year. Records with no date take `na_year`, which should be the year
#' `footprint_5` itself is built from, so their values do not change.
#'
#' ## Records the series cannot date
#'
#' Handled exactly as in `match_landcover_data()`:
#'
#' - dated **outside the span of the series** are clamped to the nearest
#'   available year. For this dataset that matters: the series starts in 2000
#'   and about a fifth of the dated records are older, so they are given the
#'   Human Footprint of 2000 rather than of their own year. The `year_name`
#'   column records it, so those records can be identified and the sensitivity
#'   tested;
#' - **undated** records, which includes every background point, take
#'   `na_year`, by default the most recent year in the series.
#'
#' @param model_data_spatial the model data, with `longitude`, `latitude` and
#'   `model_date` columns
#' @param hfp_path path to the stacked series, i.e. the `mu_hfp_all` target
#' @param colname name for the column added
#' @param na_year year to use for records with no `model_date`. `NULL` (the
#'   default) uses the most recent year available; `NA` leaves them `NA`
#' @param clamp if `TRUE` (default), records dated outside the span of the
#'   series take the nearest available year rather than `NA`
#' @param add_year whether to also return the year each record was matched to
#' @param year_name what to call that column
#' @param replace if `TRUE` (default), a column already present under `colname`
#'   is dropped first rather than being an error. It must be numeric
#' @return `model_data_spatial` with the footprint column added or replaced
#' @author geryan
#' @export
match_footprint_data <- function(
    model_data_spatial,
    hfp_path,
    colname = "footprint",
    na_year = NULL,
    clamp = TRUE,
    add_year = TRUE,
    year_name = "footprint_year",
    replace = TRUE
  ) {

  hfp_path <- as.character(hfp_path)

  if (length(hfp_path) != 1L) {
    stop(
      sprintf(
        "match_footprint_data(): expected one file, got %d",
        length(hfp_path)
      ),
      call. = FALSE
    )
  }

  if (!file.exists(hfp_path)) {
    stop(
      sprintf("match_footprint_data(): file not found: %s", hfp_path),
      call. = FALSE
    )
  }

  r <- terra::rast(hfp_path)

  new_names <- if (add_year) c(year_name, colname) else colname

  clash <- intersect(names(model_data_spatial), new_names)

  if (length(clash)) {

    if (!replace) {
      stop(
        sprintf(
          "match_footprint_data(): %s already in the data; change `colname` or set `replace = TRUE`",
          paste(clash, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    not_numeric <- clash[
      !vapply(model_data_spatial[clash], is.numeric, logical(1))
    ]

    if (length(not_numeric)) {
      stop(
        sprintf(
          "match_footprint_data(): refusing to replace non-numeric column(s): %s",
          paste(not_numeric, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    model_data_spatial <- model_data_spatial |>
      dplyr::select(-dplyr::all_of(clash))

  }

  key <- model_data_spatial |>
    dplyr::select(
      x = longitude,
      y = latitude,
      date = model_date
    ) |>
    dplyr::distinct()

  extracted <- extract_year_indexed_layer_data(
    dat = key,
    r = r,
    na_year = na_year,
    clamp = clamp
  )

  matched <- key

  matched[[year_name]] <- extracted$year
  matched[[colname]] <- extracted$value

  matched <- matched |>
    dplyr::rename(
      longitude = x,
      latitude = y,
      model_date = date
    )

  if (!add_year) {
    matched <- matched |>
      dplyr::select(-dplyr::all_of(year_name))
  }

  out <- model_data_spatial |>
    dplyr::left_join(
      y = matched,
      by = c("longitude", "latitude", "model_date")
    )

  # the join key is distinct in `matched`, so anything other than a row-for-row
  # result means the key is not what it is assumed to be
  if (nrow(out) != nrow(model_data_spatial)) {
    stop(
      sprintf(
        "match_footprint_data(): join changed the number of rows (%d -> %d)",
        nrow(model_data_spatial),
        nrow(out)
      ),
      call. = FALSE
    )
  }

  out

}
