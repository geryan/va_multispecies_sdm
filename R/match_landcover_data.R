#' Append year-matched ESA land cover proportions to the model data
#'
#' The land cover analogue of `match_offset_data()`: for every record in
#' `model_data_spatial`, take the year of that record and read the land cover
#' proportion at its location from that year's layer, one column per land cover
#' class.
#'
#' Input is the `esa_landcover_proportion` target, which is one file per class
#' holding one layer per year (1992-2022), written by
#' `stack_esa_landcover_proportion()`. Each file is opened lazily and only the
#' cells that are needed are read, so memory does not scale with the number of
#' years or the size of the continent.
#'
#' The extraction is done once per distinct location-and-date, then joined back
#' on, exactly as `match_offset_data()` does, because the records sit on far
#' fewer distinct locations than there are rows.
#'
#' ## Which classes
#'
#' By default every class among `landcover_paths` is added. Pass `keep_classes`
#' to add only some of them -- which is what the model wants, because the full
#' set of proportions sums to exactly 1 and so is perfectly collinear with an
#' intercept. Dropping a class or two breaks that closure; the classes that are
#' dropped stay in the denominator, so the ones that remain are still the
#' fraction of the cell they cover.
#'
#' ## Column names
#'
#' The new columns can be prefixed. With `prefix = ""` the columns are named
#' for the classes themselves, which is what the model uses, and which is only
#' safe once the WorldCover covariates are gone: they include `grassland`,
#' `water` and `wetland`, which are also ESA class names, and `trees` /
#' `mangroves` / `built` / `cropland`, which are one character or one synonym
#' away from `tree` / `mangrove` / `urban` / `crop_other`.
#'
#' The function errors rather than overwrite if a name it is about to add is
#' already present, unless `replace = TRUE`. `replace` exists for one specific
#' situation: the static single-year land cover layers in `covariate_rast_5_all`
#' (see `esa_landcover_model_layers()`) are extracted at record locations by
#' `get_spatial_values()` under these same names, and this function then
#' replaces those static values with each record's own year. Background points,
#' which have no date, take `na_year` and so keep the values they already had.
#'
#' ## Records the stack cannot date
#'
#' Two groups of records do not have a land cover year of their own, and both
#' are handled the way `match_offset_data()` handles them:
#'
#' - dated **before 1992 or after 2022** are clamped to the nearest available
#'   year, matching the `min_year` / `max_year` clamp in
#'   `extract_ym_indexed_layer_data()`;
#' - **undated** records, which includes every background point, take
#'   `na_year`, by default the most recent year in the stack. This is the
#'   annual counterpart of `match_offset_data()` falling back to
#'   `average_last_year()`.
#'
#' The year actually used is returned in a column so this is auditable rather
#' than silent; set `add_year = FALSE` to drop it.
#'
#' @param model_data_spatial the model data, with `longitude`, `latitude` and
#'   `model_date` columns
#' @param landcover_paths character vector of paths to the per-class stacks,
#'   i.e. the `esa_landcover_proportion` target
#' @param landcover_classes optional class names, i.e. the
#'   `esa_landcover_classes` target. Only used as a check on the files supplied:
#'   the class of each file is read from its own layer names, and a mismatch is
#'   an error
#' @param keep_classes optional classes to actually add, a subset of those in
#'   `landcover_paths`. `NULL` (the default) adds all of them. Columns appear in
#'   the order given
#' @param na_year year to use for records with no `model_date`. `NULL` (the
#'   default) uses the most recent year available; `NA` leaves them `NA`
#' @param clamp if `TRUE` (default), records dated outside 1992-2022 take the
#'   nearest available year rather than `NA`
#' @param prefix prepended to each land cover class to make the column name
#' @param add_year whether to also return the year each record was matched to
#' @param year_name what to call that column. `NULL` (the default) makes it
#'   `<prefix>year`, which with `prefix = ""` is a bare `year` -- generic enough
#'   to be worth naming explicitly
#' @param replace if `TRUE`, columns already present under the names being added
#'   are dropped first rather than being an error. They must all be numeric
#' @return `model_data_spatial` with one column added per land cover class
#' @author geryan
#' @export
match_landcover_data <- function(
    model_data_spatial,
    landcover_paths,
    landcover_classes = NULL,
    keep_classes = NULL,
    na_year = NULL,
    clamp = TRUE,
    prefix = "esa_",
    add_year = TRUE,
    year_name = NULL,
    replace = FALSE
  ) {

  if (is.null(year_name)) {
    year_name <- paste0(prefix, "year")
  }

  # class of each file comes from its own layer names, not from its position in
  # `landcover_paths`, so a change in branch order cannot silently relabel the
  # columns
  all_paths <- esa_landcover_class_paths(landcover_paths)

  if (!is.null(landcover_classes)) {
    expected <- as.character(landcover_classes)

    if (!setequal(names(all_paths), expected)) {
      stop(
        sprintf(
          "match_landcover_data(): files hold classes %s but `landcover_classes` is %s",
          paste(sort(names(all_paths)), collapse = ", "),
          paste(sort(expected), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  paths <- esa_landcover_class_paths(landcover_paths, keep_classes)

  classes <- names(paths)

  rasters <- lapply(unname(paths), terra::rast)

  # all classes are written on the same grid, so the cell lookup done inside
  # extract_year_indexed_layer_data() is the same for every one of them
  for (i in seq_along(rasters)[-1]) {
    if (!terra::compareGeom(rasters[[1]], rasters[[i]], stopOnError = FALSE)) {
      stop(
        sprintf(
          "match_landcover_data(): %s is not on the same grid as %s",
          basename(paths[i]),
          basename(paths[1])
        ),
        call. = FALSE
      )
    }
  }

  new_names <- paste0(prefix, classes)

  if (add_year) {
    new_names_all <- c(year_name, new_names)
  } else {
    new_names_all <- new_names
  }

  clash <- intersect(names(model_data_spatial), new_names_all)

  if (length(clash)) {

    if (!replace) {
      stop(
        sprintf(
          "match_landcover_data(): %s already in the data; change `prefix` or set `replace = TRUE`",
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
          "match_landcover_data(): refusing to replace non-numeric column(s): %s",
          paste(not_numeric, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    model_data_spatial <- model_data_spatial |>
      dplyr::select(-dplyr::all_of(clash))

  }

  # the records sit on far fewer distinct location-dates than there are rows, so
  # extract once per distinct key and join the result back on
  key <- model_data_spatial |>
    dplyr::select(
      x = longitude,
      y = latitude,
      date = model_date
    ) |>
    dplyr::distinct()

  matched <- key

  for (i in seq_along(rasters)) {
    extracted <- extract_year_indexed_layer_data(
      dat = key,
      r = rasters[[i]],
      na_year = na_year,
      clamp = clamp
    )

    if (i == 1L) {
      matched[[year_name]] <- extracted$year
    } else if (!identical(matched[[year_name]], extracted$year)) {
      # every class stack spans the same years, so they must all resolve each
      # record to the same one; if not, the columns are not comparable
      stop(
        sprintf(
          "match_landcover_data(): %s resolved records to different years than %s",
          basename(paths[i]),
          basename(paths[1])
        ),
        call. = FALSE
      )
    }

    matched[[new_names[i]]] <- extracted$value
  }

  matched <- matched |>
    dplyr::rename(
      longitude = x,
      latitude = y,
      model_date = date
    )

  if (!add_year) {
    matched <- matched |>
      dplyr::select(
        -dplyr::all_of(year_name)
      )
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
        "match_landcover_data(): join changed the number of rows (%d -> %d)",
        nrow(model_data_spatial),
        nrow(out)
      ),
      call. = FALSE
    )
  }

  out

}
