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
#' on, exactly as `match_offset_data()` does, because the 53 000 records sit on
#' only about 3 500 distinct locations.
#'
#' ## Column names
#'
#' The new columns are prefixed, `esa_` by default. This is not cosmetic:
#' `model_data_spatial` already carries WorldCover covariates named
#' `grassland`, `water` and `wetland`, which are also ESA class names, and
#' `trees` / `mangroves` / `built` / `cropland`, which are one character or one
#' synonym away from `tree` / `mangrove` / `urban` / `crop_other`. Without a
#' prefix the first three collide outright and the rest are an invitation to
#' pick the wrong covariate. The function errors rather than overwrite if any
#' name it is about to add is already present.
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
#'   `esa_landcover_classes` target. Only used as a check: the class of each
#'   file is read from its own layer names, and a mismatch is an error
#' @param na_year year to use for records with no `model_date`. `NULL` (the
#'   default) uses the most recent year available; `NA` leaves them `NA`
#' @param clamp if `TRUE` (default), records dated outside 1992-2022 take the
#'   nearest available year rather than `NA`
#' @param prefix prepended to each land cover class to make the column name
#' @param add_year whether to also return the year each record was matched to,
#'   as `<prefix>year`
#' @return `model_data_spatial` with one column added per land cover class
#' @author geryan
#' @export
match_landcover_data <- function(
    model_data_spatial,
    landcover_paths,
    landcover_classes = NULL,
    na_year = NULL,
    clamp = TRUE,
    prefix = "esa_",
    add_year = TRUE
  ) {

  landcover_paths <- as.character(landcover_paths)

  missing_paths <- landcover_paths[!file.exists(landcover_paths)]

  if (length(missing_paths)) {
    stop(
      sprintf(
        "match_landcover_data(): file not found: %s",
        paste(missing_paths, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  rasters <- lapply(landcover_paths, terra::rast)

  # the class of each file comes from its own layer names (`<class>_<year>`),
  # not from the position of the file in `landcover_paths`, so a change in
  # branch order cannot silently relabel the columns
  classes <- vapply(
    rasters,
    function(r) {
      cl <- unique(sub("_[0-9]{4}$", "", names(r)))

      if (length(cl) != 1L) {
        stop(
          sprintf(
            "match_landcover_data(): %s holds more than one class (%s)",
            terra::sources(r)[1],
            paste(cl, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      cl
    },
    character(1)
  )

  if (anyDuplicated(classes)) {
    stop(
      sprintf(
        "match_landcover_data(): duplicated land cover class in `landcover_paths`: %s",
        paste(unique(classes[duplicated(classes)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(landcover_classes)) {
    expected <- as.character(landcover_classes)

    if (!setequal(classes, expected)) {
      stop(
        sprintf(
          "match_landcover_data(): files hold classes %s but `landcover_classes` is %s",
          paste(sort(classes), collapse = ", "),
          paste(sort(expected), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # all classes are written on the same grid, so the cell lookup done inside
  # extract_year_indexed_layer_data() is the same for every one of them
  for (i in seq_along(rasters)[-1]) {
    if (!terra::compareGeom(rasters[[1]], rasters[[i]], stopOnError = FALSE)) {
      stop(
        sprintf(
          "match_landcover_data(): %s is not on the same grid as %s",
          basename(landcover_paths[i]),
          basename(landcover_paths[1])
        ),
        call. = FALSE
      )
    }
  }

  new_names <- paste0(prefix, classes)

  clash <- intersect(names(model_data_spatial), new_names)

  if (length(clash)) {
    stop(
      sprintf(
        "match_landcover_data(): %s already in the data; change `prefix`",
        paste(clash, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # ~53 000 records sit on ~5 000 distinct location-dates, so extract once per
  # distinct key and join the result back on
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
      matched[[paste0(prefix, "year")]] <- extracted$year
    } else if (!identical(matched[[paste0(prefix, "year")]], extracted$year)) {
      # every class stack spans the same years, so they must all resolve each
      # record to the same one; if not, the columns are not comparable
      stop(
        sprintf(
          "match_landcover_data(): %s resolved records to different years than %s",
          basename(landcover_paths[i]),
          basename(landcover_paths[1])
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
        -dplyr::all_of(paste0(prefix, "year"))
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
