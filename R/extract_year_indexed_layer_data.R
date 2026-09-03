#' Extract values from an annually-indexed raster stack at each record's own year
#'
#' The annual counterpart of `extract_ym_indexed_layer_data()`. Given a stack
#' holding one layer per year and a table of dated point locations, return the
#' value at each location taken from the layer for that record's own year.
#'
#' The layer for a year is found by matching against the stack's own year
#' labels rather than by arithmetic on the layer position, so the stack does
#' not have to be contiguous or in order.
#'
#' Records dated outside the span of the stack are clamped to the first or last
#' available year (the same treatment `extract_ym_indexed_layer_data()` gives
#' its `min_year` / `max_year`); set `clamp = FALSE` to get `NA` for them
#' instead. Records with no date get `na_year`, or `NA` if `na_year` is `NA`.
#'
#' @param dat data frame of locations with columns `x`, `y` and `date`. `date`
#'   may be a Date, or anything `lubridate::year()` accepts, or already a year
#' @param r SpatRaster with one layer per year
#' @param years year of each layer of `r`. Defaults to the stack's time axis,
#'   `terra::time(r)`
#' @param na_year year to use for records whose `date` is `NA`. `NULL` (the
#'   default) uses the most recent year in the stack, mirroring the
#'   `average_last_year()` fallback in `match_offset_data()`; pass `NA` to
#'   leave those records unmatched
#' @param clamp if `TRUE` (default), dates before the first or after the last
#'   year of the stack take the first / last year rather than returning `NA`
#' @return `dat` with three columns added: `year` (the year actually used,
#'   after clamping and `na_year` substitution), `cellidx` and `value`
#' @author geryan
#' @export
extract_year_indexed_layer_data <- function(
    dat,
    r,
    years = NULL,
    na_year = NULL,
    clamp = TRUE
  ) {

  if (is.null(years)) {
    years <- terra::time(r)
  }

  years <- suppressWarnings(as.integer(years))

  if (length(years) != terra::nlyr(r)) {
    stop(
      sprintf(
        "extract_year_indexed_layer_data(): %d years for %d layers",
        length(years),
        terra::nlyr(r)
      ),
      call. = FALSE
    )
  }

  if (anyNA(years)) {
    stop(
      paste(
        "extract_year_indexed_layer_data(): some layers have no year.",
        "Set terra::time(r) or pass `years`."
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(years)) {
    stop(
      "extract_year_indexed_layer_data(): `years` must have one layer per year",
      call. = FALSE
    )
  }

  min_year <- min(years)
  max_year <- max(years)

  if (is.null(na_year)) {
    na_year <- max_year
  }

  # lubridate::year() namespaced because the result is assigned to a column
  # also called `year`, which would mask the function on any later use
  idx_tbl <- dat |>
    dplyr::mutate(
      year = as.integer(lubridate::year(date))
    )

  if (clamp) {
    idx_tbl <- idx_tbl |>
      dplyr::mutate(
        year = dplyr::case_when(
          year < min_year ~ min_year,
          year > max_year ~ max_year,
          .default = year
        )
      )
  }

  idx_tbl <- idx_tbl |>
    dplyr::mutate(
      year = dplyr::if_else(
        is.na(year),
        as.integer(na_year),
        year
      ),
      # match against the stack's own labels, so no assumption that layer i is
      # year min_year + i - 1
      lyridx = match(year, years)
    )

  cellidx <- idx_tbl |>
    dplyr::select(x, y) |>
    as.matrix() |>
    terra::cellFromXY(
      object = r[[1]],
      xy = _
    )

  idx <- dplyr::tibble(
    lyridx = idx_tbl$lyridx,
    cellidx = cellidx
  )

  # because of terra indexing, can't directly pull out r[[idx$lyridx]][idx$cellidx]
  # because that will pull out a stack of layers length(idx$lyridx) and then
  # extract the cell values for all of those layers ffs
  # so instead go layer by layer

  indexed_value <- rep(NA_real_, nrow(idx))

  unique_layers <- sort(unique(idx$lyridx[!is.na(idx$lyridx)]))

  for (layer in unique_layers) {
    # rows in idx that correspond to this layer, and that landed on the grid
    rowidx <- which(idx$lyridx == layer & !is.na(idx$cellidx))

    if (!length(rowidx)) {
      next
    }

    cells <- idx$cellidx[rowidx]

    # extract all at once for this layer
    vals <- terra::extract(
      x = r[[layer]],
      y = cells,
      raw = TRUE
    )

    indexed_value[rowidx] <- as.numeric(vals)
  }

  dplyr::tibble(
    idx_tbl,
    cellidx = cellidx,
    value = indexed_value
  )

}
