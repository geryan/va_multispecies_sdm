extract_ym_indexed_layer_data <- function(
    dat,
    r,
    min_year,
    max_year = NULL
) {

  idx_tbl <- dat |>
    mutate(
      month = month(date),
      year = year(date),
      year = ifelse(year < min_year, min_year, year)
    )

  if(!is.null(max_year)){
    idx_tbl <- idx_tbl |>
      mutate(
        year = ifelse(year > max_year, max_year, year)
      )
  }

  idx_tbl <- idx_tbl |>
    mutate(
      lyridx = 12 * (year - min_year) + month
    )


  cellidx <- idx_tbl |>
    select(x, y) |>
    as.matrix() |>
    cellFromXY(
      object = offsets_5[[1]],
      xy = _
    )

  idx <- tibble(lyridx = idx_tbl$lyridx, cellidx = cellidx)

  # because of terra indexing, can't directly pull out
  # offsets[[idx$lyridx]][idx$cellidx]
  # because that will pull out a stack of layers length(idx$lyridx) and then
  # extract the cell values for all of those layers ffs
  # so instead go layer by layer

  # empty object for loop to fill
  indexed_value <- rep(NA_real_, nrow(idx))

  unique_layers <- unique(idx$lyridx)

  for (layer in unique_layers) {
    # rows in idx that correspond to this layer
    rowidx <- which(idx$lyridx == layer)

    # cells for those rows
    cells <- idx$cellidx[rowidx]

    # extract all at once for this layer
    vals <- terra::extract(
      x = r[[layer]],
      y = cells,
      raw = TRUE
    )
    indexed_value[rowidx] <- as.numeric(vals)
  }

  tibble(
    idx_tbl, cellidx = cellidx, value = indexed_value
  )

}
