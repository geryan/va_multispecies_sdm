#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_data_spatial
#' @param offsets_5
#' @return
#' @author geryan
#' @export
match_offset_data <- function(model_data_spatial, offsets_5) {

  dat <- model_data_spatial |>
    select(x = longitude, y = latitude, date = model_date) |>
    distinct() |>
    filter(!is.na(date))

  date_matched_data <- extract_ym_indexed_layer_data(
    dat = dat,
    r = offsets_5,
    min_year = 2000
  )

# this function still needs to match the non-date matched data to some
# averaged layer for most recent year
# then match these offset values back to larger model data spatial tibble
# to return to targets pipeline
}



