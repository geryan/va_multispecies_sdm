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

  dat_date <- model_data_spatial |>
    select(x = longitude, y = latitude, date = model_date) |>
    distinct() |>
    filter(!is.na(date))

  date_matched_data <- extract_ym_indexed_layer_data(
    dat = dat_date,
    r = offsets_5,
    min_year = 2000
  )

  dat_no_date <- model_data_spatial |>
    filter(is.na(model_date)) |>
    select(x = longitude, y = latitude) |>
    distinct()

  offset_last_avg_5 <- average_last_year(offsets_5)

  no_date_matched_data <- terra::extract(
    offset_last_avg_5,
    dat_no_date
  )

  matched_data <- rbind(
    date_matched_data |>
      select(
        longitude = x,
        latitude = y,
        model_date = date,
        offset = value
      ),
    cbind(
      dat_no_date |>
        rename(
          longitude = x,
          latitude = y,
        ),
      model_date = NA,
      offset = no_date_matched_data$mean
    )
  )

  model_data_spatial |>
    left_join(
      y = matched_data,
      by = c("longitude", "latitude", "model_date")
    )

}



