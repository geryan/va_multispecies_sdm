#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bg_kmeans_list_spatial
#' @return
#' @author geryan
#' @export
frame_bg_kmeans <- function(bg_kmeans_list_spatial) {

  # bg_kmeans_list$x |>
  #   as_tibble() |>
  #   mutate(
  #     # generate a unique sequence of letters for longitude and latitude to be used as indices; nb these are not valid lat/long
  #     longitude = 1:length(bg_kmeans_list$weight),
  #     #latitude = generate_unique_letters(length(bg_kmeans_list$weight)),
  #     latitude = 1:length(bg_kmeans_list$weight),
  #     data_type = "bg",
  #     n = 0
  #   ) |>
  #   bind_cols(
  #     weight = bg_kmeans_list$weight |>
  #       as.numeric()
  #   )
  #
  tibble(
    longitude = bg_kmeans_list_spatial$coords[,"x"],
    latitude = bg_kmeans_list_spatial$coords[,"y"],
    bg_kmeans_list_spatial$x,
    weight = bg_kmeans_list_spatial$weight
  )

}
