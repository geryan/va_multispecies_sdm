get_expert_maps <- function(sp){

  v <- list.files(
    path = "data/sinka_expert_maps/",
    pattern = "*.shp",
    full.names = TRUE
  ) |>
    sapply(vect)

  nms <- list.files(
    path = "data/sinka_expert_maps/",
    pattern = "*.shp"
  ) |>
    sub(
      pattern = "\\.shp",
      replacement = "",
      x = _
    )

  nms <- ifelse(
    nms == "gambiae_ss",
    "gambiae",
    nms
  )

  v[nms %in% sp]

  v <- mapply(
    FUN = function(x, y){
      x$species <- y
      x
    },
    v[nms %in% sp],
    sp
  ) |>
    vect()

  v

}



