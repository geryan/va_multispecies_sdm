add_pa_points_list <- function(
    plot_list,
    model_data_spatial,
    colp = "deeppink",
    cola = "grey80"
){

  spp <- names(plot_list)

  pl <- sapply(
    X = spp,
    FUN = function(
    x,
    plot_list,
    model_data_spatial
    ){

      p <- plot_list[[x]]

      d <- model_data_spatial |>
        filter(
          species == x,
          data_type != presence
        ) |>
        mutate(
          detected = case_when(
            presence == 1 ~ "Detected",
            presence == 0 ~ "Undetected"
          )
        )

      p2 <- add_pa_points(
        p = p,
        point_data = d,
        colp = colp,
        cola = cola
      )

      p2

    },
    plot_list,
    model_data_spatial
  )

  pl

}
