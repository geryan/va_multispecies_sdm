prepare_multi_layer <- function(
    data_dir#,
    #layer_prefix,
    #file_id_prefix,
    #file_id_suffix
){


  r <- list.files(
    path = data_dir,
    full.names = TRUE,
    pattern = "\\.tif"
  ) %>%
    sapply(
      FUN = rast
    ) %>%
    rast

  # r_names <- names(r) %>%
  #   sub(
  #     pattern = file_id_prefix,
  #     replacement = "",
  #     x = .
  #   ) %>%
  #   sub(
  #     pattern = file_id_suffix,
  #     replacement = "",
  #     x = .
  #   ) %>%
  #   sprintf(
  #     "%s_%s",
  #     layer_prefix,
  #     .
  #   )
  #
  # names(r) <- r_names

  r

}
