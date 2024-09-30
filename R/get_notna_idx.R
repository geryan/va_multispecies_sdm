get_notna_idx <- function(
    model_data_ragged,
    type = c("pa", "po")
){



  idx <- which(
    !is.na(
      model_data_ragged |>
        select(-lon, -lat, -type) |>
        as.matrix()
    ),
    arr.ind = TRUE
  )

  idxtype <- which(model_data_ragged$type == type)

  idx[idx[,1] %in% idxtype,]
  # this is still wrong
  # only 812 records vs 1133 in
  # data_records |> filter(species %in% target_species, pa == "pa") |> pull(species) |> table()
  # no this is correct, it's after getting rid of the duplicated site records

}
