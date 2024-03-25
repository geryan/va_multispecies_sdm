filter_few <- function(x){
  x |>
    group_by(species, pa) |>
    mutate(
      n_pa = n()
    ) |>
    group_by(
      species
    ) |>
    mutate(
      n_records = n()
    ) |>
    ungroup() |>
    filter(n_records > 39)

}
