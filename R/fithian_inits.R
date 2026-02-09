fithian_inits <- function(dat, target_species, n_pixel){

  ns <- dat |>
    select(species, data_type, presence) |>
    filter(presence != 0) |>
    mutate(
      species_id = match(
        x = species,
        table = target_species
      ),
      data_type = ifelse(
        data_type == "count",
        "pa",
        data_type
      )
    ) |>
    group_by(species_id, data_type) |>
    summarise(n = n()) |>
    arrange(data_type)

  npa_sp <- ns |>
    filter(data_type == "pa") |>
    pull(n)

  npo <- ns |>
    filter(data_type == "po") |>
    pull(n)

  npa_sites <- dat |>
    filter(presence != 0, data_type != "po") |>
    select(longitude, latitude) |>
    distinct() |>
    nrow()


  start_alphas <- log((1 + npa_sp)/npa_sites)

  start_gammas <- log1p(npo) - start_alphas - log(n_pixel)

  list(
    alpha = start_alphas,
    beta = 0,
    gamma = start_gammas,
    delta = 0
  )

}
