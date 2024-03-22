pa_from_va <- function(va_data){
  va_data |>
    rowwise() |> # NB this rowwise is necessary for the below `any` to work by row, but may be slow on a very large dataset
    mutate(
      any_sm_na_count = any(
        !is.na(sampling.method_1) & is.na(n_1),
        !is.na(sampling.method_2) & is.na(n_2),
        !is.na(sampling.method_3) & is.na(n_3),
        !is.na(sampling.method_4) & is.na(n_4)
      ) # this checks if there are any non-empty sampling methods with a NA count
    ) |>
    rename(entered_n_tot = n_tot) |> # renaming becase want to keep for checking but will get double-counted by the sum(c_across(...)) below if left with a name beginning "n_"
    mutate(
      count = case_when(
        any_sm_na_count ~ NA, # assign NA n_tot if there is a non-empty sampling method that is NA
        TRUE ~ sum(c_across(starts_with("n_")), na.rm = TRUE) # otherwise sum up the values ignoring NAs
      )
    ) |>
    select(-entered_n_tot) |>
    group_by(source_ID, binary.absence) |>
    mutate(
      pa = any(binary.absence == TRUE)
    ) |>
    arrange(source_ID, species) |>
    select(source_ID, species, binary.presence, binary.absence, lon, lat, count, pa)
}
