combine_raw_data <- function(
  old_raw_data,
  new_raw_data
){

  bind_rows(
    old_raw_data |>
      mutate(
        bio_data = as.character(bio_data),
        source_id = as.character(source_id)
      ),
    new_raw_data
  )

}
