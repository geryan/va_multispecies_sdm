make_records_table <- function(data_records) {

  table(
    data_records$species,
    data_records$presence,
    data_records$pa
  )

}
