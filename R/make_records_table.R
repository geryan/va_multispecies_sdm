make_records_table <- function(pa_data_records) {

  table(
    pa_data_records$species,
    pa_data_records$presence,
    pa_data_records$pa
  )

}
