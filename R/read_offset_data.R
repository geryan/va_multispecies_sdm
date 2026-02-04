read_offset_data <- function(
    odir = "/Users/gryan/Dropbox/vector_rasters/"
  ){

   onames <- list.files(path = odir) |>
    sub(
      pattern = "an_gambiae\\.",
      replacement = "offset_",
      x = _
    ) |>
    sub(
      pattern = ".adult.tif",
      replacement = "",
      x = _
    )

   offsets_raw <- list.files(odir, full.names = TRUE) |>
     rast()

   names(offsets_raw) <- onames

   offsets_raw

}
