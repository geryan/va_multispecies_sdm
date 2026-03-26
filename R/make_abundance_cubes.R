make_abundance_cubes <- function(
    lambda_no_offset_file,
    offset_stack,
    target_species,
    write_dir
){

  if(is.character(lambda_no_offset_file)){
    lambda_no_offset <- rast(lambda_no_offset_file)
  } else {
    lambda_no_offset <- lambda_no_offset_file
  }

  if(!dir.exists(write_dir)){
    dir.create(write_dir)
  }


  sapply(
    X = target_species,
    FUN = function(x, y, z, w){

      r <- y[[x]]*z


      rnames <- names(z) |>
        sub(
          pattern = "offset",
          replacement = x,
          x = _
        )

      names(r) <- rnames

      writeRaster(
        x = r,
        filename = sprintf(
          "%s/%s_abundance.tif",
          w,
          x
        ),
        filetype = "GTiff",
        overwrite = TRUE
      )

      x

    },
    y = lambda_no_offset,
    z = offset_stack,
    w = write_dir
  )


}
