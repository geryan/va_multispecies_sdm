scale_rast_to_1 <- function(x, reverse = FALSE, filename = NULL, overwrite = TRUE){
  vals <- terra::values(x)

  nvs <- apply(
    vals,
    MARGIN = 2,
    FUN = function(x){
      x/max(x, na.rm = TRUE)
    }
  )

  r <- x

  if(reverse){
    nvs <- 1 - nvs
  }

  r[] <- nvs


  if(is.null(filename)){
    return(r)
  }

  writereadrast(
    r,
    filename,
    overwrite
  )
}
