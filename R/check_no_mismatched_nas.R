check_no_mismatched_nas <- function(proj_mask, ...){

  r <- c(...)

  zz <- sapp(
    x = r,
    fun = function(x,y){
      is.na(x) == is.na(y)
    },
    y = proj_mask
  )

  # zz[[1:4]]
  # zz[[5:8]]
  # plot(zz)

  which(!values(zz))

}
