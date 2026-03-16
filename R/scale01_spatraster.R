scale01_spatraster <- function(x){

  mm <- minmax(x)

  xmin <- mm[1]
  xmax <- mm[2]

  (x - xmin)/(xmax - xmin)

}
