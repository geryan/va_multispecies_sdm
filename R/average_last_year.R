average_last_year <- function(r){

  nl <- nlyr(r)

  mean(r[[(nl-11): nl]])

}
