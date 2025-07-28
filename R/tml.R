tml <- function(x){

  tar_make({{x}})
  tar_load(
    names = {{x}},
    envir = .GlobalEnv
  )

}
