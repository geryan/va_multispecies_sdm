split_sims <- function(sims) {

  ns <- dim(sims[[1]])[1]

  lapply(
    X = seq_len(ns),
    FUN = function(i, sims){

      row_i <- lapply(
        X = sims,
        FUN = function(x){
          x[i, , , drop = FALSE]
        }
      )

      do.call(
        what = greta::initials,
        args = row_i
      )
    },
    sims
  )

}
