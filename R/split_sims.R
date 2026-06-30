split_sims <- function(sims) {

  ns <- dim(sims[[1]])[1]

  lapply(
    X = seq_len(ns),
    FUN = function(i, sims){

      row_i <- lapply(
        X = sims,
        FUN = function(x){
          x_i <- x[i, , , drop = FALSE]

          array(
            data = x_i,
            dim = dim(x = x)[-1],
            dimnames = dimnames(x = x)[-1]
          )

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
