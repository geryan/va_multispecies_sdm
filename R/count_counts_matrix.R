count_counts_matrix <- function(
    x,
    xmax = NULL
){

  if(is.null(xmax)){
    xmax <- max(x)
  }

  apply(
    X =  x,
    MARGIN = 1,
    FUN = count_counts,
    xmax = xmax
  ) |>
    lapply(
      FUN = function(x){x$count}
    ) |>
    do.call(
      what = rbind,
      args = _
    )


}
