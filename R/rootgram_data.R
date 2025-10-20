rootgram_data <- function(
    y,
    yrep,
    xlen = c("y", "yrep")
){

  y_tab <- count_counts(x = y)

  yrep_matrix <- count_counts_matrix(x = yrep)

  if(xlen == "y"){

    y_data <- y_tab$count

    ncol_yrep <- length(y_data)

    yrep_data <- yrep_matrix[, 1:ncol_yrep]
  } else if(xlen == "yrep"){

  }

  list(
    y = y_data,
    yrep = yrep_data
  )

}
