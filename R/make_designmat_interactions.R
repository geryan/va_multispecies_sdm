#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param x_bioregion
#' @return
#' @author Nick Golding
#' @export
make_designmat_interactions <- function(a, b) {
# given two design matrices of covariates 'a' and 'b' (with the same number of
# rows, but different numbers of columns), return a design matrix for all
# interactions between the columns of the two matrices

  n_a <- ncol(a)
  n_b <- ncol(b)

  index_a <- rep(seq_len(n_a), each = n_b)
  index_b <- rep(seq_len(n_b), n_a)

  a[, index_a] * b[, index_b]

}
