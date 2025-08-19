generate_unique_letters <- function(n) {
  chars <- c(letters, LETTERS)
  base <- length(chars)
  int_to_seq <- function(x) {
    if (x < base) {
      return(chars[x + 1])
    }
    seq <- character()
    while (x >= 0) {
      seq <- c(chars[(x %% base) + 1], seq)
      x <- (x %/% base) - 1
      if (x < 0) break
    }
    paste(seq, collapse = "")
  }
  sapply(0:(n-1), int_to_seq)
}
