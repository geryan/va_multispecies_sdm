count_counts <- function(
    x,
    xmax = NULL
  ){

  if(is.null(xmax)){
    xmax <- max(x)
  }

  tibble(value = x) |>
    count(value) |>
    full_join(
      y = tibble(value = 0:xmax),
      by = "value"
    ) |>
    arrange(value) |>
    mutate(count = ifelse(is.na(n), 0, n)) |>
    select(-n)

}
