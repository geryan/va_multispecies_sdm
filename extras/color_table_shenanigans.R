r <- rast(system.file("extdata/cyl_era.tif", package = "tidyterra"))

plot(r)

coltab_pal <- get_coltab_pal(r)



coltab_pal
#> Precambric-Paleozoic            Paleozoic   Paleozoic-Mesozoic
#>            "#FFBFE9"            "#9ADDCF"            "#D79EBD"
#>             Mesozoic    Mesozoic-Cenozoic             Cenozoic
#>            "#A4FF74"            "#FFD480"            "#FFFFBF"
#>         Undetermined
#>            "#FFFFFF"

# \donttest{
# With ggplot2 + tidyterra
library(ggplot2)

gg <- ggplot() +
  geom_spatraster(data = r)


# Default plot
gg

x <- colz
x <- r
{
  if (!inherits(x, "SpatRaster")) {
    cli::cli_alert_info(paste("{.fun tidyterra::get_coltab_pal} only works with",
                              "{.cls SpatRaster} objects, not {.cls {class(x)}}.",
                              "Returning {.field NULL}"))
    return(NULL)
  }
  if (!any(terra::has.colors(x))) {
    cli::cli_alert_info("{.arg x} does not have a color table. Returning {.field NULL}",
    )
    return(NULL)
  }
  iter <- seq_len(terra::nlyr(x))[!terra::has.colors(x)]
  if (length(iter) > 0 && min(iter) > 0) {
    for (h in iter) {
      tmpr <- terra::subset(x, h)
      vals <- as.factor(pull(tmpr))
      terra::values(tmpr) <- vals
      df <- as_tibble(terra::cats(tmpr)[[1]])
      coltb <- data.frame(t(col2rgb(terrain.colors(nrow(df),
                                                   rev = TRUE), alpha = TRUE)))
      coltbend <- cbind(df[, 1], coltb)
      terra::coltab(tmpr) <- coltbend
      x[[h]] <- tmpr
    }
  }
  lcats <- terra::cats(x)
  actcats <- lapply(seq_len(terra::nlyr(x)), function(p) {
    terra::activeCat(x[[p]])
  })
  lcats <- lapply(seq_len(terra::nlyr(x)), function(i) {
    i_df <- lcats[[i]]
    if (is.null(i_df)) {
      return(NULL)
    }
    actcat <- unlist(actcats[i])
    df <- i_df[, c(1, actcat + 1)]
    names(df) <- c("id", "label")
    df
  })
  nm <- paste0(names(x), "_", seq_len(terra::nlyr(x)))
  names(lcats) <- nm
  cats_end <- dplyr::bind_rows(lcats, .id = "layer")
  cols_alpha_l <- terra::coltab(x)
  cols_alpha_l <- lapply(cols_alpha_l, function(j) {
    if (is.null(j)) {
      return(NULL)
    }
    df <- j[, seq_len(5)]
    names(df) <- c("id", "r", "g", "b", "a")
    df
  })
  names(cols_alpha_l) <- nm
  cols_end <- dplyr::bind_rows(cols_alpha_l, .id = "layer")
  tojoin <- intersect(names(cats_end), names(cols_end))
  finaltab <- dplyr::left_join(cats_end, cols_end, by = tojoin)
  colfields <- finaltab[, c("r", "g", "b", "a")]
  namedpal <- rgb(tidyr::drop_na(colfields), maxColorValue = 255)
  nms <- unique(finaltab[["label"]])
  if (!identical(length(namedpal), length(nms))) {
    namedpal <- c(namedpal, terrain.colors(length(nms)))
    namedpal <- namedpal[seq_len(length(nms))]
  }
  names(namedpal) <- nms
  namedpal
}
