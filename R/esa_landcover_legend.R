#' LCCS class legend, read out of the NetCDF rather than hard coded
#'
#' The CCI files carry the legend on the `lccs_class` variable as CF
#' `flag_values` / `flag_meanings` attributes, which GDAL surfaces as
#' metadata. Reading it from the file keeps the lookup honest -- the legend
#' gained sub-classes between v2_0_7cds and v2_1_1, and a transcribed table
#' would silently drift.
#'
#' Returns the same shape as landcover_lookup / settlement_lookup:
#' a data.frame(value, category) suitable for `levels<-`.
#'
#' Returns NULL (with a warning) if the attributes cannot be read, so that a
#' legend hiccup never takes down a long unattended run -- the stack is simply
#' written without categorical levels. On the real CDS files it does read:
#' 1992 yields 38 classes, 0 = no_data through 220 = snow_and_ice.
#'
#' NB depends on esa_landcover_nc(), which lives in R/prepare_esa_landcover.R.
#' Move the two files together.
esa_landcover_legend <- function(archive, varname = "lccs_class"){

  scratch <- file.path(tempdir(), "esa_lc_legend")
  dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  legend <- tryCatch(
    {
      nc <- esa_landcover_nc(archive, scratch)

      md <- terra::describe(
        sprintf('NETCDF:"%s":%s', nc, varname),
        meta = TRUE
      )

      values <- esa_split_attr(md, paste0(varname, "#flag_values"))
      meanings <- esa_split_attr(md, paste0(varname, "#flag_meanings"))

      if (length(values) == 0 || length(values) != length(meanings)) {
        stop("flag_values / flag_meanings missing or mismatched")
      }

      data.frame(
        value = as.numeric(values),
        category = meanings,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      warning(
        sprintf(
          "esa_landcover_legend(): could not read legend from %s (%s). Stack will be written without class labels.",
          basename(archive),
          conditionMessage(e)
        ),
        call. = FALSE
      )
      NULL
    }
  )

  legend

}

#' Pull one `key=value` line out of gdalinfo metadata and split it into tokens
esa_split_attr <- function(md, key){

  line <- grep(paste0("^", key, "="), md, value = TRUE)

  if (length(line) == 0) {
    return(character(0))
  }

  line[1] |>
    sub(pattern = paste0("^", key, "="), replacement = "", x = _) |>
    gsub(pattern = "[{}]", replacement = "", x = _) |>
    strsplit(split = "[,[:space:]]+") |>
    unlist() |>
    trimws() |>
    (\(x) x[nzchar(x)])()

}
