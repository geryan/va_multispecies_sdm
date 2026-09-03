#' Resolve which per-class land cover stack holds which class
#'
#' The `esa_landcover_proportion` target is branched over class, so it arrives
#' downstream as a bare vector of file paths. The class each file holds is read
#' from that file's own layer names (`<class>_<year>`) rather than from its
#' position in the vector, so a change in branch order cannot silently relabel
#' anything.
#'
#' @param landcover_paths character vector of paths to the per-class stacks,
#'   i.e. the `esa_landcover_proportion` target
#' @param classes optional character vector of the classes wanted. If given,
#'   only those are returned, in the order given, and a class that is not
#'   present is an error
#' @return named character vector, class -> path
#' @author geryan
#' @export
esa_landcover_class_paths <- function(
    landcover_paths,
    classes = NULL
  ) {

  landcover_paths <- as.character(landcover_paths)

  missing_paths <- landcover_paths[!file.exists(landcover_paths)]

  if (length(missing_paths)) {
    stop(
      sprintf(
        "esa_landcover_class_paths(): file not found: %s",
        paste(missing_paths, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  found <- vapply(
    landcover_paths,
    function(path) {
      cl <- unique(sub("_[0-9]{4}$", "", names(terra::rast(path))))

      if (length(cl) != 1L) {
        stop(
          sprintf(
            "esa_landcover_class_paths(): %s holds more than one class (%s)",
            basename(path),
            paste(cl, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      cl
    },
    character(1),
    USE.NAMES = FALSE
  )

  if (anyDuplicated(found)) {
    stop(
      sprintf(
        "esa_landcover_class_paths(): duplicated land cover class: %s",
        paste(unique(found[duplicated(found)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- stats::setNames(landcover_paths, found)

  if (!is.null(classes)) {

    classes <- as.character(classes)

    absent <- setdiff(classes, found)

    if (length(absent)) {
      stop(
        sprintf(
          "esa_landcover_class_paths(): class not among the files supplied: %s (files hold %s)",
          paste(absent, collapse = ", "),
          paste(sort(found), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    out <- out[classes]

  }

  out

}


#' One year of ESA land cover proportions, as model covariate layers
#'
#' Pulls a single year out of the per-class proportion stacks written by
#' `stack_esa_landcover_proportion()` and returns it as one SpatRaster, one
#' layer per class, named by class alone (no year suffix) so the layer names
#' are the model covariate names.
#'
#' This is the static, single-year face of the time-varying land cover: it is
#' what goes into `covariate_rast_5_all`, and so what the model predicts to and
#' what the background points are drawn against. Records get their own year's
#' values instead, through `match_landcover_data()`.
#'
#' The year is found by matching against the stack's own time axis, not by
#' arithmetic on layer position, matching `extract_year_indexed_layer_data()`.
#'
#' The proportion stacks are written on `project_mask_5_outline`, so they carry
#' values in the coastal and inland water cells that `project_mask_5` removes.
#' Passing `project_mask` masks them to the analysis grid and asserts the result
#' has no NA anywhere the mask has data -- the condition `mismatched_nas` checks
#' downstream, failing here with a usable message rather than there with a cell
#' index.
#'
#' @param landcover_paths character vector of paths to the per-class stacks,
#'   i.e. the `esa_landcover_proportion` target
#' @param classes classes to return, in the order they should appear as layers.
#'   These become the layer names
#' @param year the year to take, e.g. 2022
#' @param project_mask optional SpatRaster to mask to, i.e. `project_mask_5`
#' @return SpatRaster with one layer per class in `classes`
#' @author geryan
#' @export
esa_landcover_model_layers <- function(
    landcover_paths,
    classes,
    year,
    project_mask = NULL
  ) {

  year <- as.integer(year)

  if (length(year) != 1L || is.na(year)) {
    stop(
      "esa_landcover_model_layers(): `year` must be a single year",
      call. = FALSE
    )
  }

  paths <- esa_landcover_class_paths(landcover_paths, classes)

  layers <- lapply(
    names(paths),
    function(cl) {

      r <- terra::rast(paths[[cl]])

      years <- suppressWarnings(as.integer(terra::time(r)))

      if (anyNA(years)) {
        stop(
          sprintf(
            "esa_landcover_model_layers(): %s has layers with no year on its time axis",
            basename(paths[[cl]])
          ),
          call. = FALSE
        )
      }

      idx <- match(year, years)

      if (is.na(idx)) {
        stop(
          sprintf(
            "esa_landcover_model_layers(): %s holds %d-%d, not %d",
            basename(paths[[cl]]),
            min(years),
            max(years),
            year
          ),
          call. = FALSE
        )
      }

      out <- r[[idx]]

      # the year is fixed and recorded in the target, so drop it from the layer
      # name and the time axis: these are covariate layers now, and they get
      # combined with layers that have no time of their own
      names(out) <- cl
      terra::time(out) <- NULL

      out

    }
  )

  out <- terra::rast(layers)

  if (!is.null(project_mask)) {

    if (!terra::compareGeom(out, project_mask, stopOnError = FALSE)) {
      stop(
        "esa_landcover_model_layers(): land cover is not on the same grid as `project_mask`",
        call. = FALSE
      )
    }

    out <- terra::mask(out, project_mask)

    # NA anywhere the mask has data would put NAs into the design matrix and
    # trip check_no_mismatched_nas() downstream
    extra_na <- terra::global(
      is.na(out) & !is.na(project_mask),
      fun = "sum",
      na.rm = TRUE
    )

    bad <- rownames(extra_na)[extra_na[, 1] > 0]

    if (length(bad)) {
      stop(
        sprintf(
          "esa_landcover_model_layers(): NA inside the project mask in: %s",
          paste(
            sprintf("%s (%d cells)", bad, extra_na[bad, 1]),
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

  }

  out

}
