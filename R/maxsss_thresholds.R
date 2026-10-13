#' Per-species occurrence thresholds that maximise sensitivity + specificity
#'
#' The maxSSS criterion of Liu et al. (2005), recommended again by Liu et al.
#' (2013) for presence-only data: for each species, the probability threshold
#' at which sensitivity + specificity -- equivalently, the true skill statistic
#' -- is greatest.
#'
#' Evaluated on unique cells of `pred_p`, so densely sampled places count once:
#' a cell is a presence if any record of the species there detected it, and an
#' absence if records there only failed to. The absences are whatever
#' `model_data_spatial` holds -- for the _rep section observed and inferred
#' zeros, for _noinf observed only, for _cx the filtered set -- i.e. the zeros
#' that section's model was fitted to. Background points are not used, and
#' cells where `pred_p` is NA are dropped.
#'
#' Candidate thresholds are 0 and every distinct predicted value at those
#' cells; a cell is predicted present when its value is above the threshold
#' (at or above, for a threshold of 0); tied maxima are averaged. Those are the
#' conventions of PresenceAbsence::optimal.thresholds(opt.methods =
#' "MaxSens+Spec") and its cmx(), which give the same threshold when handed the
#' same candidates (extras/check_maxsss.R). binarise_by_threshold() applies the
#' same rule.
#'
#' Liu, C., Berry, P.M., Dawson, T.P. & Pearson, R.G. (2005) Selecting
#' thresholds of occurrence in the prediction of species distributions.
#' Ecography 28, 385-393.
#' Liu, C., White, M. & Newell, G. (2013) Selecting thresholds for the
#' prediction of species occurrence with presence-only data. Journal of
#' Biogeography 40, 778-789.
#'
#' @param pred_p SpatRaster of probability of occurrence, one layer per species
#' @param model_data_spatial the records a section's model was fitted to
#' @return tibble: species, threshold, sensitivity and specificity at that
#'   threshold, and the numbers of presence and absence cells
#' @author geryan
#' @export
maxsss_thresholds <- function(
    pred_p,
    model_data_spatial
){

  cell <- terra::cellFromXY(
    pred_p[[1]],
    as.matrix(model_data_spatial[, c("longitude", "latitude")])
  )

  obs <- model_data_spatial |>
    dplyr::mutate(
      cell = cell
    ) |>
    dplyr::filter(
      !is.na(species),
      !is.na(cell)
    ) |>
    dplyr::group_by(
      species,
      cell
    ) |>
    dplyr::summarise(
      observed = as.integer(any(presence == 1)),
      .groups = "drop"
    )

  values <- terra::extract(
    pred_p,
    unique(obs$cell)
  )

  row <- match(
    obs$cell,
    unique(obs$cell)
  )

  dplyr::bind_rows(
    lapply(
      names(pred_p),
      function(sp){

        o <- obs[obs$species == sp, ]
        pred <- values[row[obs$species == sp], sp]
        keep <- !is.na(pred)

        out <- maxsss(
          pred = pred[keep],
          observed = o$observed[keep]
        )

        dplyr::tibble(
          species = sp,
          threshold = out[["threshold"]],
          sensitivity = out[["sensitivity"]],
          specificity = out[["specificity"]],
          presence_cells = sum(o$observed[keep] == 1),
          absence_cells = sum(o$observed[keep] == 0)
        )

      }
    )
  )

}

#' The maxSSS threshold for one set of predictions and 0/1 observations
#'
#' @param pred predicted probabilities
#' @param observed 1 for presence, 0 for absence
#' @return named numeric: threshold, sensitivity, specificity. NA throughout,
#'   with a warning, if there are no presences or no absences
maxsss <- function(
    pred,
    observed
){

  p <- pred[observed == 1]
  a <- pred[observed == 0]

  if (length(p) == 0 || length(a) == 0) {
    warning(
      "maxsss(): needs both presences and absences; returning NA",
      call. = FALSE
    )
    return(
      c(
        threshold = NA_real_,
        sensitivity = NA_real_,
        specificity = NA_real_
      )
    )
  }

  candidates <- sort(unique(c(0, pred)))

  sensitivity <- vapply(
    candidates,
    function(t) mean(predicted_present(p, t)),
    numeric(1)
  )

  specificity <- vapply(
    candidates,
    function(t) mean(!predicted_present(a, t)),
    numeric(1)
  )

  total <- sensitivity + specificity

  threshold <- mean(candidates[total == max(total)])

  c(
    threshold = threshold,
    sensitivity = mean(predicted_present(p, threshold)),
    specificity = mean(!predicted_present(a, threshold))
  )

}

#' Present above the threshold, or at or above a threshold of 0, as
#' PresenceAbsence::cmx() classifies
#'
#' @param x predicted probabilities: numeric vector or SpatRaster
#' @param threshold a single threshold
#' @return logical, of the same kind as `x`
predicted_present <- function(
    x,
    threshold
){

  if (threshold == 0) {
    x >= threshold
  } else {
    x > threshold
  }

}
