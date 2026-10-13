#' Drop inferred zeros that a record at the same location contradicts
#'
#' generate_model_data_records() infers a zero (an absence, or a count of 0)
#' for every target species a survey did not report, whenever one of that
#' survey's records used a molecular ID. It does so survey by survey, so it
#' does not look at what else has been recorded at the same place. This drops
#' the inferred rows for species `s` at a location where either:
#'
#' 1. a complex or group containing `s` (see complex_member_defs()) was recorded
#'    present, at any time, since that record may be `s` identified only to
#'    complex level; or
#' 2. `s` itself was recorded present, at any time.
#'
#' A location is the exact coordinates, the same key the fit uses. "Present" is
#' decided exactly as generate_model_data_records() decides it for observed
#' records: a positive count, else no zero count and a stated presence, else no
#' stated absence.
#'
#' Only inferred rows are dropped. Observed records and background points
#' (`inferred` NA) are all kept, and no row is changed.
#'
#' @param model_data_spatial the model data
#' @param full_data_records all cleaned records, including the complex-level
#'   ones that are not target species
#' @param complex_members tibble of `label` and `species`, from
#'   complex_member_defs()
#' @return `model_data_spatial` without the contradicted inferred rows
#' @author geryan
#' @export
drop_contradicted_inferred_zeros <- function(
    model_data_spatial,
    full_data_records,
    complex_members
){

  present_records <- full_data_records |>
    dplyr::mutate(
      present = dplyr::case_when(
        occurrence_n > 0 ~ TRUE,
        occurrence_n == 0 ~ FALSE,
        binary_presence == "yes" ~ TRUE,
        binary_absence == "yes" ~ FALSE,
        .default = TRUE
      )
    ) |>
    dplyr::filter(present) |>
    dplyr::distinct(
      label = species,
      latitude,
      longitude
    )

  # species that can be taken to be present at each location: the species
  # recorded there (rule 2), and the members of every complex recorded there
  # (rule 1)
  covered <- dplyr::bind_rows(
    present_records |>
      dplyr::transmute(
        species = label,
        latitude,
        longitude
      ),
    present_records |>
      dplyr::inner_join(
        complex_members,
        by = "label",
        relationship = "many-to-many"
      ) |>
      dplyr::select(
        species,
        latitude,
        longitude
      )
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      covered = TRUE
    )

  out <- model_data_spatial |>
    dplyr::left_join(
      covered,
      by = c(
        "species",
        "latitude",
        "longitude"
      )
    ) |>
    dplyr::filter(
      !(inferred %in% TRUE & covered %in% TRUE)
    ) |>
    dplyr::select(
      -covered
    )

  # `covered` is distinct on the join key, so the join cannot add rows
  if (nrow(out) > nrow(model_data_spatial)) {
    stop(
      "drop_contradicted_inferred_zeros(): join added rows",
      call. = FALSE
    )
  }

  out

}
