#' Target species covered by each complex / group label in the data
#'
#' One row per data label and target species it could be. Used by
#' drop_contradicted_inferred_zeros(): where a label was recorded present, no
#' zero is inferred for its members, as the record may be one of them
#' identified only to complex or group level.
#'
#' Labels are as they appear in `full_data_records`, i.e. after
#' clean_species(). Membership follows Harbach's Mosquito Taxonomic Inventory
#' "Anopheles classification" (updated 6 January 2024), which the Walter Reed
#' Biosystematics Unit (WRBU) classification follows:
#'
#' - Gambiae Complex (Pyretophorus Series): amharicus, arabiensis, bwambae,
#'   coluzzii, gambiae, melas, merus, quadriannulatus
#' - Funestus Group (Myzomyia Series): Funestus Subgroup (funestus, funestus-like,
#'   parensis, vaneedeni, ...), Minimus Subgroup (leesoni, ...), Rivulorum
#'   Subgroup (rivulorum, rivulorum-like, brucei, fuscivenosus). The data's
#'   "funestus complex" is taken to be the morphological group, so it covers
#'   leesoni and rivulorum as well as funestus
#' - Coustani Group (Myzorhynchus Series): caliginosus, coustani, crypticus,
#'   fuscicolor, namibiensis, paludis, symesi, tenebrosus, ziemanni
#' - Nili Complex (Ardensis Group): carnevalei, nili, ovengensis, somalicus
#'
#' Two further choices, not taxonomy:
#' - `gambiae_coluzzii` records name the two species without separating them
#'   (the old S / M forms)
#' - "funestus like" and "rivulorum like" are treated as the funestus and
#'   rivulorum complexes. They are distinct species of the Funestus Group, and
#'   clean_species() means to map them to complexes but misses them, because
#'   the data spell them with a space rather than a hyphen
#'
#' Not included: hybrid and either-or labels (hybrid_gambiae_melas,
#' hybrid_coluzzii_melas, rufipes_pretoriensis,
#' hybrid_funestus_rivulorum-like), and complexes with no target member
#' (marshallii_complex, culicifacies_complex).
#'
#' @return tibble with columns `label` and `species`
#' @author geryan
#' @export
complex_member_defs <- function(){

  members <- list(
    gambiae_complex = c(
      "arabiensis",
      "coluzzii",
      "gambiae",
      "melas",
      "merus",
      "quadriannulatus"
    ),
    gambiae_coluzzii = c(
      "coluzzii",
      "gambiae"
    ),
    funestus_complex = c(
      "funestus",
      "leesoni",
      "rivulorum"
    ),
    `funestus like` = c(
      "funestus",
      "leesoni",
      "rivulorum"
    ),
    coustani_complex = c(
      "coustani",
      "ziemanni"
    ),
    nili_complex = "nili",
    rivulorum_complex = "rivulorum",
    `rivulorum like` = "rivulorum"
  )

  tibble::tibble(
    label = rep(
      names(members),
      lengths(members)
    ),
    species = unlist(
      members,
      use.names = FALSE
    )
  )

}
