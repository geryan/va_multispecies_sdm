#' Aggregate the 38 LCCS classes into the 10 modelling classes
#'
#' The ESA CCI legend is finer than anything the model can use -- 38 classes,
#' several of which differ only in canopy closure or leaf type. This collapses
#' them to the 10 covers that matter here, keyed on the legend's own
#' `category` strings rather than on the numeric codes, so a code reshuffle
#' between product versions cannot silently remap a class.
#'
#' Three classes are deliberately dropped rather than grouped:
#' no_data, snow_and_ice and lichens_and_mosses. Dropped means dropped from
#' BOTH sides of the proportion -- see proportion_esa_landcover() -- so the 10
#' proportions sum to 1 over the cells that carry any usable cover at all,
#' and a cell that is entirely no_data comes out NA rather than 0.
#'
#' Returns a named list of character vectors: group -> legend categories. The
#' names, and their order, become the layer order of the proportion stacks.
esa_landcover_group_defs <- function(){

  list(

    crop_other = c(
      "cropland_rainfed",
      "cropland_rainfed_herbaceous_cover",
      "cropland_rainfed_tree_or_shrub_cover",
      "mosaic_cropland"
    ),

    irrigated = "cropland_irrigated",

    tree = c(
      "mosaic_natural_vegetation",
      "tree_broadleaved_evergreen_closed_to_open",
      "tree_broadleaved_deciduous_closed_to_open",
      "tree_broadleaved_deciduous_closed",
      "tree_broadleaved_deciduous_open",
      "tree_needleleaved_evergreen_closed_to_open",
      "tree_needleleaved_evergreen_closed",
      "tree_needleleaved_evergreen_open",
      "tree_needleleaved_deciduous_closed_to_open",
      "tree_needleleaved_deciduous_closed",
      "tree_needleleaved_deciduous_open",
      "tree_mixed",
      "mosaic_tree_and_shrub"
    ),


    shrubland = c(
      "shrubland",
      "shrubland_evergreen",
      "shrubland_deciduous"
    ),

    grassland = c(
      "grassland",
      "mosaic_herbaceous"
    ),

    sparse = c(
      "sparse_vegetation",
      "sparse_tree",
      "sparse_shrub",
      "sparse_herbaceous"
    ),

    wetland = c(
      "tree_cover_flooded_fresh_or_brakish_water",
      "shrub_or_herbaceous_cover_flooded"
    ),

    mangrove = "tree_cover_flooded_saline_water",

    urban = "urban",

    bare = c(
      "bare_areas",
      "bare_areas_consolidated",
      "bare_areas_unconsolidated"
    ),

    water = "water"

  )

}

#' Legend categories excluded from the proportions entirely
esa_landcover_ignored_classes <- function(){

  c("no_data", "snow_and_ice", "lichens_and_mosses")

}

#' Join a group definition onto a legend, and check the two agree
#'
#' The check is the point. Every legend category has to be either assigned to
#' a group or explicitly ignored, and every category named in the definition
#' has to exist in the legend -- so a legend that gains, loses or renames a
#' class between product versions stops the run instead of quietly dropping
#' cover into the denominator. (As at 2026-08-28 the v2_0_7cds and v2_1_1
#' legends are byte-identical, 38 classes each, so this is insurance.)
#'
#' @param lookup data.frame(value, category), from esa_landcover_legend()
#' @return data.frame(value, category, group, group_id), ordered by value.
#'   `group` is a factor whose level order is the order of `groups`; ignored
#'   classes get NA for both `group` and `group_id`.
esa_landcover_group_lookup <- function(
    lookup,
    groups = esa_landcover_group_defs(),
    ignore = esa_landcover_ignored_classes()
  ){

  if (is.null(lookup) || !all(c("value", "category") %in% names(lookup))) {
    stop(
      "esa_landcover_group_lookup(): `lookup` must be a data.frame with `value` and `category`",
      call. = FALSE
    )
  }

  assigned <- unlist(groups, use.names = FALSE)

  if (anyDuplicated(assigned)) {
    stop(
      sprintf(
        "esa_landcover_group_lookup(): category assigned to more than one group: %s",
        paste(unique(assigned[duplicated(assigned)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_from_legend <- setdiff(c(assigned, ignore), lookup$category)

  if (length(missing_from_legend) > 0) {
    stop(
      sprintf(
        "esa_landcover_group_lookup(): named in the grouping but absent from the legend: %s",
        paste(missing_from_legend, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  unhandled <- setdiff(lookup$category, c(assigned, ignore))

  if (length(unhandled) > 0) {
    stop(
      sprintf(
        "esa_landcover_group_lookup(): legend class neither grouped nor ignored: %s",
        paste(unhandled, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  membership <- data.frame(
    category = assigned,
    group = rep(names(groups), lengths(groups)),
    stringsAsFactors = FALSE
  )

  out <- merge(lookup, membership, by = "category", all.x = TRUE)

  out$group <- factor(out$group, levels = names(groups))
  out$group_id <- as.integer(out$group)

  out <- out[order(out$value), c("value", "category", "group", "group_id")]

  rownames(out) <- NULL

  out

}
