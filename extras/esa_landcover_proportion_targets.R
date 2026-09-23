# ---------------------------------------------------------------------------
# ESA land cover as CLASS PROPORTIONS -- targets to drop into _targets.R
#
# Paste the four tar_target() calls below -- the contents of the list(), not
# the list() itself -- into the main list() in _targets.R, immediately after
# the existing `esa_landcover_all` target. The list() wrapper is here only so
# that this file parses and can be checked on its own; delete it on the way in.
#
# Nothing here is a prototype. The five functions it calls live in R/ and are
# already loaded by the existing tar_source("R").
#
# WHY IT DOES NOT REUSE esa_landcover_year
# prepare_esa_landcover() resamples with method = "mode", so each 5 km cell in
# esa_landcover_year holds only the majority class of the 225 300 m cells
# under it -- the sub-grid mix is gone by then. Proportions therefore have to
# be rebuilt from the source NetCDFs, which is why these targets depend on
# esa_landcover_zip rather than on esa_landcover_year. The existing
# esa_landcover_year / esa_landcover_all targets are untouched and stay valid.
#
# COST, measured on this machine over the full African extent (1992):
#   58 s and 2.2 GB peak RSS per year, so ~30 min for 1992-2022
#   16 MB per year-file, ~500 MB for the per-year set and ~500 MB per-class
#   scratch: the segregated source-resolution stack, in tempdir(), wiped per
#   branch whether or not the branch errors
#
# The branches are independent, so an interrupted run resumes at the year it
# stopped on, exactly like esa_landcover_year.
# ---------------------------------------------------------------------------

list(

  # the 38 -> 10 class grouping, as a target so that editing
  # esa_landcover_group_defs() invalidates only what depends on it
  tar_target(
    esa_landcover_groups,
    esa_landcover_group_defs()
  ),

  tar_target(
    esa_landcover_classes,
    names(esa_landcover_groups)
  ),

  # branched over year: one file per year, one layer per class.
  # reads the legend out of each year's own NetCDF and errors if it does not
  # match the grouping, so a product-version legend change cannot pass silently
  tar_target(
    esa_landcover_proportion_year,
    proportion_esa_landcover(
      archive = esa_landcover_zip,
      new_mask = project_mask_5_outline,
      year = esa_landcover_years,
      groups = esa_landcover_groups,
      outputdir = "outputs/raster/esa_landcover_proportion"
    ),
    pattern = map(esa_landcover_zip, esa_landcover_years),
    format = "file"
  ),

  # the deliverable, branched over class: one file per class, one layer per
  # year. `esa_landcover_proportion_year` is not in the pattern, so each
  # branch receives all 31 paths and pulls its own class out of each
  tar_target(
    esa_landcover_proportion,
    stack_esa_landcover_proportion(
      paths = esa_landcover_proportion_year,
      years = esa_landcover_years,
      class = esa_landcover_classes,
      outputdir = "outputs/raster/esa_landcover_proportion"
    ),
    pattern = map(esa_landcover_classes),
    format = "file"
  )

)

# ---------------------------------------------------------------------------
# USING THE RESULT
#
#   tar_load(esa_landcover_proportion)         # 10 paths, in class order
#   tree <- terra::rast(grep("_tree[.]tif$", esa_landcover_proportion, value = TRUE))
#   tree                                       # 31 layers, tree_1992 .. tree_2022
#   tree[["tree_2015"]]                        # subset by name, or by time()
#
# Every layer is a proportion in [0, 1] of the 300 m cells that carried usable
# cover. Across the 10 classes the proportions sum to 1 at every non-NA cell:
# no_data, snow_and_ice and lichens_and_mosses are dropped from BOTH the
# numerator and the denominator, so a cell that is part snow is renormalised
# over the cover it does have rather than being scaled down.
# proportion_esa_landcover() errors rather than return a stack where that
# fails, which is also the check that NA survived the warp.
#
# NA means no usable source cell: sea, and anything that was entirely
# no_data / snow_and_ice / lichens_and_mosses. It is NA in all 10 classes at
# once, never 0.
# ---------------------------------------------------------------------------
