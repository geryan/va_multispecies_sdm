# ===========================================================================
# SUPERSEDED 2026-09-03. `model_data_spatial_landcover` no longer exists: the
# match_landcover_data() call was folded into `model_data_spatial` itself when
# the ESA classes replaced the WorldCover covariates in the model, and it now
# runs with prefix = "", keep_classes = model_landcover_classes and
# replace = TRUE. See the `model_data_spatial` target in _targets.R.
# Kept only for the notes below on clamping, undated records and verification.
# ===========================================================================

# ---------------------------------------------------------------------------
# Year-matched ESA land cover proportions on the model data
#   -- target to drop into _targets.R
#
# Paste the tar_target() call below -- the contents of the list(), not the
# list() itself -- into the main list() in _targets.R, immediately after the
# existing `model_data_spatial` target. The list() wrapper is here only so that
# this file parses and can be checked on its own; delete it on the way in, and
# add the trailing comma after its closing `)` that the wrapper does not need.
#
# Nothing here is a prototype. The two functions it calls live in R/ and are
# already loaded by the existing tar_source("R"):
#   R/match_landcover_data.R            the land cover analogue of match_offset_data()
#   R/extract_year_indexed_layer_data.R the annual analogue of extract_ym_indexed_layer_data()
#
# WHAT IT DOES
# For every record in `model_data_spatial`, take the year of `model_date` and
# read the land cover proportion at that record's location from THAT year's
# layer, for all 10 classes, and append them as columns. It is the same
# date-matched extraction `match_offset_data()` does against `offsets_5`, with
# a year index instead of a year-month index.
#
# THIS ADDS A NEW TARGET AND INVALIDATES NOTHING
# `model_data_spatial_landcover` sits downstream of `model_data_spatial`, so
# adding it leaves every existing target -- including the fits -- valid. Fold
# the call into `model_data_spatial` itself only if you want the whole
# downstream chain rebuilt; see the note at the bottom.
#
# COST, measured on this machine over the real data: 7 s, negligible memory.
# 53 430 records sit on 3 450 distinct locations and 5 361 distinct
# location-dates, so the extraction is done once per distinct key and joined
# back on. Each class file is opened lazily and only the needed cells are read.
# ---------------------------------------------------------------------------

list(

  # Year-matched land cover proportions appended to the model data.
  #
  # `esa_landcover_proportion` is a branched format = "file" target, so it
  # arrives here as the 10 per-class paths; `esa_landcover_classes` is passed
  # only so a mismatch is an error rather than a silently relabelled column
  # (the class of each file is read from its own layer names).
  tar_target(
    model_data_spatial_landcover,
    match_landcover_data(
      model_data_spatial = model_data_spatial,
      landcover_paths = esa_landcover_proportion,
      landcover_classes = esa_landcover_classes
    )
  )

)

# ---------------------------------------------------------------------------
# WHAT YOU GET
#
#   tar_load(model_data_spatial_landcover)
#
# `model_data_spatial` with 11 columns added: one per land cover class, plus
# `esa_year`, the year each record was actually matched to.
#
#   esa_year
#   esa_crop_other  esa_irrigated  esa_tree      esa_grassland  esa_sparse
#   esa_wetland     esa_mangrove   esa_urban     esa_bare       esa_water
#
# Verified on the real data: 53 430 rows in and out, no NAs in any class, and
# the 10 classes sum to exactly 1 in every row -- which is also the check that
# all 10 came from the same cell and the same year. Values were reproduced
# against a hand-built terra::extract() by lon/lat on the named year layer,
# max absolute difference 0.
#
# THE PREFIX IS LOAD-BEARING
# `model_data_spatial` already carries WorldCover covariates called
# `grassland`, `water` and `wetland` -- which are also ESA class names -- and
# `trees` / `mangroves` / `built` / `cropland`, one character or one synonym
# away from `tree` / `mangrove` / `urban` / `crop_other`. Hence `prefix =
# "esa_"` by default. match_landcover_data() errors rather than overwrite if a
# name it is about to add already exists, so `prefix = ""` fails loudly.
#
# RECORDS THE STACK CANNOT DATE
# Both groups are handled the way match_offset_data() handles them, and
# `esa_year` records what was used, so neither is silent:
#
#   - 2 975 records are dated before 1992, the first ESA year. They are
#     clamped to 1992, matching the min_year clamp in
#     extract_ym_indexed_layer_data(). Pass clamp = FALSE for NA instead.
#   - 4 726 records have no model_date at all, including all 249 background
#     points. They take the most recent year, 2022 -- the annual counterpart
#     of match_offset_data() falling back to average_last_year(). Pass
#     na_year = <year> for a different one, or na_year = NA for NA.
#
# Worth a thought before this goes in the model: every background point is
# undated, so under the default all 249 get 2022 land cover while the
# presence-only records they are the quadrature for get their own years,
# which span 1980-2021. Setting na_year to something nearer the middle of the
# presence-only records, or matching the two by some other rule, are both one
# argument away.
#
# IF YOU WANT THESE IN THE MODEL RATHER THAN JUST ON THE DATA
# Two further steps, neither of them done here:
#   1. fold the call into `model_data_spatial` itself, or point
#      `record_data_spatial` downstream at the new target, so the model data
#      carries the columns; and
#   2. add the wanted `esa_*` names to `target_covariate_names`.
# (2) changes the design matrix -- J = A + AB, so each covariate added is
# 1 + 17 columns per species -- and invalidates every fit.
# ---------------------------------------------------------------------------
