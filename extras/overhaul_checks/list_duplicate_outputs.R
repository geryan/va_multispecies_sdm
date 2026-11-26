# Lists the output files and folders the 2026-10-14 overhaul leaves behind, so
# they can be checked and removed by hand. Deletes nothing.
#
# Four groups:
#   renamed       written by the _rep_cx targets; the same target now writes to
#                 a new path, given in `replaced_by`
#   removed set   written by the removed _rep and _noinf model sets
#   older         anything else in outputs/rasters and outputs/images that no
#                 current target writes
#   store         objects in _targets/objects whose target no longer exists;
#                 tar_prune() removes exactly these
#
# `keep_until_check_04` marks what 04_compare_after_run.R reads: leave those
# until it has run. `named_in_code` is TRUE where the file name still appears
# in _targets.R or R/, so look before deleting.
#
# Run from the repo root, before or after the pipeline has been rebuilt:
#   Rscript extras/overhaul_checks/list_duplicate_outputs.R

library(targets)
library(dplyr)

outdir <- "outputs/overhaul_check"

describe <- function(path) {

  files <- if (dir.exists(path)) {
    list.files(
      path,
      recursive = TRUE,
      full.names = TRUE
    )
  } else if (file.exists(path)) {
    path
  } else {
    character()
  }

  tibble(
    path = path,
    type = if (dir.exists(path)) "folder" else "file",
    n_files = length(files),
    size_mb = round(sum(file.size(files)) / 1e6, 1),
    modified = if (length(files)) {
      format(max(file.mtime(files)), "%Y-%m-%d")
    } else {
      NA_character_
    }
  )

}

entries <- function(
    paths,
    group,
    replaced_by = NA_character_,
    keep_until_check_04 = FALSE
) {

  paths <- paths[file.exists(paths)]

  if (!length(paths)) {
    return(NULL)
  }

  lapply(paths, describe) |>
    bind_rows() |>
    mutate(
      group = group,
      replaced_by = replaced_by,
      keep_until_check_04 = keep_until_check_04
    )

}

###################
# renamed: the _rep_cx outputs
###################

renamed <- bind_rows(
  entries(
    "outputs/images/model_fit_test_source_re_rep_cx.RData",
    group = "renamed",
    replaced_by = "outputs/images/model_fit.RData",
    keep_until_check_04 = TRUE
  ),
  entries(
    "outputs/rasters/reparam_multispecies_pp_rep_cx.tif",
    group = "renamed",
    replaced_by = "outputs/rasters/multispecies_pp.tif",
    keep_until_check_04 = TRUE
  ),
  entries(
    "outputs/rasters/reparam_multispecies_pp_rep_cx_p.tif",
    group = "renamed",
    replaced_by = "outputs/rasters/multispecies_pp_p.tif",
    keep_until_check_04 = TRUE
  ),
  entries(
    "outputs/rasters/reparam_multispecies_pp_rep_cx_p_cv.tif",
    group = "renamed",
    replaced_by = "outputs/rasters/multispecies_pp_p_cv.tif",
    keep_until_check_04 = TRUE
  ),
  entries(
    "outputs/rasters/countries_rep_cx",
    group = "renamed",
    replaced_by = "outputs/rasters/countries"
  ),
  entries(
    "outputs/draws/coef_draws_rep_cx.rds",
    group = "renamed",
    replaced_by = "outputs/draws/coef_draws.rds",
    keep_until_check_04 = TRUE
  ),
  entries(
    "outputs/figures/distribution_plots/distn_20260928_rep_cx",
    group = "renamed",
    replaced_by = "outputs/figures/distribution_plots/distn_20261014"
  ),
  entries(
    "outputs/figures/validation/sre_rep_cx_20260928",
    group = "renamed",
    replaced_by = "outputs/figures/validation/20261014"
  ),
  entries(
    "outputs/rasters/abundance_cubes",
    group = "renamed",
    replaced_by = "outputs/rasters/abundance_cubes_masked"
  )
)

###################
# removed sets: _rep and _noinf
###################

removed_sets <- entries(
  c(
    list.files(
      "outputs/rasters",
      pattern = "^reparam_multispecies_pp_rep(_noinf)?(_p|_p_cv|_masked)?\\.tif$",
      full.names = TRUE
    ),
    list.files(
      "outputs/images",
      pattern = "^model_fit_test_source_re_rep(_noinf)?\\.RData$",
      full.names = TRUE
    ),
    "model_fit_test_source_re_rep.RData",
    list.files(
      "outputs/figures/distribution_plots",
      pattern = "^distn_20260928_rep(_noinf)?$",
      full.names = TRUE
    ),
    list.files(
      "outputs/figures/validation",
      pattern = "^sre_rep(_noinf)?_20260928$",
      full.names = TRUE
    )
  ),
  group = "removed set"
)

###################
# older: nothing current writes these
###################

current <- c(
  "outputs/rasters/multispecies_pp.tif",
  "outputs/rasters/multispecies_pp_p.tif",
  "outputs/rasters/multispecies_pp_p_cv.tif",
  "outputs/rasters/countries",
  "outputs/rasters/abundance_cubes_masked",
  "outputs/images/model_fit.RData"
)

already_listed <- c(
  renamed$path,
  removed_sets$path
)

older <- entries(
  setdiff(
    c(
      list.files(
        "outputs/rasters",
        full.names = TRUE
      ),
      list.files(
        "outputs/images",
        full.names = TRUE
      )
    ),
    c(current, already_listed)
  ),
  group = "older"
)

###################
# store: objects with no target
###################

manifest_names <- tar_manifest(fields = "name")$name

# a branch's object is named <target>_<hash>, so match on the target's name
meta <- tar_meta(fields = tidyselect::any_of(c("name", "parent", "type")))

branch_parent <- setNames(
  meta$parent,
  meta$name
)

stored <- list.files("_targets/objects")

owner <- ifelse(
  !is.na(branch_parent[stored]),
  branch_parent[stored],
  stored
)

orphans <- stored[!owner %in% manifest_names]

store <- entries(
  file.path("_targets/objects", orphans),
  group = "store"
)

###################
# together
###################

code <- unlist(
  lapply(
    c(
      "_targets.R",
      list.files(
        "R",
        full.names = TRUE
      )
    ),
    readLines,
    warn = FALSE
  )
)

duplicates <- bind_rows(
  renamed,
  removed_sets,
  older,
  store
) |>
  mutate(
    replaced_by_exists = !is.na(replaced_by) & file.exists(replaced_by),
    # as a whole path component: "abundance_cubes" must not match
    # "abundance_cubes_masked"
    named_in_code = group != "store" & vapply(
      basename(path),
      function(name) {
        any(grepl(paste0(name, "\""), code, fixed = TRUE)) ||
          any(grepl(paste0(name, "/"), code, fixed = TRUE))
      },
      logical(1)
    )
  ) |>
  select(
    group,
    path,
    type,
    n_files,
    size_mb,
    modified,
    replaced_by,
    replaced_by_exists,
    keep_until_check_04,
    named_in_code
  )

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  duplicates,
  file.path(
    outdir,
    "duplicate_outputs.csv"
  )
)

duplicates |>
  group_by(group) |>
  summarise(
    entries = n(),
    files = sum(n_files),
    size_gb = round(sum(size_mb) / 1e3, 2)
  ) |>
  print()

cat(
  sprintf(
    "written: %s\n",
    file.path(outdir, "duplicate_outputs.csv")
  )
)
