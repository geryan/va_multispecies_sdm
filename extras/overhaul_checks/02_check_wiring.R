# Wiring check for the 2026-10-14 overhaul of _targets.R: the pipeline as now
# defined against the snapshot taken before (01_snapshot_before.R).
#
# Builds nothing and changes nothing in the store. Run from the repo root:
#   Rscript extras/overhaul_checks/02_check_wiring.R
#
# It asks five things.
#   A. Is every carried-over model target the same command as before, once the
#      names are mapped? The deliberate differences are written out below as
#      edits to the old command, so anything else shows up as a failure.
#   B. Is every other target untouched?
#   C. Were exactly the intended targets removed and added?
#   D. Is the dependency graph the same, apart from the intended edges?
#   E. Is everything upstream of the fit still up to date in the store, and
#      does every new command refer only to things that exist?

library(targets)
library(dplyr)

source("extras/overhaul_checks/helpers.R")

before <- readRDS("outputs/overhaul_check/snapshot_before.rds")

failures <- character()

check <- function(ok, label, detail = NULL) {

  cat(
    sprintf(
      "%s  %s\n",
      if (ok) "ok  " else "FAIL",
      label
    )
  )

  if (!ok) {
    failures <<- c(failures, label)
    if (!is.null(detail)) {
      print(detail)
    }
  }

  invisible(ok)

}

###################
# the pipeline now
###################

tar_validate()

after <- list(
  manifest = tar_manifest(
    fields = tidyselect::any_of(
      c("name", "command", "pattern", "format")
    )
  ),
  edges = tar_network(
    targets_only = TRUE,
    outdated = FALSE
  )$edges
)

target_map <- overhaul_rename_map(before$manifest$name)
function_map <- overhaul_function_map()

# output paths that lost their suffixes, as they appear in the commands
path_map <- c(
  "outputs/images/model_fit_test_source_re_rep_cx.RData" = "outputs/images/model_fit.RData",
  "outputs/rasters/reparam_multispecies_pp_rep_cx" = "outputs/rasters/multispecies_pp",
  "outputs/rasters/countries_rep_cx" = "outputs/rasters/countries",
  "outputs/draws/coef_draws_rep_cx.rds" = "outputs/draws/coef_draws.rds",
  "outputs/figures/validation/sre_rep_cx_%s/" = "outputs/figures/validation/%s/",
  "outputs/figures/distribution_plots/distn_%s_rep_cx" = "outputs/figures/distribution_plots/distn_%s"
)

# rename symbols and path strings throughout an expression. Argument names are
# not symbols, so `plot_dir = rep_plot_dir_cx` becomes `plot_dir = plot_dir`
rename_expr <- function(
    x,
    symbol_map,
    string_map = character()
) {

  if (is.name(x)) {
    name <- as.character(x)
    if (name %in% names(symbol_map)) {
      return(as.name(symbol_map[[name]]))
    }
    return(x)
  }

  if (is.character(x) && length(x) == 1 && x %in% names(string_map)) {
    return(string_map[[x]])
  }

  if (is.call(x) || is.pairlist(x)) {
    x[] <- lapply(
      x,
      rename_expr,
      symbol_map = symbol_map,
      string_map = string_map
    )
  }

  x

}

parse_one <- function(text) {
  if (is.na(text)) {
    return(NULL)
  }
  parse(
    text = text,
    keep.source = FALSE
  )[[1]]
}

same_expr <- function(a, b) {
  identical(
    deparse(a),
    deparse(b)
  )
}

before_command <- lapply(
  setNames(
    before$manifest$command,
    before$manifest$name
  ),
  parse_one
)

after_command <- lapply(
  setNames(
    after$manifest$command,
    after$manifest$name
  ),
  parse_one
)

symbol_map <- c(target_map, function_map)

###################
# A. carried-over model targets
###################

# the old command of each, with the names and paths mapped
expected <- lapply(
  before_command[names(target_map)],
  rename_expr,
  symbol_map = symbol_map,
  string_map = path_map
)

names(expected) <- unname(target_map)

# the deliberate differences, each an edit to that mapped command

# new run tag
expected$run_tag <- "20261014"

# warm-up 1000 -> 2000
expected$model_fit$n_burnin <- 2000

# refit cascade: everything that reads the fit image depends on its contents
expected$resids_and_rhats$model_fit_image_multisp_pp_count_sm <- quote(model_fit_image)

expected$coef_draws$image_name <- quote(model_fit_image)

# refit cascade: the prediction paths never change, so their md5s join the value
predict_call <- expected$preds
predict_call$image_name <- quote(model_fit_image)

expected$preds <- bquote(
  {
    pred_files <- .(predict_call)
    c(
      pred_files,
      md5 = list(
        tools::md5sum(
          unlist(pred_files)
        )
      )
    )
  }
)

# slope heatmaps replaced by the bioregion multipliers
expected$plot_bioregion_multipliers <- quote(
  make_bioregion_multiplier_plots(
    coef_draws_file = coef_draws,
    covariates = covariate_rast_10,
    model_data_spatial = model_data_spatial_cx,
    plot_dir = file.path(
      plot_dir,
      "bioregion_multipliers"
    )
  )
)

deliberate <- c(
  "run_tag",
  "model_fit",
  "resids_and_rhats",
  "coef_draws",
  "preds",
  "plot_bioregion_multipliers"
)

carried <- tibble(
  old = names(target_map),
  new = unname(target_map)
) |>
  mutate(
    exists = new %in% names(after_command),
    command_as_expected = mapply(
      function(new, exists) {
        exists && same_expr(expected[[new]], after_command[[new]])
      },
      new,
      exists
    ),
    deliberate_change = new %in% deliberate
  )

pattern_of <- function(manifest) {
  setNames(
    manifest$pattern,
    manifest$name
  )
}

format_of <- function(manifest) {
  setNames(
    manifest$format,
    manifest$name
  )
}

carried <- carried |>
  mutate(
    pattern_same = mapply(
      function(old, new) {
        same_expr(
          rename_expr(
            parse_one(pattern_of(before$manifest)[[old]]),
            symbol_map = symbol_map
          ),
          parse_one(pattern_of(after$manifest)[[new]])
        )
      },
      old,
      new
    ),
    format_same = format_of(before$manifest)[old] == format_of(after$manifest)[new]
  )

cat("\nA. carried-over model targets\n")

check(
  all(carried$exists),
  sprintf(
    "all %s carried-over targets exist under their new names",
    nrow(carried)
  ),
  carried |>
    filter(!exists)
)

check(
  all(carried$command_as_expected),
  sprintf(
    "commands identical under the rename map (%s unchanged, %s with a deliberate edit)",
    sum(!carried$deliberate_change),
    sum(carried$deliberate_change)
  ),
  carried |>
    filter(!command_as_expected)
)

check(
  all(carried$pattern_same) && all(carried$format_same),
  "branching patterns and storage formats unchanged",
  carried |>
    filter(!pattern_same | !format_same)
)

###################
# B. every other target
###################

# names present before and now that are not the new name of a renamed target
untouched_names <- setdiff(
  intersect(
    before$manifest$name,
    after$manifest$name
  ),
  unname(target_map)
)

both <- inner_join(
  before$manifest |>
    filter(name %in% untouched_names),
  after$manifest,
  by = "name",
  suffix = c("_before", "_after")
) |>
  mutate(
    same = mapply(
      function(a, b) {
        same_expr(parse_one(a), parse_one(b))
      },
      command_before,
      command_after
    ) &
      (is.na(pattern_before) & is.na(pattern_after) | pattern_before %in% pattern_after) &
      format_before == format_after
  )

# the three relinked later targets: CV reads the _cx data; the cubes read the
# new predictions, masked, and return their files
expected_relinked <- c(
  "abundance_cubes",
  "cv_folds_file",
  "cv_fold_draws"
)

cat("\nB. every other target\n")

check(
  setequal(
    both$name[!both$same],
    expected_relinked
  ),
  sprintf(
    "%s other targets unchanged; only the cubes and the two CV targets differ",
    sum(both$same)
  ),
  both |>
    filter(!same) |>
    select(name)
)

cv_expected <- lapply(
  before_command[c("cv_folds_file", "cv_fold_draws")],
  function(command) {
    command$model_data_spatial <- quote(model_data_spatial_cx)
    command
  }
)

check(
  same_expr(cv_expected$cv_folds_file, after_command$cv_folds_file) &&
    same_expr(cv_expected$cv_fold_draws, after_command$cv_fold_draws),
  "CV targets differ only in reading model_data_spatial_cx"
)

cubes_expected <- before_command$abundance_cubes

cubes_expected$lambda_no_offset_file <- quote(
  mask_landcover_and_expert_offset(
    p = rast(preds$lambda_no_offset),
    expert = expert_offset_maps_10,
    bare = landcover_bare_10
  )
)

cubes_expected$write_dir <- "outputs/rasters/abundance_cubes_masked"

check(
  same_expr(cubes_expected, after_command$abundance_cubes) &&
    format_of(after$manifest)[["abundance_cubes"]] == "file",
  "cubes differ only in the masked new predictions, the output folder and format = file"
)

###################
# C. removed and added
###################

removed <- setdiff(
  before$manifest$name,
  c(
    untouched_names,
    names(target_map)
  )
)

removed_expected <- c(
  # the original chain and _sre
  "model_fit",
  "resids_and_rhats",
  "model_fit_sre",
  "refit_model_fit_sre",
  "resids_and_rhats_sre",
  # the _rep section
  "rep_tag",
  "rep_fit_image",
  "rep_pred_prefix",
  "rep_validation_dir",
  "rep_plot_dir",
  grep(
    "_rep$",
    before$manifest$name,
    value = TRUE
  ),
  # the stale tail after the CV section
  "preds_sm",
  "pred_dist_not_masked",
  "pred_p",
  "plot_pred_p",
  "pred_pcv_not_masked",
  "pred_pcv",
  "pred_lambda_mean_not_masked",
  "pred_gr1",
  "pred_dominant",
  "pred_dist_scale_sm",
  "distribution_plots_sm",
  "rel_abund_rgb_sm",
  "rel_abund_plots_sm"
)

added <- setdiff(
  after$manifest$name,
  c(
    untouched_names,
    unname(target_map)
  )
)

added_expected <- c(
  "model_fit_image",
  "pred_dominant_expert",
  "pred_dominant_001_expert",
  "plot_pred_dominant_expert"
)

cat("\nC. removed and added\n")

check(
  setequal(removed, removed_expected),
  sprintf(
    "%s targets removed, all of them from the old model sets",
    length(removed)
  ),
  list(
    unexpected = setdiff(removed, removed_expected),
    missing = setdiff(removed_expected, removed)
  )
)

check(
  setequal(added, added_expected),
  sprintf(
    "%s targets added: %s",
    length(added),
    paste(sort(added), collapse = ", ")
  ),
  list(
    unexpected = setdiff(added, added_expected),
    missing = setdiff(added_expected, added)
  )
)

# the expert-range dominant maps are the _rep ones on the main predictions
expert_map <- c(
  pred_dominant_expert_rep = "pred_dominant_expert",
  pred_dominant_001_expert_rep = "pred_dominant_001_expert",
  plot_pred_dominant_expert_rep = "plot_pred_dominant_expert",
  pred_dominant_rep = "pred_dominant",
  pred_dominant_001_rep = "pred_dominant_001",
  rep_plot_dir = "plot_dir"
)

expert_same <- vapply(
  names(expert_map)[1:3],
  function(old) {
    same_expr(
      rename_expr(
        before_command[[old]],
        symbol_map = expert_map
      ),
      after_command[[expert_map[[old]]]]
    )
  },
  logical(1)
)

check(
  all(expert_same),
  "expert-range dominant maps are the _rep commands on the main predictions"
)

check(
  same_expr(after_command$model_fit_image, quote(model_fit)) &&
    format_of(after$manifest)[["model_fit_image"]] == "file",
  "model_fit_image is model_fit as a file target"
)

leftover <- grep(
  "_rep(_|$)|_sre(_|$)|_sm$|_cx$",
  after$manifest$name,
  value = TRUE
)

check(
  setequal(
    leftover,
    c(
      "model_data_spatial_cx",
      "data_summary_table_cx"
    )
  ),
  "no _rep, _sre or _sm name is left; _cx only on the model data and its table",
  leftover
)

###################
# D. dependency graph
###################

rename_nodes <- function(x) {
  ifelse(
    x %in% names(target_map),
    target_map[x],
    x
  ) |>
    unname()
}

edge_key <- function(edges) {
  paste(edges$from, edges$to, sep = " -> ")
}

before_edges <- before$edges |>
  filter(
    !from %in% removed,
    !to %in% removed
  ) |>
  mutate(
    from = rename_nodes(from),
    to = rename_nodes(to)
  ) |>
  edge_key()

after_edges <- edge_key(after$edges)

edges_dropped <- setdiff(before_edges, after_edges)
edges_gained <- setdiff(after_edges, before_edges)

edges_dropped_expected <- c(
  # the image target now sits between the fit and its readers
  "model_fit -> resids_and_rhats",
  "model_fit -> preds",
  "model_fit -> coef_draws",
  # CV moved to the _cx data
  "model_data_spatial -> cv_folds_file",
  "model_data_spatial -> cv_fold_draws"
)

edges_gained_expected <- c(
  "model_fit -> model_fit_image",
  "model_fit_image -> resids_and_rhats",
  "model_fit_image -> preds",
  "model_fit_image -> coef_draws",
  "model_data_spatial_cx -> cv_folds_file",
  "model_data_spatial_cx -> cv_fold_draws",
  "model_data_spatial_cx -> plot_bioregion_multipliers",
  # the cubes: new predictions, masked
  "preds -> abundance_cubes",
  "expert_offset_maps_10 -> abundance_cubes",
  "landcover_bare_10 -> abundance_cubes",
  # the expert-range dominant maps
  "pred_dominant -> pred_dominant_expert",
  "pred_dominant_001 -> pred_dominant_001_expert",
  "expert_maps_buffer_1000 -> pred_dominant_expert",
  "expert_maps_buffer_1000 -> pred_dominant_001_expert",
  "pred_dominant_expert -> plot_pred_dominant_expert",
  "pred_dominant_001_expert -> plot_pred_dominant_expert",
  "plot_dir -> plot_pred_dominant_expert"
)

cat("\nD. dependency graph\n")

check(
  setequal(edges_dropped, edges_dropped_expected),
  sprintf(
    "%s edges carried over; the %s dropped are the intended ones",
    length(intersect(before_edges, after_edges)),
    length(edges_dropped)
  ),
  list(
    unexpected = setdiff(edges_dropped, edges_dropped_expected),
    missing = setdiff(edges_dropped_expected, edges_dropped)
  )
)

check(
  setequal(edges_gained, edges_gained_expected),
  sprintf(
    "the %s gained are the intended ones",
    length(edges_gained)
  ),
  list(
    unexpected = setdiff(edges_gained, edges_gained_expected),
    missing = setdiff(edges_gained_expected, edges_gained)
  )
)

###################
# E. the store, and dangling references
###################

# everything the fit, the predictions, the maps, the cubes and the CV draw on,
# that existed before under the same name
model_targets <- c(
  unname(target_map),
  added,
  expected_relinked
)

upstream <- setdiff(
  ancestors(
    model_targets,
    after$edges
  ),
  model_targets
)

outdated_now <- tar_outdated()

upstream_meta <- before$meta |>
  filter(name %in% upstream) |>
  select(name, data_before = data) |>
  left_join(
    tar_meta(fields = "data") |>
      select(name, data_now = data),
    by = "name"
  )

cat("\nE. the store, and dangling references\n")

# bg_kmeans_df etc. feed the CV only and were never the model's inputs
upstream_outdated <- intersect(upstream, outdated_now)
upstream_outdated_before <- intersect(upstream, before$outdated)

check(
  setequal(upstream_outdated, upstream_outdated_before),
  sprintf(
    "of the %s upstream targets, none became outdated (%s were already, none of them an input to the fit)",
    length(upstream),
    length(upstream_outdated_before)
  ),
  list(
    newly_outdated = setdiff(upstream_outdated, upstream_outdated_before),
    already_outdated = upstream_outdated_before
  )
)

fit_upstream <- ancestors(
  "model_fit",
  after$edges
) |>
  setdiff("fit_image")

check(
  !any(fit_upstream %in% outdated_now),
  sprintf(
    "all %s stored inputs to the fit are up to date, so only the fit and what follows will build",
    length(fit_upstream)
  ),
  intersect(fit_upstream, outdated_now)
)

check(
  all(upstream_meta$data_before == upstream_meta$data_now, na.rm = TRUE),
  "stored data hashes of the upstream targets equal the snapshot"
)

# everything a changed or new command refers to must exist: as a target, as a
# function in R/, or in the packages the pipeline loads
pipeline_env <- new.env()

sys.source(
  "_targets.R",
  envir = pipeline_env
)

invisible(
  lapply(
    tar_option_get("packages"),
    function(package) {
      suppressPackageStartupMessages(
        library(
          package,
          character.only = TRUE
        )
      )
    }
  )
)

changed_targets <- c(
  unname(target_map),
  added,
  expected_relinked
)

unresolved <- lapply(
  setNames(changed_targets, changed_targets),
  function(name) {
    globals <- codetools::findGlobals(
      eval(
        call("function", NULL, after_command[[name]])
      ),
      merge = TRUE
    )
    globals[
      !globals %in% after$manifest$name &
        !vapply(globals, exists, logical(1))
    ]
  }
)

unresolved <- unresolved[lengths(unresolved) > 0]

check(
  length(unresolved) == 0,
  sprintf(
    "every name in the %s changed or new commands is a target, a function or a package object",
    length(changed_targets)
  ),
  unresolved
)

r_functions <- Filter(
  function(name) {
    is.function(get(name, envir = globalenv()))
  },
  ls(globalenv())
)

check(
  !any(after$manifest$name %in% r_functions),
  "no target shares a name with a function in R/",
  intersect(after$manifest$name, r_functions)
)

# files were deleted from R/, so follow the calls: every function the pipeline
# reaches, directly or through another function, must call only functions that
# still exist
calls_of <- function(f) {
  codetools::findGlobals(
    f,
    merge = FALSE
  )$functions
}

reached <- unique(
  unlist(
    lapply(
      after_command,
      function(command) {
        intersect(
          all.names(command),
          r_functions
        )
      }
    )
  )
)

queue <- reached

while (length(queue)) {
  called <- unique(
    unlist(
      lapply(
        queue,
        function(name) {
          calls_of(get(name, envir = globalenv()))
        }
      )
    )
  )
  queue <- setdiff(
    intersect(called, r_functions),
    reached
  )
  reached <- c(reached, queue)
}

missing_calls <- lapply(
  setNames(reached, reached),
  function(name) {
    called <- calls_of(get(name, envir = globalenv()))
    called[!vapply(called, exists, logical(1))]
  }
)

missing_calls <- missing_calls[lengths(missing_calls) > 0]

check(
  length(missing_calls) == 0,
  sprintf(
    "the %s functions in R/ that the pipeline reaches call only functions that exist",
    length(reached)
  ),
  missing_calls
)

###################
# summary
###################

cat(
  sprintf(
    "\ntargets before: %s | now: %s | carried over: %s | removed: %s | added: %s\n",
    nrow(before$manifest),
    nrow(after$manifest),
    nrow(carried),
    length(removed),
    length(added)
  )
)

will_build <- intersect(
  after$manifest$name,
  outdated_now
) |>
  intersect(model_targets)

cat(
  sprintf(
    "model targets that tar_make() will build: %s of %s\n",
    length(will_build),
    length(model_targets)
  )
)

saveRDS(
  list(
    checked = Sys.time(),
    carried = carried,
    removed = removed,
    added = added,
    edges_dropped = edges_dropped,
    edges_gained = edges_gained,
    outdated_now = outdated_now,
    failures = failures
  ),
  "outputs/overhaul_check/wiring_check.rds"
)

if (length(failures)) {
  cat(
    sprintf(
      "\n%s CHECK(S) FAILED\n",
      length(failures)
    )
  )
  quit(status = 1)
}

cat("\nall wiring checks passed\n")
