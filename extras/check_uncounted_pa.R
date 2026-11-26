# Smoke test: the main fit builds and samples once the present-but-uncounted records
# and their location-dates' inferred zeros are presence-absence (2026-11-06).
# Patches the stored model_data_spatial_cx in memory (nearly the same change as the
# edited generate_model_data_records(); misses 2 location-dates with an uncounted
# absence). Reads the real store read-only; writes only the image path given.
#
#   Rscript extras/check_uncounted_pa.R <image>   (from the project root; about 1 min)

args <- commandArgs(trailingOnly = TRUE)
image <- args[1]

pkgs <- c(
  "dplyr", "sdmtools", "readr", "tidyr", "terra", "ggplot2", "geotargets",
  "idpalette", "tidyterra", "geodata", "coda", "greta", "DHARMa", "lubridate",
  "magrittr", "stringr", "bayesplot", "patchwork", "MCMCvis", "bssdm"
)

suppressPackageStartupMessages({
  library(targets)
  for (p in pkgs) library(p, character.only = TRUE)
})

tar_source(files = "R")

ok <- function(cond, msg) {
  cat(sprintf("%-4s %s\n", if (isTRUE(cond)) "ok" else "FAIL", msg))
  if (!isTRUE(cond)) failures <<- c(failures, msg)
}
failures <- character()

key <- c(
  "source_id",
  "start_date",
  "sampling_method",
  "sampling_method_detailed",
  "latitude",
  "longitude",
  "study_months"
)

old <- tar_read(model_data_spatial_cx)

partial <- old |>
  filter(
    data_type == "count",
    presence == 1,
    count == 0
  ) |>
  distinct(
    across(all_of(key))
  ) |>
  mutate(
    partial = TRUE
  )

new <- old |>
  left_join(
    partial,
    by = key
  ) |>
  mutate(
    move = data_type == "count" &
      partial %in% TRUE &
      (inferred %in% TRUE | (presence == 1 & count == 0)),
    data_type = if_else(move, "pa", data_type),
    count = if_else(move, NA_real_, count),
    n = if_else(move, presence, n)
  ) |>
  select(
    -partial,
    -move
  )

cat(
  sprintf(
    "moved: %d presences, %d inferred zeros\n",
    sum(old$data_type != new$data_type & old$presence == 1),
    sum(old$data_type != new$data_type & old$presence == 0)
  )
)

ok(
  nrow(new) == nrow(old),
  "same number of rows"
)
ok(
  sum(new$data_type == "count" & new$presence == 1 & new$count == 0, na.rm = TRUE) == 0,
  "no present count row with count 0"
)

set.seed(1)
t0 <- Sys.time()

fit_model_multispecies_pp_count(
  model_data_spatial = new,
  target_covariate_names = tar_read(target_covariate_names),
  target_species = tar_read(target_species),
  bioregion_names = tar_read(bioregion_names),
  image_name = image,
  n_burnin = 20,
  n_samples = 20,
  n_chains = 2,
  n_cores = 2
)

cat(
  sprintf(
    "fit: %.1f min\n",
    as.numeric(difftime(Sys.time(), t0, units = "mins"))
  )
)

e <- new.env()
load(image, envir = e)

n_count <- sum(new$data_type == "count")
n_pa <- sum(new$data_type == "pa")

ok(
  length(e$count_data_response) == n_count,
  sprintf("count response has the %d count rows", n_count)
)
ok(
  length(e$pa_data_response) == n_pa,
  sprintf("PA response has the %d PA rows", n_pa)
)

# the stored data values; calculate() would simulate the response instead
pa_y <- as.numeric(greta:::get_node(e$pa_data_response)$value())
ok(
  all(pa_y %in% c(0, 1)) &&
    sum(pa_y) == sum(new$presence[new$data_type == "pa"]),
  "PA response is 0/1 and its presences match"
)

ok(
  inherits(e$draws, "mcmc.list") &&
    length(e$draws) == 2,
  "draws: 2 chains"
)

v <- as.matrix(e$draws)
ok(
  all(is.finite(v)),
  sprintf("all %d draws x %d parameters finite", nrow(v), ncol(v))
)

cat(
  if (length(failures) == 0) "\nALL CHECKS PASS\n" else "\nFAILURES\n"
)
