# Presence records that the plotted maps hide, by species, and what hides them.
#
#   Rscript extras/check_masked_presences.R
#
# Reads the store read-only (~1 min). Writes outputs/mask_check/masked_presences.csv, one
# row per species x 10 km cell with a presence record, and prints a summary.
#
# What stands between a presence record and the plotted maps:
#
#   Before the model. Records outside `project_mask_5` are dropped by get_spatial_values()
#   and never reach the fit: either outside the offset's area (`project_mask_5_outline`)
#   or in a 5 km cell `water_mask_5` calls water.
#
#   In the plotted maps. pred_lambda is
#       habitat x offset x expert x (1 - bare)
#   where habitat is the model's abundance without the offset
#   (`preds$lambda_no_offset`), offset is the An. gambiae mechanistic offset
#   (`offsets_avg_10`), expert is `expert_offset_maps_10` (1 inside the species' expert
#   map, falling to 0 at 1000 km; only the 8 species with a map, coluzzii taking
#   gambiae's) and bare is `landcover_bare_10`. pred_p is the same chain on the
#   probability scale. The offset is part of the model, not a mask, but for a species
#   whose climate differs from gambiae's it acts like one. The maps use its mean over
#   the last 12 months of the series (2024); `offset_record_months` is what the fit
#   used, at the records' own survey months.
#
# A presence cell is HIDDEN where the masked probability (pred_p) is below the species'
# maxSSS threshold, i.e. the binary range map shows it absent. Its cause is the factor
# furthest below its usual value, on the log scale, among those well below it: habitat
# or offset at least 10 times below their median over that species' presence cells,
# expert or (1 - bare) below 0.5. "habitat" means the model itself predicts the species
# poorly there; offset, expert and bare are what to look at. A hidden cell with none of
# these is "no single factor": a prediction just under the threshold. Separately, a cell
# is FADED where expert x (1 - bare) < 0.5 (the continuous maps cut by more than half).

suppressPackageStartupMessages({
  library(targets)
  library(terra)
  library(dplyr, warn.conflicts = FALSE)
  library(tidyr)
  library(readr)
})

outdir <- "outputs/mask_check"
dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)

target_species <- tar_read(target_species)
records <- tar_read(model_data_records)
outline <- tar_read(project_mask_5_outline)
project_mask <- tar_read(project_mask_5)
preds <- tar_read(preds)
p_raw <- rast(preds$p)
habitat <- rast(preds$lambda_no_offset)
pred_p <- tar_read(pred_p)
offset <- tar_read(offsets_avg_10)
expert <- tar_read(expert_offset_maps_10)
bare <- tar_read(landcover_bare_10)
dominant_area <- tar_read(expert_maps_buffer_1000)
thresholds <- tar_read(thresholds_maxsss)
countries <- tar_read(atlas_country_boundaries)
model_data <- tar_read(model_data_spatial_cx)

# the expert layer each species is multiplied by, as in add_expert_offset()
expert_layer <- function(sp) {
  case_when(
    sp %in% c("coluzzii", "gambiae") ~ "gambiae",
    sp %in% names(expert) ~ sp,
    .default = NA_character_
  )
}

presences <- records |>
  filter(
    presence == 1,
    species %in% target_species
  )

xy <- as.matrix(presences[, c("longitude", "latitude")])

presences <- presences |>
  mutate(
    in_outline = !is.na(terra::extract(outline, xy)[, 1]),
    in_project_mask = !is.na(terra::extract(project_mask, xy)[, 1]),
    cell_10 = cellFromXY(p_raw, xy),
    # off the 10 km grid (e.g. Mauritius, Reunion) there is no cell to group on
    group_key = if_else(
      is.na(cell_10),
      sprintf("%.4f|%.4f", latitude, longitude),
      as.character(cell_10)
    )
  )

cells <- presences |>
  group_by(
    species,
    group_key
  ) |>
  summarise(
    cell_10 = first(cell_10),
    latitude = mean(latitude),
    longitude = mean(longitude),
    n_records = n(),
    data_types = paste(sort(unique(data_type)), collapse = "+"),
    sources = paste(sort(unique(source_id)), collapse = " "),
    in_outline = any(in_outline),
    in_project_mask = any(in_project_mask),
    .groups = "drop"
  ) |>
  select(-group_key)

# the 50 atlas countries only; anything else (e.g. Morocco, Egypt) is "other"
cells$country <- terra::extract(
  countries,
  vect(
    cells,
    geom = c("longitude", "latitude"),
    crs = crs(countries)
  )
)$COUNTRY
cells$country[is.na(cells$country)] <- "other"

# the offset the fit used for these records, in their own survey months: if it is
# as low as the map's, the model never fitted them well either, and a longer average
# for the maps would not help
record_offset <- model_data |>
  filter(
    presence == 1,
    data_type != "bg"
  ) |>
  mutate(
    cell_10 = cellFromXY(
      p_raw,
      as.matrix(pick(longitude, latitude))
    )
  ) |>
  group_by(
    species,
    cell_10
  ) |>
  summarise(
    offset_record_months = median(offset),
    .groups = "drop"
  )

cells <- cells |>
  left_join(
    record_offset,
    by = c("species", "cell_10")
  )

lookup <- function(m, cell, col) {
  out <- rep(NA_real_, length(cell))
  ok <- !is.na(cell) & !is.na(col)
  out[ok] <- m[cbind(cell[ok], match(col[ok], colnames(m)))]
  out
}

p_raw_v <- values(p_raw)
habitat_v <- values(habitat)
pred_p_v <- values(pred_p)
offset_v <- values(offset)[, 1]
expert_v <- values(expert)
bare_v <- values(bare)[, 1]

dominant_r <- rasterize(
  dominant_area,
  p_raw[[1]]
)

cells <- cells |>
  mutate(
    expert_layer = expert_layer(species),
    habitat = lookup(habitat_v, cell_10, species),
    offset = offset_v[cell_10],
    expert_factor = if_else(
      is.na(expert_layer),
      1,
      lookup(expert_v, cell_10, expert_layer)
    ),
    bare_factor = 1 - bare_v[cell_10],
    mask_factor = expert_factor * bare_factor,
    p_unmasked = lookup(p_raw_v, cell_10, species),
    p_masked = lookup(pred_p_v, cell_10, species),
    in_dominant_expert_area = !is.na(values(dominant_r)[cell_10, 1])
  ) |>
  left_join(
    thresholds |>
      select(species, threshold),
    by = "species"
  ) |>
  group_by(species) |>
  mutate(
    # usual values at this species' presence cells (those with a prediction)
    habitat_ratio = habitat / median(habitat[in_project_mask], na.rm = TRUE),
    offset_ratio = offset / median(offset[in_project_mask], na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    hidden = p_masked < threshold,
    faded = mask_factor < 0.5,
    cause = {
      deficits <- cbind(
        habitat = if_else(habitat_ratio < 0.1, log(habitat_ratio), 0),
        offset = if_else(offset_ratio < 0.1, log(offset_ratio), 0),
        expert = if_else(expert_factor < 0.5, log(expert_factor), 0),
        bare = if_else(bare_factor < 0.5, log(bare_factor), 0)
      )
      deficits[is.na(deficits)] <- 0
      if_else(
        apply(deficits, 1, min) < 0,
        colnames(deficits)[max.col(-deficits, ties.method = "first")],
        "no single factor"
      )
    },
    status = case_when(
      !in_outline ~ "dropped: outside offset area",
      !in_project_mask ~ "dropped: water",
      is.na(p_unmasked) ~ "no 10 km prediction cell",
      hidden ~ paste("hidden:", cause),
      faded ~ "shown, faded by expert x bare",
      .default = "shown"
    )
  ) |>
  arrange(
    species,
    status,
    p_masked
  )

write_csv(
  cells,
  file.path(outdir, "masked_presences.csv")
)

summary_tbl <- cells |>
  group_by(species) |>
  summarise(
    cells = n(),
    drop_outside = sum(status == "dropped: outside offset area"),
    drop_water = sum(status == "dropped: water"),
    drop_water_land_in_10km = sum(status == "dropped: water" & !is.na(p_unmasked)),
    hidden_offset = sum(status == "hidden: offset"),
    hidden_expert = sum(status == "hidden: expert"),
    hidden_bare = sum(status == "hidden: bare"),
    hidden_habitat = sum(status == "hidden: habitat"),
    hidden_other = sum(status == "hidden: no single factor"),
    faded = sum(status == "shown, faded by expert x bare"),
    outside_dominant_area = sum(!in_dominant_expert_area & in_project_mask)
  )

cat("\nPresence cells (species x 10 km cell) by what happens to them on the maps\n\n")
print(
  summary_tbl,
  n = Inf,
  width = Inf
)

cat("\nHidden by the offset, expert range or bare ground, and dropped, by species:\n\n")
print(
  cells |>
    filter(
      grepl("dropped|offset|expert|bare", status)
    ) |>
    group_by(species, status) |>
    summarise(
      cells = n(),
      records = sum(n_records),
      lat = sprintf("%.0f to %.0f", min(latitude), max(latitude)),
      lon = sprintf("%.0f to %.0f", min(longitude), max(longitude)),
      offset_ratio = signif(median(offset_ratio), 2),
      mask_factor = signif(median(mask_factor), 2),
      sources = n_distinct(unlist(strsplit(sources, " "))),
      .groups = "drop"
    ),
  n = Inf,
  width = Inf
)

cat("\nHidden by the offset, by species and country:\n\n")
print(
  cells |>
    filter(status == "hidden: offset") |>
    group_by(species, country) |>
    summarise(
      cells = n(),
      records = sum(n_records),
      offset_ratio = signif(median(offset_ratio), 2),
      offset_map = signif(median(offset), 2),
      offset_record_months = signif(median(offset_record_months, na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    arrange(species, desc(cells)),
  n = Inf,
  width = Inf
)

cat(sprintf("\nwritten: %s\n", file.path(outdir, "masked_presences.csv")))
