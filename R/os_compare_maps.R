# Where the offset-strength ("os") model's maps differ from the current model's, and
# whether the presence records support the change. Written 2026-10-19.
#
# Each model is cut at its own maxSSS threshold (`thresholds_maxsss`,
# `thresholds_maxsss_os`), on the masked probability (`pred_p`, `pred_p_os`). Three tables,
# per species, written as CSVs to `output_dir` and returned as tibbles:
#
#   presence_cells.csv   10 km cells with a presence record (that have a prediction):
#                        how many each model hides (below its threshold), how many the os
#                        model newly shows, and how many it newly hides
#   range_by_offset.csv  the area each model shows as present, by the level of the 2024
#                        mean offset (< 0.01, 0.01-0.1, 0.1-1, >= 1), next to the number
#                        of presence cells in each band. The lowest band is the
#                        Sahara-like one: the os model's range should grow there only
#                        where there are records
#   unmasked_low_offset.csv  the area over the threshold BEFORE the expert-range and
#                        bare-ground masks where the offset is < 0.01: how much the masks,
#                        rather than the model, keep each species out of the desert
#
# Areas are in km^2 (the grid is lon/lat, so cell areas come from terra::cellSize()).
os_compare_maps <- function(
    pred_p,
    pred_p_os,
    p_unmasked,
    p_unmasked_os,
    thresholds,
    thresholds_os,
    offset,
    model_data_records,
    target_species,
    output_dir
){

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  area_km2 <- terra::values(
    terra::cellSize(
      pred_p[[1]],
      unit = "km"
    )
  )[, 1]

  offset_v <- terra::values(offset)[, 1]

  offset_band <- cut(
    offset_v,
    breaks = c(0, 0.01, 0.1, 1, Inf),
    labels = c("< 0.01", "0.01-0.1", "0.1-1", ">= 1"),
    right = FALSE,
    include.lowest = TRUE
  )

  p_v <- terra::values(pred_p)
  p_os_v <- terra::values(pred_p_os)
  pu_v <- terra::values(p_unmasked)
  pu_os_v <- terra::values(p_unmasked_os)

  thr <- function(tab, sp) tab$threshold[match(sp, tab$species)]

  # presence cells: species x 10 km cell, from the records before any spatial filtering
  presences <- model_data_records |>
    filter(
      presence == 1,
      species %in% target_species
    )

  presence_cells <- presences |>
    mutate(
      cell = terra::cellFromXY(
        pred_p[[1]],
        as.matrix(pick(longitude, latitude))
      )
    ) |>
    filter(!is.na(cell)) |>
    distinct(species, cell)

  presence_cells <- presence_cells |>
    mutate(
      col = match(species, names(pred_p)),
      p_current = p_v[cbind(cell, col)],
      p_os = p_os_v[cbind(cell, col)],
      shown_current = p_current >= thr(thresholds, species),
      shown_os = p_os >= thr(thresholds_os, species),
      offset_band = offset_band[cell]
    ) |>
    filter(
      !is.na(p_current),
      !is.na(p_os)
    )

  presence_tbl <- presence_cells |>
    group_by(species) |>
    summarise(
      presence_cells = n(),
      hidden_current = sum(!shown_current),
      hidden_os = sum(!shown_os),
      newly_shown = sum(shown_os & !shown_current),
      newly_hidden = sum(!shown_os & shown_current),
      .groups = "drop"
    )

  range_tbl <- lapply(
    target_species,
    function(sp){

      col <- match(sp, names(pred_p))
      present_current <- p_v[, col] >= thr(thresholds, sp)
      present_os <- p_os_v[, col] >= thr(thresholds_os, sp)

      tibble::tibble(
        band = offset_band,
        area = area_km2,
        current = present_current,
        os = present_os
      ) |>
        filter(!is.na(band)) |>
        group_by(band) |>
        summarise(
          km2_current = sum(area[current %in% TRUE]),
          km2_os = sum(area[os %in% TRUE]),
          .groups = "drop"
        ) |>
        mutate(species = sp)

    }
  ) |>
    bind_rows() |>
    left_join(
      presence_cells |>
        count(
          species,
          band = offset_band,
          name = "presence_cells"
        ),
      by = c("species", "band")
    ) |>
    mutate(
      presence_cells = coalesce(presence_cells, 0L),
      km2_change = km2_os - km2_current
    ) |>
    select(
      species,
      band,
      presence_cells,
      km2_current,
      km2_os,
      km2_change
    )

  low <- which(offset_v < 0.01)

  unmasked_tbl <- lapply(
    target_species,
    function(sp){
      col <- match(sp, names(pred_p))
      tibble::tibble(
        species = sp,
        km2_offset_below_0.01 = sum(area_km2[low]),
        km2_over_threshold_current = sum(area_km2[low][pu_v[low, col] >= thr(thresholds, sp)], na.rm = TRUE),
        km2_over_threshold_os = sum(area_km2[low][pu_os_v[low, col] >= thr(thresholds_os, sp)], na.rm = TRUE),
        km2_over_threshold_masked_current = sum(area_km2[low][p_v[low, col] >= thr(thresholds, sp)], na.rm = TRUE),
        km2_over_threshold_masked_os = sum(area_km2[low][p_os_v[low, col] >= thr(thresholds_os, sp)], na.rm = TRUE)
      )
    }
  ) |>
    bind_rows()

  readr::write_csv(
    presence_tbl,
    file.path(output_dir, "presence_cells.csv")
  )

  readr::write_csv(
    range_tbl,
    file.path(output_dir, "range_by_offset.csv")
  )

  readr::write_csv(
    unmasked_tbl,
    file.path(output_dir, "unmasked_low_offset.csv")
  )

  list(
    presence_cells = presence_tbl,
    range_by_offset = range_tbl,
    unmasked_low_offset = unmasked_tbl
  )

}
