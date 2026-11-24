# Spatial inputs and covariates

Read when working on the covariate layers, how records get their covariate
values, or the prediction years. Expert range maps are in `maps.md`.

## Layers

- **Mechanistic *An. gambiae* offset**, monthly (`offsets_raw`; its path is
  chosen by the `user_is_*` flags, which differ between the laptop and Spartan).
  Matched to records on year-month. Never 0 (median 1e-4 in the Sahara). Maps
  use the 2024 mean (`offsets_avg_10`); the abundance cubes use the last 24
  months.
- **ESA CCI land-cover class proportions**, annual 1992–2022
  (`esa_landcover_proportion_year` → `esa_landcover_proportion`), 11 groups from
  `esa_landcover_group_defs()`. The model uses 9 (`model_landcover_classes`):
  `bare` and `sparse` are left out because the classes sum to 1 (the user's
  decision, 2026-09-03, after first keeping `sparse`: don't reinstate it).
- **Mu et al. human footprint**, annual 2000–2024 (`mu_hfp_*` targets →
  `footprint_5`).
- `prox_to_sea`; One Earth bioregions (fractional, 17).
- **Travel time** from research facilities: `bias_tt_raw` in minutes;
  `bias_tt_5` is the reversed 0–1 version the model uses. No data for some
  island states (Cabo Verde, Seychelles); Cabo Verde is also outside the
  prediction grid.
- `target_covariate_names` = `model_landcover_classes` + footprint +
  `prox_to_sea`: one list picks both the raster layers and the data columns.
- **In the chain but unused by the model:** the soil layers (`soiltype_*`;
  `soiltype_names` is `NULL`), `subrealm_layers` and `subrealm_names`,
  `record_data_spatial_subsample` (a pass-through), `offset_names` (`NULL`).
  Removing them changes `covariate_rast_5_all` and invalidates everything below
  it, so do it only alongside a planned refit.

## ESA land-cover proportions

- **Built from the CDS zips** (`esa_landcover_zip`,
  `data/raster/esa_landcover/*.zip`, ~14 GB), not from `esa_landcover_year`,
  which is mode-resampled (the sub-grid mix is already gone). Don't delete the
  zips: they are the live target's files, and losing them redownloads and
  invalidates the land-cover chain and the fit.
- Each 5 km cell holds the share of its 225 source 300 m cells in each class.
  no_data, snow_and_ice and lichens_and_mosses are left out of numerator and
  denominator, so the classes sum to 1 and a cell with no usable cover is NA in
  every class. `mosaic_natural_vegetation` counts as tree (the user's call).
  Shrubland was split from grassland on 2026-09-03 (f93a0ee), giving 11 groups.
  Keep the sum-to-1 assertion in `proportion_esa_landcover()`: it catches NA
  not surviving the GDAL warp.
- 58 s and 2.2 GB per year, ~30 min for the series. The v2.0.7 (1992–2015) and
  v2.1.1 (2016–2022) legends are byte-identical, despite the warning in
  `_targets.R`.
- `landcover_bare` is the ESA 2022 bare proportion
  (`esa_landcover_model_layers(classes = "bare")`); the maps are multiplied by
  (1 − bare). `landcover_raw` (WorldCover) is kept only because `water_mask_5`,
  and so `project_mask_5`, are built from it.
- ESA and WorldCover classes are not interchangeable (r between products: tree
  0.41, crop_other/cropland 0.25, urban/built 0.93), so coefficients are not
  comparable across the 2026-09-03 swap.

## Matching records to covariates

- `get_spatial_values()` gives records the static layers; `match_offset_data()`
  (year-month), `match_landcover_data()` and `match_footprint_data()` (year)
  then replace them with each record's own date's values (`replace = TRUE`).
  Land-cover years are resolved from the stack's own `terra::time()` labels
  (`extract_year_indexed_layer_data()`).
- **Land cover:** before 1992 clamps to 1992; undated rows (every background
  point) take 2022 (`landcover_prediction_year`). **Footprint:** before 2000
  clamps to 2000 (21,075 dated records, ~20%); undated take 2024
  (`footprint_prediction_year`). The `landcover_year` and `footprint_year`
  columns record what was used.
- **Open, never answered:** background points sit on 2022 land cover and 2024
  footprint, while the presence-only records they are quadrature for span
  1980–2021. One argument (`na_year`) changes it.
- The prediction surface is not one instant: 2022 land cover (where ESA CCI
  ends) with 2024 footprint (the user's choice).

## Mu footprint traps

- figshare item 16571064 holds 2000–2024 (version 8), not the 2000–2018 its
  title says. `GET /articles/{id}/files` paginates at 10 without warning; use
  `/articles/{id}/versions/{v}`, which also pins the version.
- Scale the whole series by one divisor (49.7854690551758, stored as the
  GeoTIFF tag `hfp_scale_divisor`). `scale_rast_to_1()` per layer gives each
  year its own divisor and fakes a trend.
- `download_mu_hfp()` keeps neither the zips nor the global tifs (the user's
  requirement: the target must not invalidate when the sources are gone) and
  short-circuits on a valid existing output, with no network call. Changing
  `project_mask_5` therefore re-downloads 11 GiB (`keep_source = TRUE` avoids
  it).
- `mu_hfp_manifest` must hold nothing clock-dependent: a `Sys.Date()` would
  re-download daily.
- `terra::time()` returns a bare numeric when it was set with
  `tstep = "years"`; `hfp_layer_years()` handles both forms.
- Reprojection: crop in Mollweide to the densified mask outline, then
  `project(method = "average")`; 887 coastal cells come out NA each year and are
  filled with `fill_na_with_nearest_mean()`.
- The old static geodata footprint and Mu 2024 correlate at only 0.77 on the
  records, so coefficients are not comparable across that swap either.
- Source details: `extras/mu_hfp_manifest.md`, `data/raw/mu_hfp/README.md`.

## Raster checks

terra's `resample(method = "average" / "mode")` gives wrong values on tiny
target grids (≤ 4 cells across), so don't validate raster code on toy grids;
use ≥ 60 × 60 or compare with `aggregate()`.
