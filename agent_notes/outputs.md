# Outputs, figures and atlases

Read when working on plots, the atlases, output paths, side targets, disk
space or the country extracts.

## Paths

- fit image `outputs/images/model_fit.RData`
- raw prediction rasters `outputs/rasters/multispecies_pp{,_p,_p_cv}.tif`
- abundance cubes `outputs/rasters/abundance_cubes_masked/`
- plots `outputs/figures/distribution_plots/distn_<run_tag>/`, including
  `species_atlas.pdf`, `country_atlas.pdf`, `country_pages/<ISO3>/`,
  `country_atlases/<ISO3>_<Country>.pdf`, `response_curves/`,
  `bioregion_multipliers/`
- validation `outputs/figures/validation/<run_tag>/`
- country rasters `outputs/rasters/countries/<ISO3>/`
- coefficient draws `outputs/draws/coef_draws.rds`; tables `outputs/tables/`

They come from five targets: `run_tag` (dates the plot and validation
folders), `fit_image`, `pred_prefix`, `validation_dir`, `plot_dir`. The fit
depends on `fit_image` alone, so a new `run_tag` does not refit. `run_tag` is
still `20261014`, so every refit since has overwritten that run's folders.

## Plot targets

- Per-species maps; panels (3×6 grid; featured landscape and portrait; since
  2026-11-18 the four main vectors alone, 2×2, probability only,
  `plot_pred_panel_main_vectors`); dominant species (also masked to
  `expert_maps_buffer_1000`); maxSSS binary ranges (`thresholds_maxsss`,
  `pred_binary`); the RGB main-vector map (`rel_abund_rgb`).
- `coef_draws`: 1,000 coefficient draws pulled out of the fit image once
  (`extract_cv_draws()`), so the plots below never load the image.
- **Response curves** (`plot_response_curves`,
  `R/make_response_curve_plots.R`): one greyscale figure per species from the
  main effects, which are in effect the average bioregion. For land cover:
  relative abundance when a class is 10 percentage points higher than the
  average surveyed location, every other class (bare and sparse included)
  scaled down by the same factor so the total stays 1. The subtitle and heading
  wording are the user's. The user dropped the all-species-per-covariate
  versions as useless.
- **Bioregion multipliers** (`plot_bioregion_multipliers`,
  `R/make_bioregion_multiplier_plots.R`): per species, each bioregion's
  multiplier at its average covariates, with 90% intervals and record counts,
  greyscale; plus the multiplier maps (`bioregion_effect`). Why there are no
  slope figures: `model.md`.
- **Data and covariate figures** (outside the model set):
  `plot_records_traveltime` (records next to travel time) and
  `plot_covariate_panels`, which makes four figures in `outputs/figures/`:
  `covariate_panels_landscape.png` (5×3) and `covariate_panels_portrait.png`
  (A4; thinned, 300 dpi), and since 2026-11-06, through
  `make_covariate_panels(panels = )`, `covariate_panels_bioregions.png` (key on
  the right, no title, 16 × 11 cm) and `covariate_panels_covariates.png` (3 rows
  × 4, colour bar in the last cell, 16 × 13.1 cm), both 7 pt text at printed
  size, `maxcell = Inf` and 600 dpi. The new arguments (`panels`, `maxcell`,
  `dpi`, `width`, `height`) default to the old behaviour.
- **Continental PNGs are thinned:** `distplot()` defaults to tidyterra's
  500,000-cell cap and the 10 km grid has 732,511 cells. Defaulting to `Inf` was
  offered, not decided.
- **No rootograms are drawn:** `rootgram_data()` fails with "subscript out of
  bounds" (`yrep_matrix[, 1:ncol_yrep]`), and `try()` in
  `validation_and_checking()` hides it. It predates the 2026-10-14 overhaul.

## Atlases

- The species atlas PDF; per-country GeoTIFFs and the country atlas PDF for 50
  countries (WHO AFRO plus Sudan, Somalia and Djibouti). The country atlas
  opens with a continental "Africa" section (`species_atlas_pages` +
  `traveltime_page`).
- **Rendering:** pages are `cairo_pdf`s with the map embedded as an image, one
  pixel per 10 km cell. PDF viewers (Preview, browsers) smooth small images
  whatever the no-interpolate flag says, so small countries blurred.
  `sharpen_raster_grobs()` (`R/raster_for_pdf.R`) enlarges the built image by
  pixel repetition before saving: pixel-identical, +0.3–0.8 s per page against
  ~2.5 s for disaggregating the cells. `raster_for_pdf()` averages down layers
  too big for the page (the ~1 km travel time). tidyterra thins anything over
  500,000 cells, so the atlas draws with `maxcell = Inf`.
- **Cover, footer and citations** (`make_atlas_pdf()`, all three atlas targets;
  2026-10-16, cfaf971): the cover has the Vector Atlas logo, title, subtitle and
  "Please cite as" with two citations between green rules: Vector Atlas (2026),
  and Ryan, Sinka & Golding (2026) on Figshare, doi 10.26188/34044441. Every
  later page's footer has a small logo, the same citations, the "Contents" link
  and the page number; both URLs are clickable; the contents starts on page 2.
  The citations are the `citations` argument's default, written as LaTeX.
- **Logo:** `data/vector-atlas-logo.pdf` (the `logo` argument), made from the
  SVG with `rsvg-convert -f pdf` (xelatex can't include an SVG; 5.7 KB against
  the PNG's 104 KB; embedded once and reused). `data/` is git-ignored, so copy
  the logo to Spartan; targets doesn't track it, so a new logo needs the atlases
  invalidated by hand.
- **Open:** the cover subtitle (in `_targets.R`) still reads "Reparameterised
  multispecies model, contradicted inferred zeros removed, run …", and
  *Anopheles* in the title is not italic.

## Side targets (listed for the user to decide on, not removed)

None feeds the model, the cubes or the CV.

- *The hull and range-offset chain:* `bioregions_v`, `convex_hulls`,
  `bioregion_hulls`, `point_hulls_*`, `convex_offset_maps` (~18 min),
  `bioregion_offset_maps` (~38 min), `range_offsets`; also `bioregions_all`,
  `species_unique_presence_location_records`, `mod_dat_pts`.
- *Nothing downstream:* the ten `africa_mess*` / `africa_exdet*` novelty maps
  (on the pre-`_cx` data); `bg_points`; `bioregion_names_mask` →
  `bioregion_mask_5` → `bioregion_mask_10` → `landscape_mask_10`; `offsets_10`,
  `offsets_avg_p_5`, `offsets_avg_p_10`; `expert_offset_maps_500` →
  `expert_offset_maps_10_500`; `esa_landcover_year` → `esa_landcover_all`, and
  `esa_landcover_lookup`; `bioregion_stack`; `west_africa_extent`, `nga`, `cod`,
  `kentzauga`, `ken`, `uga`; `exploration_full_data_records`;
  `presences_outside_expert_range` (+ `_plot`);
  `species_unique_location_records` (+ `_presence_records`);
  `data_summary_table`, `pa_data_table`, `covs_plots`, `bias_offset_plots`,
  `simple_point_plots`.
- *Unused by the model but in its chain* (soil, subrealm and others):
  `covariates.md`. Removing those invalidates the fit.
- **`africa_mess_br` and `africa_exdet_br` fail now and then** with
  `[writeRaster] too many values for writing: 5853242 > 2926621` (exactly 2 ×
  ncell). A terra bug, seen in 1.9.40: calling `writeValues()` more than once
  into an in-memory raster doubles the buffer, and whether `bssdm::mess()` /
  `exdet()` write in more than one block depends on free RAM, so it is
  intermittent and worse inside a pipeline run. `R/mess_safe.R` (MESS through
  bssdm's data.frame method) avoids it for MESS but is not wired in; exdet has
  no safe version. Closest upstream report: rspatial/terra#1692 (closed). State 2026-11-24: `africa_mess_br` built 2026-11-23;
  `africa_exdet_br` errored 2026-11-09 and is the only outdated target.

## Disk and old outputs

- **Cleanup of 2026-11-23** (project 82 → 47 GB), at the user's request; none
  was tracked by a current target. Deleted: the old fit images
  `outputs/images/model_fit_test_source_re_rep{,_cx,_noinf}.RData` and
  `model_fit_test_source_re_rep.RData` in the root (the `_rep_cx` image was the
  only other draw of the background methods, so that comparison is no longer
  possible); `outputs/rasters/abundance_cubes/` (old unmasked cubes);
  `outputs/rasters/countries_rep_cx/`;
  `outputs/figures/distribution_plots/distn_20260928_rep_cx/`;
  `outputs/model_layers_mech.tif`; `data/raster/mechmask_africa_covs_std.tif`;
  `outputs/predstemp/`; `extras/sre_*_draws.rds` (so the `extras/sre_*` scripts
  that read them can't rerun); a 2024 RStudio suspended session in
  `.Rproj.user/`.
- **Kept, unreferenced, the user's call:** `data/raster/landcover_raw.tif`
  (2.3 GB; the target of that name lives in `_targets/objects`), `data/covs.tif`
  (1.2 GB), `outputs/images/model_fit_os.RData` (6.5 GB, os decision pending),
  `outputs/rasters/multispecies_pp_os*.tif`, `data/raster/map_covs/*.zip`
  (1.8 GB), older `distribution_plots/` runs, and older rasters in
  `outputs/rasters/` (`reparam_*`, `multisp_pp_lambda_no_offset_*`, `p_8spp_*`,
  `spartan_*`, `via_spartan/`, `archive_plots_*`,
  `20251029_dominant_simple_covs/`).
- **Don't delete** `data/raster/esa_landcover/*.zip` (14 GB): the files of the
  live `esa_landcover_zip` target.
- `outputs/overhaul_check/duplicate_outputs.csv` predates the cleanup; most of
  its large rows are gone. The `_os` targets still have store entries
  (`tar_prune()` not run).

## Session artefacts (git-ignored)

`outputs/session_20261001/` (test renders, bioregion prototypes, final test
figures, `abundance_drivers.txt` and its scripts, `response_review/`);
`outputs/overhaul_check/`; `outputs/greta_issue/` (greta reprex and issue text,
not filed); `outputs/model_comparison/` (os); `outputs/mask_check/`;
`outputs/kappa_sahara_preview/`. `extras/plot_sources_by_country.R` is the
user's, untracked.

## Country data extracts (not part of this project)

`nigeria_environmental_space/` (2026-10-19, by
`extras/make_nigeria_environmental_space.R`; TerraClimate climate added
2026-11-24 by `extras/make_nigeria_terraclimate.R`, a one-off unlikely to be
revisited) and `uganda_environmental_space/` (2026-10-23, by
`extras/make_uganda_environmental_space.R`). Covariates cut out for the user's
other work; nothing in the pipeline reads them. Don't extend, regenerate or
wire them in unless asked. Each has a `README.md` (Nigeria also an `AGENTS.md`).
They are cropped and masked to GADM 4.1 level 0 (the `nga` and `uga` targets)
on the 5 km grid, read from `outputs/raster/` and the store: the finest the
project holds (the raw 1 km footprint was not
kept, and the 300 m ESA source holds one class per cell, not proportions).
`.tif` and `.csv` files are git-ignored, and each folder's `.gitignore` ignores
its boundary `.gpkg`. The original scripts and notes were committed on
2026-11-06 (2becaa2, 6712720).
