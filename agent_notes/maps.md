# Prediction maps, masks and expert ranges

Read when working on `pred_p`, `pred_lambda`, `pred_cv`, masks, expert range
maps, thresholds, dominant species or the abundance cubes.

## From predictions to maps

- `preds` (raw rasters `outputs/rasters/multispecies_pp{,_p,_p_cv}.tif`, with
  their md5s) → the masked `pred_p`, `pred_lambda`, `pred_cv`
  (`mask_landcover_and_expert_offset()`, `add_expert_offset()`): scaled by the
  expert offset (`expert_offset_maps_10`) and by (1 − bare).
- **Expert maps reach only the masked maps** (`expert_offset_maps` → `_10` →
  `pred_p`, `pred_lambda`, `pred_cv`, `abundance_cubes`), never the fit
  (checked with `tar_network()`, 2026-11-23). Changing one rebuilds 1–2 h of
  targets, not the fit.
- **`pred_cv` is scaled down** by the expert offset and (1 − bare), which makes
  uncertainty look lower near range edges. It should probably be masked
  instead (open).
- Thresholds: maxSSS per species (`thresholds_maxsss`, `maxsss_thresholds()`,
  identical to `PresenceAbsence::optimal.thresholds()`) → `pred_binary`.
- Dominant species: `pred_dominant`, `pred_dominant_001`, and the `_expert`
  versions masked to `expert_maps_buffer_1000`. After the geotargets round trip
  the category ID column is `value`, not `ID`: index `levels()` by position.
- RGB composition of the main vectors (`rel_abund_rgb`): red arabiensis, green
  gambiae + coluzzii, blue funestus.

## Abundance cubes (`abundance_cubes`)

- Monthly relative abundance, one file per species with one layer per month,
  for the last 24 months of the offset series
  (`outputs/rasters/abundance_cubes_masked/`). It is the model's abundance
  without the offset, scaled by the expert offset and (1 − bare) as
  `pred_lambda` is, times each month's offset, so the mean of a species' last 12
  layers is its `pred_lambda` layer.
- Country crops: `extras/crop_abundance_cubes_to_countries.R` (the user's,
  untracked) was run 2026-11-11 for MOZ and AGO →
  `outputs/rasters/countries/<ISO3>/abundance_cubes/` (18 cubes each) plus a
  hand-made `abundance_cubes.zip`. One-off copies: a refit won't update them.

## Expert range maps

- `sinka_expert_maps` + `pharoensis_expert_map` + `coluzzii_expert_map` →
  `expert_maps` (`add_expert_map()`). `expert_maps_buffer_1000` is all maps
  dissolved plus 1000 km. `expert_offset_maps` takes ~30–40 min (31 min on
  2026-11-23); the side target `expert_offset_maps_500` as long again.
- `add_expert_offset()` sends `gambiae_coluzzii` and `gambiae_complex` to the
  gambiae (Sinka) layer. coluzzii has its own map since 2026-11-23.
- `make_expert_offset_maps()` run on one species alone names its layer
  `layer`. With several species, as in the pipeline, the layers are named
  correctly; `names(tar_read(expert_offset_maps_10))` includes `coluzzii`
  (checked 2026-11-24, after the build).

### Coluzzii (Wiebe et al. 2017; built 2026-11-23)

- The area between the solid range line and the west African coast in Wiebe et
  al. (2017) Malar J 16:85, Fig. 1a, mainland only (the user's choice: Bioko,
  São Tomé and Príncipe left out). 8.68 M km², 17.5°W–29.4°E, 16.3°S–20.0°N.
- Digitised by `extras/digitise_wiebe2017_coluzzii.R` (~30 s, run from the
  project root) from the 784 px web figure
  (`data/vector/other_expert_maps/wiebe2017/wiebe2017_fig1.png`; Springer and
  PMC block automated downloads of the full-size one), so good to about
  ±25 km. The figure is plain lon/lat; the GADM borders fit it with a median
  error of 0.14 px. Check figure: `coluzzii_check.png` next to the shapefile.
- Plumbing: `coluzzii_expert_map_file` (format = "file", tracks the `.shp`) →
  `coluzzii_expert_map` (`get_coluzzii_expert_map()`, simplified at 0.05° as for
  pharoensis: 296,542 → 812 vertices, area −0.006%) → `expert_maps`.
- `data/` is git-ignored: copy `data/vector/other_expert_maps/wiebe2017/` to
  Spartan before building there.
- **Effect** (tested on stored objects before the build): the other 17 species'
  `pred_p` unchanged (≤ 3e-8) and `expert_maps_buffer_1000` unchanged in area.
  coluzzii's `pred_p` changes by up to 0.72, only outside the Wiebe range (East
  Africa, Ethiopia, southern Africa and Madagascar go to ~0). Its maxSSS
  threshold drops 0.575 → 0.376 (sensitivity 0.78 → 0.93, specificity
  0.85 → 0.73), so the area over threshold grows 2.26 → 4.57 M km² (mostly DRC
  +645,000 km², Chad, Nigeria, Guinea, Niger, Congo, Gabon; 2.24 at the old
  threshold).
- 91 coluzzii presence records (38 locations) fall outside the polygon: the
  Gulf of Guinea islands, coastal points, and 16 at one site in Katanga, DRC
  (26.3°E, 10.6°S). All are within the 1000 km taper, so none gets an expert
  factor below 0.5.
- coluzzii now drops out of the hulls (`convex_hulls` etc.), as pharoensis does.

## Presence records the maps hide

`extras/check_masked_presences.R` → `outputs/mask_check/masked_presences.csv`
(2026-10-19, on the 2026-10-16 fit, before the coluzzii map; not rerun). 4,471
species × 10 km presence cells; "hidden" = below the species' maxSSS threshold
on `pred_p`.

- The expert-range masks hide none (expert factor ≥ 0.57 at every hidden cell).
  Bare ground hides 4 (arabiensis 2, coluzzii 2).
- **The *An. gambiae* offset hides 127 cells in 12 species**, where it is at
  least 10× below the species' usual: arid margins (Sudan, Mauritania, Niger,
  Botswana, Namibia), highlands (Ethiopia, Kenya, Rwanda) and North Africa
  (pharoensis in Egypt, 55 records; ziemanni in Morocco). Most: arabiensis 40,
  pharoensis 20, ziemanni 17, funestus 15. Not a 2024 artefact: the records' own
  survey months had an even lower offset (median 0.0043, against 0.9 at other
  presences), so the fit never fitted them either. 23 of the 127 have a 2024
  offset below 0.01 (the Nile valley, some Ethiopian highland cells, part of
  Morocco).
- **A per-species power κ on the offset** (scratch calculation, 2026-10-19, on
  the 2026-10-16 fit with habitat fixed and each species re-levelled; maps for 7
  species in `outputs/kappa_sahara_preview/`): the bare-ground mask, not the
  offset, blanks the Sahara (96% of a 17–30°N, 13°W–34°E box is over 90% bare).
  At κ = 0.5 the box gains 7 cells over arabiensis' threshold and 19 for
  coluzzii (all 17–20°N), and pharoensis 148 (mostly the Nile valley). Before
  the masks, arabiensis' median Sahara probability goes 0.0002 → 0.016
  (threshold 0.59); coluzzii is the exception, with 13% of the box over its
  threshold before masking. The larger change is outside the Sahara:
  arabiensis' binary range spreads over the Horn of Africa and southern Africa
  at κ ≤ 0.5. Fitted as the os model, κ went to ~0 (`os_model.md`).
- **The water mask drops 129 presence cells (402 records) before the model:**
  coastal and lakeshore records in a 5 km cell `water_mask_5` calls water
  (gambiae 28 cells, coluzzii 22, melas 22, arabiensis 19; 58 have land in the
  same 10 km cell). The user does not consider these a worry (2026-10-19):
  leave them.
- 13 cells are off the offset's area altogether: arabiensis on Réunion (10
  cells, 78 records) and Cabo Verde (2), and one coluzzii on the Cameroon coast.
