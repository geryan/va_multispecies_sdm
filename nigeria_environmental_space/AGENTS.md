# Nigeria environmental space: notes for agents

## What this is

A small, self-contained set of data for Nigeria, 2013–2022:
- annual land-cover class proportions;
- annual human footprint;
- *Anopheles gambiae* s.s. presence records.

It was cut out of the Vector Atlas multispecies SDM repository (the parent
folder) to keep for separate work. Nothing in the parent project uses it.

Everything here can be read with `terra` and base R, with this folder as the
working directory. `plot_nigeria_environmental_space.R` reads every file and
makes simple maps. Citations for every source are in `README.md`.

**Provenance.** Made on 2026-10-19 by `extras/make_nigeria_environmental_space.R`
in the parent repository. That script reads the parent's `targets` store and
its processed rasters, so it can't be rerun outside that repository. Treat the
files here as the data, and don't try to rebuild them.

**Years.** 2013–2022 is the last ten years both raster series cover: land cover
runs 1992–2022 and footprint 2000–2024. The same data for Uganda, for all years,
are in `../uganda_environmental_space/`.

## Files

| File | Contents |
|---|---|
| `landcover_<class>_2013_2022.tif` | Land-cover class proportion (0–1). 11 files, 10 layers each (`<class>_<year>`). |
| `footprint_2013_2022.tif` | Human Footprint index (0–50), 10 layers (`footprint_<year>`). |
| `gambiae_records_nigeria_2013_2022.csv` | 51 records: `month`, `year`, `latitude`, `longitude`. |
| `nigeria_boundary.gpkg` | Nigeria, GADM 4.1 level 0 (attributes `GID_0`, `COUNTRY`). |
| `plot_nigeria_environmental_space.R` | Reads everything and draws quick-look plots. |
| `README.md` | Short description and citations. |

## The grid

- Every raster shares one grid: WGS 84 lon/lat (EPSG:4326), 0.04166665° cells
  (1/24°, about 4.6 km), 231 rows × 288 columns.
- Extent is x 2.667 to 14.667, y 4.250 to 13.875.
- Each layer carries its year on terra's time axis (`time(r)` gives 2013 to
  2022, `tstep = "years"`).
- Values are float32. Cells outside the boundary are NA.
- 5 km is the finest the source project holds for both data sets. Don't expect
  to find finer versions.

## Land cover

Source: ESA CCI / C3S annual land cover, 300 m. Product version 2.0.7 for 2013–2015
and 2.1.1 (C3S) for 2016–2022.

- Each 5 km cell holds the share of its 225 (15 × 15) source cells in each class.
  It is a proportion, not a majority class.
- Source cells of no data, snow and ice, or lichens and mosses are left out of
  both the numerator and the denominator. So the 11 classes sum to exactly 1 in
  every non-NA cell (checked for every year).
- The 11 classes, grouped from the ESA legend:

| Class | ESA classes |
|---|---|
| `crop_other` | rainfed cropland (all types), mosaic cropland |
| `irrigated` | irrigated cropland |
| `tree` | all tree cover, mosaic natural vegetation, mosaic tree and shrub |
| `shrubland` | shrubland (evergreen, deciduous) |
| `grassland` | grassland, mosaic herbaceous |
| `sparse` | sparse vegetation (tree, shrub, herbaceous) |
| `wetland` | tree, shrub or herbaceous cover flooded, fresh or brackish water |
| `mangrove` | tree cover flooded, saline water |
| `urban` | urban |
| `bare` | bare areas |
| `water` | water |

- **Nigerian means, 2022:** crop_other 0.55, tree 0.28, shrubland 0.087,
  grassland 0.030, water 0.014, urban 0.011, mangrove 0.0097, irrigated 0.0079,
  wetland 0.0046, sparse 0.0009, bare 0.0002.
- 43,830 non-NA cells, the same cells in every year and every class.
- Because the classes sum to 1, they are compositional. A model that uses all 11
  together as covariates has a singular design. Drop one class (the parent
  project drops `bare` and `sparse`) or use a log-ratio transform.

## Human footprint

Source: Mu et al. (2022), annual terrestrial Human Footprint, figshare version 8,
which runs to 2024 although the paper's title says 2018.

- These are raw index units, 0–50, not rescaled. The parent project divides by
  one constant across all years for its model: 49.785, the maximum over Africa
  and every year 2000–2024. Use one divisor across
  years if you rescale: scaling each year to its own maximum creates false
  trends.
- The source is 1 km in the Mollweide equal-area projection. It was reprojected
  to this grid by averaging (`terra::project(method = "average")`).
- **Nigerian values:** mean 16.0 in 2013 and 16.7 in 2022; range 2.4–44.1 in
  2013 and 2.8–45.0 in 2022.
- 43,595 non-NA cells, the same in every year: 235 fewer than land cover. The
  source has no values over water, and those 235 cells are mostly water (median
  water proportion 0.81).

## *An. gambiae* records

Source: the Vector Atlas data extract of 2026-06-16, as cleaned by the parent
project.

- **Species:** *An. gambiae* sensu stricto (records labelled "gambiae" or
  "gambiae_s form"). Not included: *An. coluzzii*, records identified only to the
  *gambiae* complex or s.l., and the combined "gambiae_coluzzii" label.
- **All 51 rows are presences.** 46 are presence-absence records marked present;
  5 are counts above zero. No absences are included. The parent model's inferred
  zeros are left out, and Nigeria had only one recorded absence of *gambiae* in
  any year. So use the records as presence-only data, with background points or
  a bias model, not as presence-absence.
- **Rows are not independent.** There are 23 distinct locations and 25 distinct
  location × month combinations. One site (8.45 N, 4.633 E, March 2017) has 13
  rows from one source: 1 house resting indoors and 12 "other" methods. Remove
  duplicates first if each site and time should count once.
- **Dates are approximate.** `month` and `year` are the midpoint of the survey.
  49 of the 51 records come from surveys longer than a month (median 11.6
  months), so the month is rarely the month of collection. Matching on year is
  safer than matching on month.
- **Records by year:** 2013 4, 2014 4, 2016 14, 2017 19, 2018 8, 2020 2; none in
  2015, 2019, 2021 or 2022. They come from 8 sources.
- **Sampling methods** (not in the CSV): 21 larval, 18 other, 9 house resting
  indoors, 3 odour-baited.
- **Coordinates** are as recorded, and their precision varies. Some are coarse,
  e.g. (7.0, 3.0), so a few may be centroids rather than sites.
- Only records inside the boundary were kept. None fell within 5 km outside it,
  so the boundary dropped nothing near the border.
- One record (6.41581 N, 2.8835 E, May 2016, near the Benin border on the coast)
  falls on a cell with no footprint value. Land cover has a value at every
  record.

## Working here

- Use `terra` for the rasters: `rast()` on one file gives all its years as
  layers, and `r[[time(r) == 2020]]` picks one year.
- Read the boundary with `vect("nigeria_boundary.gpkg")`.
- Read the records with `read.csv()` and turn them into points with
  `vect(records, geom = c("longitude", "latitude"), crs = "EPSG:4326")`.
- Don't overwrite the data files. Write any new outputs to a subfolder.
