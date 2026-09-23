# Posit Assistant Context: Vector Atlas Multispecies SDM

## Project Overview

This is a Bayesian species distribution modeling (SDM) project for multiple *Anopheles* mosquito species (malaria vectors) across Africa. The project integrates presence-only records, presence-absence surveys, and count data into a multispecies point process model to predict vector distributions and relative abundance using environmental covariates and mechanistic offsets.

**Target species** (18 *Anopheles* species, from `target_spp()`):  
*arabiensis*, *coluzzii*, *coustani*, *funestus*, *gambiae*, *leesoni*, *maculipalpis*, *melas*, *merus*, *moucheti*, *nili*, *pharoensis*, *pretoriensis*, *quadriannulatus*, *rivulorum*, *rufipes*, *squamosus*, *ziemanni*

## Data

### Primary sources
- **Vector Atlas (VA) records** (`data/tabular/va.data_*.csv`): ~250,000+ cleaned occurrence and count observations from field surveys, with metadata including species, sampling method, location, date, and literature source
- **Source metadata** (`ars_sources`): Literature citations and study metadata for all VA records
- **Processed datasets** (`data/processed/`): Cleaned, indexed, and spatially matched data ready for modeling

### Spatial covariates (5km resolution, 10km for predictions)

Two of these are **time-varying**: each record takes the value for its own year,
while the prediction surface is a single year held in a raster. The same data
therefore wears two faces, and the record side is attached in
`model_data_spatial` rather than by `get_spatial_values()`.

- **Landcover** (ESA CCI / C3S, annual 1992–2022, class *proportions* not
  majority class): `crop_other`, `irrigated`, `tree`, `shrubland`, `grassland`,
  `wetland`, `mangrove`, `urban`, `water`. `bare` and `sparse` are deliberately
  excluded — the full set sums to exactly 1 and so is collinear with the
  per-species intercept. Matched per record year by `match_landcover_data()`;
  predicted at `landcover_prediction_year` (2022). Replaced the static
  WorldCover classes.
- **Human footprint** (Mu et al. 2022, annual 2000–2024, native Mollweide 1 km):
  matched per record year by `match_footprint_data()`; predicted at
  `footprint_prediction_year` (2024). Replaced the static `geodata` 2009 layer.
  See `data/raw/mu_hfp/README.md` and `extras/mu_hfp_manifest.md`.
- **Proximity**: distance to sea (`prox_to_sea`)
- **Bioregions** (One Earth): 24 smoothed bioregion indicators, 17 of which
  enter the design (`bioregion_names` drops the rest), plus 5 subrealms. These
  are fractional, not 0/1.
- **Soil** (ISRIC SoilGrids): clay and silt content — built, but **not currently
  model covariates**: `soiltype_names` is `NULL`, so `subset_covariate_rast()`
  drops them.
- **Sampling bias offset**: travel time from research facilities
- **Mechanistic offsets**: environmental suitability indices from mosquito mechanistic models (300+ monthly/seasonal layers), matched on year-month
- **Expert ranges**: historical species range maps (Sinka et al. 2010) used as prior information

The model design is `target_covariate_names` — the 9 landcover classes plus
`footprint` and `prox_to_sea` — interacted with the bioregion layers.

## Workflow

The project uses the `targets` R package for reproducible pipeline management (`_targets.R`):
- Automated dependency tracking and caching
- Data processing, covariate indexing, model fitting, and prediction steps
- Multiple model variants tested (PO-only, PA-only, combined; with/without random effects)

Helper functions are in `R/` (100+ functions for data processing, spatial operations, and modeling utilities).

## Coding preferences

**Default to tidyverse** (dplyr, tidyr, tibble, ggplot2, stringr, purrr) for tabular and functional operations.

**Use terra** for all spatial operations (rasters, vector data, spatial indexing). Prefer `terra::` functions over legacy `raster::` or `sp::` equivalents.

**Bayesian modeling** uses `greta` (probabilistic programming with TensorFlow backend) for the main multispecies point process model.

**Spatial visualization** combines `ggplot2` + `tidyterra` for raster overlays and maps.

Use the base R pipe `|>` consistently. Write comments only when the reasoning is non-obvious; skip comments that narrate code.

## Common operations

- **Extracting covariates to points**: Use `terra::extract(raster, points, method='simple')` and join spatially
- **Rasterizing point counts**: `terra::rasterize(points, template_raster, fun='sum')` or similar
- **Handling missing covariate data**: Inspect with `NA` detection and imputation strategies; document assumptions
- **Filtering and reshaping VA data**: Most operations use grouped operations with `dplyr::group_by()` + `summarise()` or row-wise `mutate()`

## Key files

- `_targets.R` — reproducible pipeline
- `R/*.R` — helper functions for data processing and spatial operations
- `data/tabular/va.data_*.csv` — primary occurrence/count data
- `data/processed/` — cleaned intermediate datasets
- `outputs/rasters/` — predicted species distribution rasters
- `outputs/figures/` — validation diagnostics and maps
- `extras/sources_counts_resids.R` — exploratory analysis and data quality checks
- `kenya_mess/` — regional environmental space analysis (MESS)

## Current focus

- Developing and validating multispecies Bayesian models
- Integrating heterogeneous data types (PO, PA, counts) with proper likelihood accounting
- Addressing taxonomic groupings (sibling species complexes)
- Running posterior predictive checks and convergence diagnostics
- Predicting species-specific probability of presence and expected abundance maps

## coding conventions

where functions take >1 argument, write them over several lines, so

runif(
  n,
  min = 0,
  max = 1
)

not:
runif(n, min = 0, max = 1)

do:
sapply(
 X,
 FUN,
 simplify = TRUE,
 USE.NAMES = TRUE
)

dont:
sapply(X, FUN, simplify = TRUE, USE.NAMES = TRUE)

This is easier to read, and new lines are free. 
