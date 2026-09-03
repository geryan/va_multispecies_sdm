# Posit Assistant Context: Vector Atlas Multispecies SDM

## Project Overview

This is a Bayesian species distribution modeling (SDM) project for multiple *Anopheles* mosquito species (malaria vectors) across Africa. The project integrates presence-only records, presence-absence surveys, and count data into a multispecies point process model to predict vector distributions and relative abundance using environmental covariates and mechanistic offsets.

**Target species** (9 *Anopheles* species):  
*arabiensis*, *gambiae*, *coluzzii*, *funestus*, *melas*, *merus*, *moucheti*, *nili*

## Data

### Primary sources
- **Vector Atlas (VA) records** (`data/tabular/va.data_*.csv`): ~250,000+ cleaned occurrence and count observations from field surveys, with metadata including species, sampling method, location, date, and literature source
- **Source metadata** (`ars_sources`): Literature citations and study metadata for all VA records
- **Processed datasets** (`data/processed/`): Cleaned, indexed, and spatially matched data ready for modeling

### Spatial covariates (5km resolution, 10km for predictions)
- **Landcover** (WorldCover): trees, grassland, shrubs, cropland, water, wetlands, mangroves, etc.
- **Bioregions** (One Earth): ~30 smoothed bioregion indicators across Africa
- **Soil** (ISRIC SoilGrids): clay and silt content
- **Proximity**: distance to sea, human footprint
- **Sampling bias offset**: travel time from research facilities
- **Mechanistic offsets**: environmental suitability indices from mosquito mechanistic models (300+ monthly/seasonal layers)
- **Expert ranges**: historical species range maps (Sinka et al. 2010) used as prior information

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

- `_targets.R` — reproducible pipeline (1594 lines)
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
