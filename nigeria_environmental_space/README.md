# Nigeria land cover, human footprint, climate and An. gambiae records

Annual land cover and human footprint for Nigeria, 2013–2022, and TerraClimate
precipitation, maximum temperature and vapour pressure deficit (annual
2000–2025, monthly 2019–2022), cropped and masked to the national boundary,
plus An. gambiae records from the Vector Atlas. Made by
`extras/make_nigeria_environmental_space.R` and
`extras/make_nigeria_terraclimate.R` in the Vector Atlas multispecies SDM
repository. `plot_nigeria_environmental_space.R` reads and plots the land
cover, footprint and records.

All rasters are on the same grid: WGS 84 lon/lat (EPSG:4326), 0.0417° cells
(about 5 km). Cells outside Nigeria are NA. TerraClimate is published at 1/30°
(about 3.7 km) and was averaged onto this grid, weighted by area. The climate
layers have values in 19 coastal cells where land cover and footprint are NA.

## Files

| File | Contents |
|---|---|
| `landcover_<class>_2013_2022.tif` | Proportion of the cell in that land-cover class (0–1). One file per class, one layer per year. The 11 classes sum to 1 in each cell. |
| `footprint_2013_2022.tif` | Human Footprint index (0–50, raw units), one layer per year. |
| `terraclimate_ppt_annual_2000_2025.tif` | Total precipitation over the year (mm), one layer per year. |
| `terraclimate_tmax_annual_2000_2025.tif` | Mean of the 12 monthly maximum 2 m temperatures (°C), one layer per year. |
| `terraclimate_vpd_annual_2000_2025.tif` | Mean of the 12 monthly vapour pressure deficits (kPa), one layer per year. |
| `terraclimate_<var>_monthly_2019_2022.tif` | The same three variables by month (ppt: monthly total; tmax, vpd: monthly mean), 48 layers, January 2019 to December 2022. |
| `gambiae_records_nigeria_2013_2022.csv` | An. gambiae s.s. records inside Nigeria (month, year, latitude, longitude). Month and year are the midpoint of the survey. Recorded data only; no inferred absences. |
| `nigeria_boundary.gpkg` | Nigeria national boundary (GADM level 0), used for the crop and mask. |

## Citations

**Land cover (ESA CCI / C3S)**

> Copernicus Climate Change Service, Climate Data Store (2019). Land cover
> classification gridded maps from 1992 to present derived from satellite
> observation. Copernicus Climate Change Service (C3S) Climate Data Store (CDS).
> doi:[10.24381/cds.006f2c9a](https://doi.org/10.24381/cds.006f2c9a)

**Human footprint (Mu et al.)**

> Mu, H., Li, X., Wen, Y., Huang, J., Du, P., Su, W., Miao, S., Geng, M. (2022).
> A global record of annual terrestrial Human Footprint dataset from 2000 to 2018.
> *Scientific Data* 9: 176.
> doi:[10.1038/s41597-022-01284-8](https://doi.org/10.1038/s41597-022-01284-8)

Data: figshare, doi:[10.6084/m9.figshare.16571064](https://doi.org/10.6084/m9.figshare.16571064)
(CC BY 4.0).

**Climate (TerraClimate v1.1)**

> Abatzoglou, J.T., Hegewisch, K.C., Yin, C. and D.A. Kalashnikov, 2026,
> TerraClimate v1.1, An Updated Global Climate and Climatic Water Balance
> Dataset with Climate Scenarios. Scientific Data.

Data: Zenodo, doi:[10.5281/zenodo.23961197](https://doi.org/10.5281/zenodo.23961197)
(CC BY 4.0), version v1.1 created 2026-11-14. Downloaded 2026-11-24 from the
Climatology Lab's THREDDS server (`TERRACLIMATE_ALL`); see
<https://www.climatologylab.org/terraclimate.html>. The data providers ask that
the CHELSA datasets (Karger et al., 2021), CRU TS (Harris et al., 2020) and
ERA5 (Hersbach et al., 2020) also be acknowledged.

**An. gambiae records (Vector Atlas)**

> Vector Atlas (2026). Vector Atlas. <https://www.vectoratlas.icipe.org>

Records are from the Vector Atlas data extract of 2026-06-16.

**Boundary**

> GADM (2022). Database of Global Administrative Areas, version 4.1.
> <https://gadm.org>
