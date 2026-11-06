# Nigeria land cover, human footprint and An. gambiae records

Annual land cover and human footprint for Nigeria, 2013–2022, cropped and masked
to the national boundary, plus An. gambiae records from the Vector Atlas. Made
by `extras/make_nigeria_environmental_space.R` in the Vector Atlas multispecies
SDM repository. `plot_nigeria_environmental_space.R` reads and plots them.

All rasters are on the same grid: WGS 84 lon/lat (EPSG:4326), 0.0417° cells
(about 5 km). Cells outside Nigeria are NA.

## Files

| File | Contents |
|---|---|
| `landcover_<class>_2013_2022.tif` | Proportion of the cell in that land-cover class (0–1). One file per class, one layer per year. The 11 classes sum to 1 in each cell. |
| `footprint_2013_2022.tif` | Human Footprint index (0–50, raw units), one layer per year. |
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

**An. gambiae records (Vector Atlas)**

> Vector Atlas (2026). Vector Atlas. <https://www.vectoratlas.icipe.org>

Records are from the Vector Atlas data extract of 2026-06-16.

**Boundary**

> GADM (2022). Database of Global Administrative Areas, version 4.1.
> <https://gadm.org>
