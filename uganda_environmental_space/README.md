# Uganda land cover and human footprint

Annual land cover and human footprint for Uganda, cropped and masked to the
national boundary. Made by `extras/make_uganda_environmental_space.R` in the
Vector Atlas multispecies SDM repository.

All rasters are on the same grid: WGS 84 lon/lat (EPSG:4326), 0.0417° cells
(about 5 km), 138 rows × 130 columns. Cells outside Uganda are NA.

## Files

| File | Contents |
|---|---|
| `landcover_<class>_1992_2022.tif` | Proportion of the cell in that land-cover class (0–1). One file per class, one layer per year (31 layers, named `<class>_<year>`). |
| `footprint_2000_2024.tif` | Human Footprint index (0–50, raw units). One layer per year (25 layers, named `footprint_<year>`). |
| `uganda_boundary.gpkg` | Uganda national boundary (GADM level 0), used for the crop and mask. |

## Land cover

The ESA CCI / C3S land-cover maps (300 m, 1992–2022) grouped into 11 classes.
Each 5 km cell holds the share of its ~225 source cells in each class, so the 11
classes sum to 1 in every cell. Source cells of no data, snow and ice, or lichens
and mosses are left out of the share.

| Class | ESA CCI classes included |
|---|---|
| `crop_other` | rainfed cropland (all types), mosaic cropland |
| `irrigated` | irrigated cropland |
| `tree` | all tree cover classes, mosaic natural vegetation, mosaic tree and shrub |
| `shrubland` | shrubland (evergreen, deciduous) |
| `grassland` | grassland, mosaic herbaceous |
| `sparse` | sparse vegetation (tree, shrub, herbaceous) |
| `wetland` | tree, shrub or herbaceous cover flooded with fresh or brackish water |
| `mangrove` | tree cover flooded with saline water |
| `urban` | urban |
| `bare` | bare areas |
| `water` | water |

1992–2015 come from product version 2.0.7, and 2016–2022 from version 2.1.1 (the
C3S continuation).

## Human footprint

Mu et al.'s annual Human Footprint (figshare version 8, which runs to 2024 even
though the paper's title says 2018). The source is at 1 km in the Mollweide
projection; it was reprojected to this grid by averaging.

The footprint has no values over open water. Lake Victoria, Lake Albert, Lake
Kyoga and the other large lakes are NA in every year, so the footprint layers
cover 9,850 cells against 11,595 for land cover.

## Citations

**Land cover**

> Copernicus Climate Change Service, Climate Data Store (2019). Land cover
> classification gridded maps from 1992 to present derived from satellite
> observation. Copernicus Climate Change Service (C3S) Climate Data Store (CDS).
> doi:[10.24381/cds.006f2c9a](https://doi.org/10.24381/cds.006f2c9a)

**Human footprint**

> Mu, H., Li, X., Wen, Y., Huang, J., Du, P., Su, W., Miao, S., Geng, M. (2022).
> A global record of annual terrestrial Human Footprint dataset from 2000 to 2018.
> *Scientific Data* 9: 176.
> doi:[10.1038/s41597-022-01284-8](https://doi.org/10.1038/s41597-022-01284-8)

Data: figshare, doi:[10.6084/m9.figshare.16571064](https://doi.org/10.6084/m9.figshare.16571064)
(CC BY 4.0).

**Boundary**

> GADM (2022). Database of Global Administrative Areas, version 4.1.
> <https://gadm.org>
