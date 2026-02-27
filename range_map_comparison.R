# non expert range extent limits
#
targets.utils::tl()

plot_species <- model_data_records |>
  filter(!(species %in% unique(expert_maps$species))) |>
  pull(species) |>
  unique()

# convex hull
make_point_plots(
  model_data_spatial = model_data_spatial,
  expert_maps = convex_hulls,
  new_mask = project_mask_5,
  plot_species = plot_species,
  dir = "outputs/figures/range_plots/convex_hull"
)


# bioregion hull
make_point_plots(
  model_data_spatial = model_data_spatial,
  expert_maps = bioregion_hulls,
  new_mask = project_mask_5,
  plot_species = plot_species,
  dir = "outputs/figures/range_plots/bioregion_hull"
)

# buffered point hull
# 100 km
make_point_plots(
  model_data_spatial = model_data_spatial,
  expert_maps = point_hulls_100,
  new_mask = project_mask_5,
  plot_species = plot_species,
  dir = "outputs/figures/range_plots/point_hull_100"
)

# 500 km
make_point_plots(
  model_data_spatial = model_data_spatial,
  expert_maps = point_hulls_500,
  new_mask = project_mask_5,
  plot_species = plot_species,
  dir = "outputs/figures/range_plots/point_hull_500"
)

# 1000 km
make_point_plots(
  model_data_spatial = model_data_spatial,
  expert_maps = point_hulls_1000,
  new_mask = project_mask_5,
  plot_species = plot_species,
  dir = "outputs/figures/range_plots/point_hull_1000"
)
