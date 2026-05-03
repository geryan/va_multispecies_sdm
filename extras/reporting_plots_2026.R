library(targets)
tar_load_globals()
tar_load(pred_dist_sm)
tar_load(model_data_spatial)


# make distribution plots with expert overlay

eo <- rast("outputs/rasters/expert_offset_aggregated.tif") |>
  terra::aggregate(fact = 2, fun = mean, na.rm = TRUE)

eo <- eo[[c(1, 3, 2:7)]]


eo_dist <- pred_dist_sm * eo


make_distribution_plots(
    eo_dist,
    model_data_spatial,
    plot_dir = "outputs/figures/distribution_plots/distn_20260320_sm_alt"
  )
)


# make abundance cube plots with expert overlay

lambda_eo <- rast("outputs/rasters/via_spartan/multisp_pp_sm.tif") * eo

tar_load(offsets_10)
tar_load(target_species)

cubes <- make_abundance_cubes(
  lambda_no_offset_file = lambda_eo,
  offset_stack = offsets_10, #subset for purposes of iteration, use full for final
  target_species[1:8],
  write_dir = "outputs/rasters/abundance_cubes/ac_20260325"
)


cubes <- sapply(
  X = target_species[1:8],
  FUN = function(x){
    rast(
      x = sprintf(
        "outputs/rasters/abundance_cubes/ac_20260325/%s_abundance.tif",
        x
      )
    )
  }
)


# anopheles coluzii for every second month of 2024

#plot(cubes$coluzzii[[289:300]])

col24 <- cubes$coluzzii[[289:300]]

names(col24) <- names(col24) |>
  sub(
    pattern = ".*_",
    replacement = "",
    x = _
  ) |>
  ym() |>
  month(label = TRUE)

col24_6 <- col24[[seq(from = 1, to = 12, by = 2)]]

p_col24_6 <- ggplot() +
  geom_spatraster(
    data = sqrt(col24_6)
  ) +
  facet_wrap(~lyr) +
  scale_fill_gradient(
    low = grey(0.9),
    high = "navy",
    na.value = "transparent"#,
    # limits = limits
  ) +
  theme_void() +
  guides(fill = "none")

p_col24_6

ggsave(
  filename = "outputs/figures/abundance/coluzzii_2024_6_month.png",
  plot = p_col24_6,
  width = 9,
  height = 6
)


# Anopheles arabiensis every six years in April (peak temp) and August
# (peak rain)

aprils <- seq(from = 1, to = 300, by = 12) + 3
augusts <- seq(from = 1, to = 300, by = 12) + 7
yrs <- c(1, 5, 10, 15, 20, 25)


ars <- cubes$arabiensis
names(ars) <- names(ars) |>
  sub(
    pattern = ".*_",
    replacement = "",
    x = _
  ) |>
  ym() |>
  year()

ar_aprs <- ars[[aprils[yrs]]]
ar_augs <- ars[[augusts[yrs]]]


p_ar_aprs <- ggplot() +
  geom_spatraster(
    data = sqrt(ar_aprs)
  ) +
  facet_wrap(~lyr) +
  scale_fill_gradient(
    low = grey(0.9),
    high = "navy",
    na.value = "transparent"#,
    # limits = limits
  ) +
  theme_void() +
  guides(fill = "none")

p_ar_aprs

ggsave(
  filename = "outputs/figures/abundance/arabiensis_april_6yr.png",
  plot = p_ar_aprs,
  width = 9,
  height = 6
)


p_ar_augs <- ggplot() +
  geom_spatraster(
    data = sqrt(ar_augs)
  ) +
  facet_wrap(~lyr) +
  scale_fill_gradient(
    low = grey(0.9),
    high = "navy",
    na.value = "transparent"#,
    # limits = limits
  ) +
  theme_void() +
  guides(fill = "none")

ggsave(
  filename = "outputs/figures/abundance/arabiensis_august_6yr.png",
  plot = p_ar_augs,
  width = 9,
  height = 6
)

