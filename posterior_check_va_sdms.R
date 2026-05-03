library(targets)
library(tidyverse)

# load the fitted model image
tar_load(model_fit_image_multisp_pp_count_sm)

# get posterior predictive simulations for all training data, from
# preds_posterior object
load(model_fit_image_multisp_pp_count_sm)

# use DHARMa with these and the data (dat object) to compute randomised quantile
# residuals for these

library(DHARMa)
counts_resid <- DHARMa::createDHARMa(
  simulatedResponse = t(preds_posterior$count_pred),
  observedResponse = dat$count_dat,
  integerResponse = TRUE
)

pa_resid <- DHARMa::createDHARMa(
  simulatedResponse = t(preds_posterior$pa_pred),
  observedResponse = dat$pa_dat,
  integerResponse = TRUE
)

po_resid <- DHARMa::createDHARMa(
  simulatedResponse = t(preds_posterior$po_pred),
  observedResponse = dat$po_dat,
  integerResponse = TRUE
)

# plot the residuals overall (aiming for uniform distribution)
par(mfrow = c(3, 1))
hist(counts_resid$scaledResiduals,
     main = "count data")
hist(pa_resid$scaledResiduals,
     main = "presence/absence data")
hist(po_resid$scaledResiduals,
     main = "presence/background data")

# subset the information (the same way as in fit_model_multisp_pp_count to make
# the observation data) and add residuals
count_data_resids <- model_data |>
  filter(data_type == "count") |>
  mutate(
    residual = counts_resid$scaledResiduals,
    residual_norm = qnorm(pmin(pmax(residual, 1e-6), 1 - 1e-6))
  )

pa_data_resids <- model_data |>
  dplyr::filter(data_type == "pa") |>
  mutate(
    residual = pa_resid$scaledResiduals,
    residual_norm = qnorm(pmin(pmax(residual, 1e-6), 1 - 1e-6))
  )

po_data_resids <- model_data |>
  dplyr::filter(data_type %in% c("po", "bg")) |>
  mutate(
    residual = po_resid$scaledResiduals,
    residual_norm = qnorm(pmin(pmax(residual, 1e-6), 1 - 1e-6))
  )

all_data_resids <- bind_rows(
  count = count_data_resids,
  pa = pa_data_resids,
  pobg = po_data_resids,
  .id = "data_type_plot"
)

# plot the residuals against:
#   - species (vs data type)
#   - detailed sampling method (vs data type)
#   - detailed sampling method vs species (for each data type)
#   - detection covariate (for presence-background data only)
#   - month, faceted by country (for count data only)



#   - species (vs data type)
all_data_resids |>
  ggplot(
    aes(
      x = residual
    )
  ) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    species ~ data_type_plot,
    scales = "free_y"
  ) +
  theme_minimal()

# Mass close to 0: data at the lower value end of the posterior, so
# overpredicting

# Mass close to 1: data at the upper value end of the posterior, so
# underpredicting

# Count data are quite skewy, with underprediction
# some underprediction.

#   - detailed sampling method (vs data type)
all_data_resids |>
  ggplot(
    aes(
      x = residual
    )
  ) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    sampling_method ~ data_type_plot,
    scales = "free_y"
  ) +
  theme_minimal()

# not much evidence for misfitting by sampling method


#   - detailed sampling method (vs data type)
count_plot <- count_data_resids |>
  ggplot(
    aes(
      x = residual
    )
  ) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    species ~ sampling_method_detailed,
    scales = "free_y"
  ) +
  theme_minimal()
count_plot

# for count data, detailed sampling methods are the same as reduced?

# most data for HRI, arabiensis/funestus very overdispersed, nili
# underdispersed. Cancelling out in the earlier plot?

# coluzzii underdispersed? Lots of noise / small sample size here

#   - detailed sampling method (vs data type)
pa_plot <- pa_data_resids |>
  ggplot(
    aes(
      x = residual
    )
  ) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    species ~ sampling_method_detailed,
    scales = "free_y"
  ) +
  theme_minimal()
pa_plot

# larval PA data overdispersed, especially for arabiensis and coluzzi and
# gambiae

po_plot <- po_data_resids |>
  ggplot(
    aes(
      x = residual
    )
  ) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    species ~ sampling_method,
    scales = "free_y"
  ) +
  theme_minimal()
po_plot

# detection covariate
po_data_resids |>
  mutate(
    travel_time_bins = cut(
      travel_time,
      c(0, 0.8, 1))
  ) |>
  ggplot(
    aes(
      x = residual
    )
  ) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    travel_time_bins ~ species,
    scales = "free_y"
  ) +
  theme_minimal()

# po/bg predictions are underpredicting for very high values of travel time


#   - month, faceted by country (for count data only)
count_data_resids |>
  filter(
    as.numeric(end_date - start_date) < 32
  ) |>
  mutate(
    month = lubridate::month(model_date)
  ) |>
  ggplot(
    aes(
      x = residual
    )
  ) +
  geom_histogram(
    bins = 10
  ) +
  facet_grid(
    ~ month,
    scales = "free_y"
  ) +
  theme_minimal()

# possible underprediction in some months, but it doesn't seem systematic re.
# month. Note also that this would be more meaningful splot by region (different
# seasonal peaks)


# plot each species residuals over space
count_data_resids |>
  arrange(
    abs(residual_norm)
  ) |>
  ggplot(
    aes(
      x = longitude,
      y = latitude,
      colour = residual_norm
    )
  ) +
  geom_point(
    data = po_data_resids,
    colour = "grey",
    size = 2
  ) +
  geom_point() +
  facet_wrap(
    ~species
  ) +
  theme_minimal() +
  coord_equal() +
  scale_color_gradient2(
    low = "forestgreen",
    high = "purple"
  ) +
  ggtitle("count data",
          "green = overprediction, purple = underprediction")


pa_data_resids |>
  arrange(
    abs(residual_norm)
  ) |>
  ggplot(
    aes(
      x = longitude,
      y = latitude,
      colour = residual_norm
    )
  ) +
  geom_point(
    data = po_data_resids,
    colour = "grey",
    size = 2
  ) +
  geom_point() +
  facet_wrap(
    ~species
  ) +
  theme_minimal() +
  coord_equal() +
  scale_color_gradient2(
    low = "forestgreen",
    high = "purple"
  ) +
  ggtitle("presence-absence data",
          "green = overprediction, purple = underprediction")


po_data_resids |>
  arrange(
    abs(residual_norm)
  ) |>
  ggplot(
    aes(
      x = longitude,
      y = latitude,
      colour = residual_norm
    )
  ) +
  geom_point(
    data = po_data_resids,
    colour = "grey",
    size = 2
  ) +
  geom_point() +
  facet_wrap(
    ~species
  ) +
  theme_minimal() +
  coord_equal() +
  scale_color_gradient2(
    low = "forestgreen",
    high = "purple"
  ) +
  ggtitle("presence-background data",
          "green = overprediction, purple = underprediction")


# plot these against covariate values

all_data_resids |>
  pivot_longer(
    cols = all_of(target_covariate_names),
    values_to = "covariate_value",
    names_to = "covariate_name"
  ) |>
  ggplot(
    aes(
      x = covariate_value,
      y = residual_norm
    )
  ) +
  geom_point() +
  geom_smooth() +
  facet_grid(
    species ~ covariate_name,
    scales = "free_y"
  ) +
  theme_minimal()

# no very obvious residual (non-linear) relationships for included covariates.


# zoom in on count data for arabiensis and funestus


# plot each species residuals over space
count_data_resids |>
  arrange(
    abs(residual_norm)
  ) |>
  filter(
    species %in% c("arabiensis", "funestus")
  ) |>
  ggplot(
    aes(
      x = longitude,
      y = latitude,
      colour = residual_norm
    )
  ) +
  geom_point(
    data = po_data_resids |>
      filter(
        species %in% c("arabiensis", "funestus")
      ),
    colour = "grey",
    size = 4
  ) +
  geom_point(
    size = 0.5
  ) +
  facet_wrap(
    ~species
  ) +
  theme_minimal() +
  coord_equal() +
  scale_color_gradient2(
    low = "forestgreen",
    high = "purple"
  ) +
  ggtitle("count data",
          "green = overprediction, purple = underprediction")



