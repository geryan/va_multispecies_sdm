# Compare integration schemes for presence-background modelling

# Berman Turner device:
# https://calgary.converged.yt/articles/poisson_processes.html

# outperforms others:
# https://www.biorxiv.org/content/10.1101/2023.01.10.523499v1.full.pdf

# Define an efficient high-dimensional integration scheme

# K-means in environmental space? Can compute weights that way too


# Given a required number of background points 'bg' and a raster of covariates
# 'covariate_rast', return the environmental covariates corresponding to the
# background points, and a vector of integration weights to use when fitting a
# PPM using these data. The background points and weights are found by K-means
# clustering of a random sample of the covariate values from covariate_rast,
# with the number of samples given by `n_bg * n_samples_per_bg`. Larger values
# of 'n_samples_per_bg' should therefore result in better clustering, but with a
# longer run time. Other arguments to `stats::kmeans()` can be passed via the
# dots argument.
bg_points_kmeans <- function(n_bg, covariate_rast, n_samples_per_bg = 50, ...) {

  # sample random values from the raster and use them to train kmeans clustering

  # how many points to use when sampling the raster for kmeans
  n_pixels <- max(terra::global(covariate_rast, fun = "notNA"))
  n_samples <- min(n_bg * n_samples_per_bg, n_pixels)

  # get the samples

  # # this is ridiculously slow
  # x_sample <- terra::spatSample(covariate_rast,
  #                               n_samples,
  #                               na.rm = TRUE,
  #                               exp = Inf)

  # so extract all and sample:
  x_all <- covariate_rast[]
  valid <- apply(x_all, 1, function(x) !any(is.na(x)))
  n_valid <- sum(valid)
  valid_sample_idx <- sample.int(n_valid, n_samples)
  sample_idx <- which(valid)[valid_sample_idx]
  x_sample <- x_all[sample_idx, ]

  # get kmeans clustering
  kmn <- stats::kmeans(x_sample, centers = n_bg, ...)

  # compute environmental-space Voronoi tessellation of these centres (predicted
  # cluster ID for all pixels) and get areas covered by each cluster
  x_valid <- x_all[valid, ]
  dists <- fields::rdist(x_valid, kmn$centers)
  pixel_cluster <- apply(dists, 1, which.min)

  # compute areas of each pixel to define weights
  areas <- terra::cellSize(covariate_rast,
                           mask = TRUE,
                           unit = "km")
  pixel_areas <- areas[][valid, ]
  cluster_areas <- tapply(pixel_areas, pixel_cluster, FUN = "sum")

  # return the covariate values and weights
  list(
    x = kmn$centers,
    weight = cluster_areas
  )

}

# sample 'n_bg' points at random in the grid cells of 'covariate_rast'
bg_points_random <- function(n_bg, covariate_rast) {

  # compute total area of the raster to define weights
  total_area <- terra::expanse(covariate_rast, unit = "km")$area[1]
  list(
    x = terra::spatSample(covariate_rast,
                          n_bg,
                          na.rm = TRUE),
    weight = rep(total_area / n_bg, n_bg)
  )
}

# given a set of covariate rasters (specified by a file path, to enable parallel
# compute), covariate values for presence records for the corresponding
# covariates, a number of background points to use, and the background point
# quadrature scheme to employ, return an estimate of the regression coefficients
# of the log-linear inhomogenous PPM.
estimate_beta <- function(covariate_rast_path,
                          x_pres,
                          n_bg = 100,
                          method = c("random", "kmeans")) {

  covariate_rast <- terra::rast(covariate_rast_path)

  n_pres <- nrow(x_pres)

  # get integration points according to the required method
  method <- match.arg(method)
  bg <- switch(method,
               random = bg_points_random(n_bg, covariate_rast),
               kmeans = bg_points_kmeans(n_bg, covariate_rast))

  # set up the GLM to fit the PPM via the Berman Turner device
  y <- rep(c(1, 0),
           c(n_pres, n_bg))
  x <- rbind(x_pres, bg$x)
  w <- c(rep(1, n_pres), bg$weight)
  df <- data.frame(y = y, x)

  # fit the model
  m <- glm(y ~ .,
           data = df,
           offset = log(w),
           family = stats::poisson)

  # return all of them, except the intercept (which is not identifiable)
  as.data.frame(t(coefficients(m)[-1]))

}

library(tidyverse)
library(terra)

# # for 5x bioclim latent variables
# remotes::install_github("rdinnager/biocman")
library(biocman)

# for parallel computing in tidyverse
library(furrr)

# load 5x latent bioclim covariates, crop to madagascar, take three without
# ledge artifacts, and scale
cov_global <- biocman::get_data()
mdg_extent <- c(xmin = 43, xmax = 50.5, ymin = -26, ymax = -11.5)

cov_mdg <- crop(cov_global, mdg_extent)
cov <- scale(cov_mdg[[c(2, 4, 5)]])

# write to disk, for loading in parallel compute
cov_f <- tempfile(fileext = ".tif")
writeRaster(cov, filename = cov_f)

# test out some integration schemes for modelling PO data
set.seed(123)

# first, simulate some true parameters
n_covs <- nlyr(cov)
beta <- rnorm(n_covs, 0, 2 * sqrt(1 / n_covs))
lambda <- exp(app(cov * beta, "sum"))

# now simulate some presence-only data
n_pres <- 1000
pres_coords <- terra::spatSample(x = lambda,
                                 size = n_pres,
                                 method = "weights",
                                 values = FALSE,
                                 xy = TRUE)

# plot it
# plot(lambda)
plot(1 - exp(-lambda / 5))
points(pres_coords, cex = 0.5, lwd = 1.5)
points(pres_coords, pch = 16, col = "hotpink", cex = 0.5)

# get covariate values for presence records
x_pres <- terra::extract(x = cov,
                         y = pres_coords,
                         ID = FALSE)

# check we can estimate relatively well with lots of background points

# estimate_beta(covariate_rast_path = cov_f,
#               x_pres = x_pres,
#               n_bg = 10000,
#               method = "kmeans")
# beta

# # there are only so many unique values in the raster, so max out at this many
# # background points
# n_unique <- nrow(unique(values(cov)))

# run across a grid of parameters, in parallel
plan(multisession, workers = 8)

int_res <- expand_grid(
  replicate = seq_len(200),
  method = c("random", "kmeans"),
  n_bg = round(seq(50, 500, length.out = 30))
) %>%
  mutate(
    coefs = furrr:::future_pmap(
      list(replicate, n_bg, method),
      function(replicate, n_bg, method) {
        estimate_beta(
          n_bg = n_bg,
          method = method,
          covariate_rast_path = cov_f,
          x_pres = x_pres
        )
      },
      .options = furrr_options(seed = TRUE)
    )
  )

# validate the fit against the true parameterss
rmse <- function(estimate, truth) {
  sqrt(mean((truth - estimate) ^ 2))
}

# summarise fits
summaries <- int_res %>%
  mutate(
    rmse = purrr::map(
      coefs,
      ~rmse(as.vector(t(.x[1, ])), beta)
    )
  )


summaries %>%
  unnest(
    rmse
  ) %>%
  pivot_longer(
    cols = c(rmse),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(
    method,
    n_bg,
    metric
  ) %>%
  summarise(
    mean = mean(value),
    upper = quantile(value, 0.75),
    lower = quantile(value, 0.25)
  ) %>%
  ggplot(
    aes(
      x = n_bg,
      y = mean,
      ymax = upper,
      ymin = lower,
      colour = method
    )
  ) +
  geom_ribbon(
    alpha = 0.1
  ) +
  geom_line() +
  facet_wrap(~metric) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.5))
