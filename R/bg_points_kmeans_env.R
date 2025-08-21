# Given a required number of background points 'bg' and a raster of covariates
# 'covariate_rast', return the environmental covariates corresponding to the
# background points, and a vector of integration weights to use when fitting a
# PPM using these data. The background points and weights are found by K-means
# clustering of a random sample of the covariate values from covariate_rast,
# with the number of samples given by `n_bg * n_samples_per_bg`. Larger values
# of 'n_samples_per_bg' should therefore result in better clustering, but with a
# longer run time. Other arguments to `stats::kmeans()` can be passed via the
# dots argument.
bg_points_kmeans_env <- function(n_bg, covariate_rast, n_samples_per_bg = 50, ...) {

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
