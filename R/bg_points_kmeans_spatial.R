# sample 'n_bg' points according to a spatial pattern (defined by kmeans
# clustering of coordinates) over the grid cells of 'covariate_rast'
bg_points_kmeans_spatial <- function(
    n_bg,
    covariate_rast,
    n_samples_per_bg = 50,
    plot_dir = "outputs/figures"
  ) {

  n_pixels <- max(terra::global(covariate_rast, fun = "notNA"))
  n_samples <- min(n_bg * n_samples_per_bg, n_pixels)

  # define spatial points on the raster, and extract coordinates and covariate values
  points_samples <- spatSample(covariate_rast,
                               n_samples,
                               na.rm = TRUE,
                               method = "random",
                               xy = TRUE,
                               values = FALSE)

  # kmeans cluster these to get point locations
  kmn <- stats::kmeans(points_samples, centers = n_bg)
  bg_coords <- vect(kmn$centers)
  bg_coords$cluster <- seq_len(n_bg)

  # extract covariate values
  values <- terra::extract(covariate_rast,
                           bg_coords,
                           ID = FALSE)

  # use voronoi polygons to compute area corresponding to each bg point
  v <- terra::voronoi(bg_coords)
  v_rast_all <- rasterize(v, covariate_rast, field = "cluster")
  v_rast <- mask(v_rast_all, covariate_rast[[1]])
  areas <- cellSize(v_rast, unit = "km")
  cluster_areas <- zonal(areas, v_rast, fun = "sum")

  if(!is.null(plot_dir)){

    # plot voronoi in all it's glory; just for fun
    png(
      filename = sprintf(
        "%s/bg_voronoi_points_kmeans_spatial_all.png",
        plot_dir
      ),
      res = 300,
      height = 2000,
      width  = 2000
    )
    plot(
      v_rast_all,
      col = c("mediumorchid1", "yellow2", "olivedrab2", "grey20", "lightyellow"),
      axes = FALSE,
      legend = FALSE,
      box = FALSE
    )
    dev.off()

    png(
      filename = sprintf(
        "%s/bg_voronoi_points_kmeans_spatial_all_stained_glass.png",
        plot_dir
      ),
      res = 300,
      height = 2000,
      width  = 2000
    )
    plot(
      v_rast_all,
      col = c("red", "green", "blue", "yellow", "orange", "purple"),
      axes = FALSE,
      legend = FALSE,
      box = FALSE
    )
    dev.off()

    png(
      filename = sprintf(
        "%s/bg_voronoi_points_kmeans_spatial_all_stained_zion.png",
        plot_dir
      ),
      res = 300,
      height = 2000,
      width  = 2000
    )
    plot(
      v_rast_all,
      col = c("red", "green", "yellow", "black"),
      axes = FALSE,
      legend = FALSE,
      box = FALSE
    )
    dev.off()
    png(
      filename = sprintf(
        "%s/bg_voronoi_points_kmeans_spatial_all_stained_zion2.png",
        plot_dir
      ),
      res = 300,
      height = 2000,
      width  = 2000
    )
    plot(
      v_rast_all,
      col = c("red", "green", "yellow", "black", "white"),
      axes = FALSE,
      legend = FALSE,
      box = FALSE
    )
    dev.off()

    # plot voronoi masked
    png(
      filename = sprintf(
        "%s/bg_voronoi_kmeans_spatial.png",
        plot_dir
      ),
      res = 300,
      height = 2000,
      width  = 2000
    )
    plot(
      v_rast,
      col = c("mediumorchid1", "yellow2", "olivedrab2", "grey20", "lightyellow"),
      axes = FALSE,
      legend = FALSE,
      box = FALSE
    )
    dev.off()

    pm <- v_rast
    pm[!is.na(values(pm))] <- 0
    png(
      filename = sprintf(
        "%s/bg_points_kmeans_spatial.png",
        plot_dir
      ),
      res = 300,
      height = 2000,
      width  = 2000
    )
    plot(
      pm,
      col = "grey80",
      axes = FALSE,
      legend = FALSE,
      box = FALSE
    )
    points(
      bg_coords
    )

    dev.off()

    png(
      filename = sprintf(
        "%s/bg_points_voronoi_kmeans_spatial.png",
        plot_dir
      ),
      res = 300,
      height = 2000,
      width  = 2000
    )
    plot(
      pm,
      col = "grey80",
      axes = FALSE,
      legend = FALSE,
      box = FALSE
    )
    points(
      bg_coords
    )
    plot(
      v ,
      add = TRUE
    )
    dev.off()

  }



  list(
    x = values,
    weight = cluster_areas$area,
    coords = kmn$centers
  )
}
