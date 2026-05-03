r <- col24

# Required packages
packages <- c("terra","sf","dplyr","tidyr","ggplot2","ggforce","viridis","scales")
invisible(lapply(packages, function(p) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)))
library(terra); library(sf); library(dplyr); library(tidyr); library(ggplot2); library(ggforce); library(viridis); library(scales)

# Shared helpers ---------------------------------------------------------------
# r: SpatRaster with 12 layers (Jan..Dec)
# global color limits for temperature
temp_limits <- c(-5, 40)
viridis_pal <- viridis::viridis(256)

# Convert raster to sf points (sample n points across land)
sample_points <- function(r, n=500, method="regular"){
  # mask to non-NA
  rmask <- r[[1]]
  if(method=="regular"){
    pts <- spatSample(rmask, size=n, method="regular", as.points=TRUE)
  } else {
    pts <- spatSample(rmask, size=n, method="random", as.points=TRUE)
  }
  pts_sf <- st_as_sf(pts)
  # extract monthly values
  vals <- terra::extract(r, pts )
  colnames(vals) <- paste0("m", 1:12)
  pts_sf <- bind_cols(pts_sf, as.data.frame(vals))
  pts_sf
}

# Ensure raster has 12 layers
stop_if_not_12 <- function(r){
  if(!inherits(r, "SpatRaster")) stop("r must be a SpatRaster")
  if(nlyr(r) != 12) stop("r must have exactly 12 layers (Jan..Dec)")
}

# 1) Chrono-Rings --------------------------------------------------------------
# Draw radial 12-segment glyphs at sampled points.
plot_chrono_rings <- function(r, n_points=500, out_png=NULL, width=1600, height=1000, dpi=150){
  stop_if_not_12(r)
  pts <- sample_points(r, n=n_points, method="regular")
  # compute per-point climatology (already monthly) and per-point mean & sd
  months <- paste0("m",1:12)
  pts_long <- pts %>%
    st_drop_geometry() %>%
    mutate(id = row_number()) %>%
    pivot_longer(cols = all_of(months), names_to="month", values_to="temp") %>%
    mutate(month = as.integer(sub("m","",month))) %>%
    group_by(id) %>%
    mutate(mean_loc = mean(temp, na.rm=TRUE),
           sd_loc = sd(temp, na.rm=TRUE),
           temp_anom = temp - mean_loc,
           # normalized radius 0..1 per glyph for shape readability
           rnorm = (temp - min(temp, na.rm=TRUE)) / (max(temp, na.rm=TRUE) - min(temp, na.rm=TRUE) + 1e-9)) %>%
    ungroup()
  # build arc segments for each month using ggforce::geom_arc_bar
  # convert point coords to numeric columns
  coords <- st_coordinates(pts)
  pts_coords <- pts_long %>% mutate(X = coords[.(id),1], Y = coords[.(id),2]) # not vectorized; fix below

  # safer join coords by id
  coords_df <- data.frame(id = 1:nrow(pts), X = coords[,1], Y = coords[,2])
  pts_long <- left_join(pts_long, coords_df, by="id")

  # parameters for glyph sizing (in degrees; scale with map extent)
  ext <- ext(r)
  map_width_deg <- ext[2] - ext[1]
  glyph_base <- map_width_deg / 60  # tweak: controls glyph size
  pts_long <- pts_long %>% mutate(
    r_outer = glyph_base * (0.4 + 0.6 * rnorm),   # outer radius per month
    r_inner = r_outer * 0.6,
    start = 2*pi*(month-1)/12,
    end   = 2*pi*month/12
  )

  # prepare arc_bar data frame for geom_arc_bar
  arc_df <- pts_long %>%
    mutate(
      # ggforce::geom_arc_bar expects x0,y0,r0,r1,start,end
      x0 = X, y0 = Y, r0 = r_inner, r1 = r_outer, start = start, end = end
    )

  # outer band for sd: draw a thin ring with width proportional to sd (normalized)
  sd_df <- pts_long %>% group_by(id, X, Y) %>%
    summarize(sd_loc = first(sd_loc), mean_loc = first(mean_loc)) %>%
    ungroup() %>%
    mutate(sd_norm = sd_loc / (max(sd_loc, na.rm=TRUE) + 1e-9),
           r0 = glyph_base * 0.65,
           r1 = r0 + glyph_base * 0.12)

  p <- ggplot() +
    # base map: simple country outlines from raster extent (use terra::as.polygons of mask)
    geom_sf(data = st_as_sf(as.polygons(r[[1]])), fill = "gray95", color = "gray80", size=0.2) +
    # chrono ring segments colored by anomaly (diverging)
    geom_arc_bar(data = arc_df,
                 aes(x0 = x0, y0 = y0, r0 = r0, r1 = r1, start = start, end = end, fill = temp_anom),
                 color = NA, alpha = 0.95) +
    # outer sd band
    geom_arc_bar(data = sd_df,
                 aes(x0 = X, y0 = Y, r0 = r0, r1 = r1, start = 0, end = 2*pi, fill = sd_norm),
                 color = "black", alpha = 0.25) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                         name = "Anomaly (°C)") +
    coord_sf() +
    theme_minimal() +
    labs(title = "Chrono-Rings: Monthly Temperature Climatology (12 months)",
         subtitle = "Angle = month (Jan→Dec clockwise); radius = normalized monthly temp; outer band = interannual SD") +
    theme(legend.position = "right")

  if(!is.null(out_png)){
    ggsave(out_png, p, width = width/ dpi, height = height/ dpi, dpi = dpi)
  }
  p
}

# 2) Ribbon-Anchored Timeline ---------------------------------------------------
# Create ribbons from point to timeline; approximate gradient by plotting month segments
plot_ribbon_timeline <- function(r, n_points=200, out_png=NULL, width=1600, height=1000, dpi=150){
  stop_if_not_12(r)
  pts <- sample_points(r, n=n_points, method="regular")
  coords <- st_coordinates(pts)
  pts_df <- pts %>% st_drop_geometry() %>% mutate(id = row_number(), X = coords[,1], Y = coords[,2])
  months <- paste0("m",1:12)
  pts_long <- pts_df %>% pivot_longer(cols = all_of(months), names_to="month", values_to="temp") %>%
    mutate(month = as.integer(sub("m","",month)))

  # timeline coordinates: map months to x positions across bottom of map
  ext <- ext(r)
  x_min <- ext[1]; x_max <- ext[2]
  # timeline x positions (Jan..Dec) across the full width
  timeline_x <- seq(x_min + 0.05*(x_max-x_min), x_max - 0.05*(x_max-x_min), length.out = 12)
  names(timeline_x) <- 1:12
  timeline_y <- ext[3] - 0.06*(ext[4]-ext[3])  # slightly below map bottom

  # For each point and month create a small segment from point to the timeline month x
  # We'll draw many short segments along a curve by linear interpolation between point and (timeline_x, timeline_y)
  segs <- pts_long %>%
    rowwise() %>%
    mutate(tx = timeline_x[as.character(month)], ty = timeline_y) %>%
    ungroup() %>%
    # create intermediate points along the curve (t parameter)
    group_by(id, month) %>%
    do({
      row <- .
      # create 6 interpolation steps from point to timeline point
      tvals <- seq(0,1,length.out=6)
      df <- data.frame(
        id = row$id,
        month = row$month,
        temp = row$temp,
        t = tvals,
        x = (1 - tvals) * row$X + tvals * row$tx,
        y = (1 - tvals) * row$Y + tvals * row$ty
      )
      df
    }) %>% ungroup()

  # compute segment pairs for geom_segment (from each t to next t)
  seg_pairs <- segs %>%
    group_by(id, month) %>%
    arrange(t) %>%
    mutate(xend = lead(x), yend = lead(y)) %>%
    filter(!is.na(xend)) %>%
    ungroup()

  # map temperature to segment thickness (absolute mapping)
  seg_pairs <- seg_pairs %>% mutate(size = scales::rescale(temp, to = c(0.2, 2.5), from = temp_limits),
                                    col = temp)

  # base map
  base_map <- st_as_sf(as.polygons(r[[1]]))
  p <- ggplot() +
    geom_sf(data = base_map, fill = "gray98", color = "gray80", size=0.2) +
    # draw many small segments colored by month value (time gradient along the ribbon)
    geom_segment(data = seg_pairs, aes(x = x, y = y, xend = xend, yend = yend, color = col, size = size),
                 lineend = "round", alpha = 0.7) +
    scale_color_viridis_c(option = "viridis", limits = temp_limits, name = "Temp (°C)") +
    scale_size_identity() +
    # draw timeline mini-bars at bottom for a few example ribbons (optional)
    geom_vline(xintercept = timeline_x, color = "gray80", linetype = "dotted", size = 0.2) +
    annotate("text", x = timeline_x, y = timeline_y - 0.02*(ext[4]-ext[3]), label = month.abb, size = 2.5) +
    coord_sf() +
    theme_minimal() +
    labs(title = "Ribbon-Anchored Timeline: Temperature Evolution (12 months sample)",
         subtitle = "Ribbons connect each location to the absolute timeline; color along ribbon = time; thickness = temperature") +
    theme(legend.position = "right")

  if(!is.null(out_png)){
    ggsave(out_png, p, width = width/ dpi, height = height/ dpi, dpi = dpi)
  }
  p
}

# 3) WeaveGrid Strips ----------------------------------------------------------
# Overlay grid cells and draw aligned horizontal strips (x=time, color=temp) inside each cell
plot_weavegrid_strips <- function(r, cell_deg = 2, out_png=NULL, width=1600, height=1000, dpi=150){
  stop_if_not_12(r)
  # aggregate raster to coarse resolution (cell_deg degrees)
  # compute aggregation factor relative to raster resolution
  res_deg <- res(r)
  # choose factor to approximate cell_deg
  fact_x <- max(1, round(cell_deg / res_deg[1]))
  fact_y <- max(1, round(cell_deg / res_deg[2]))
  r_agg <- aggregate(r, fact = c(fact_x, fact_y), fun = mean, na.rm = TRUE)
  # convert aggregated raster to points (centroids) with monthly values
  pts <- as.data.frame(terra::as.data.frame(r_agg, xy=TRUE, na.rm=TRUE))
  # rename columns to m1..m12
  colnames(pts)[3:14] <- paste0("m",1:12)
  # create cell id and compute polygon extents for plotting
  pts <- pts %>% mutate(id = row_number())
  # create a data frame of small tiles inside each cell: for each cell, create 12 mini-tiles across x (time)
  # compute cell polygon width and height
  cell_w <- (ext(r)[2] - ext(r)[1]) / ncol(r_agg)
  cell_h <- (ext(r)[4] - ext(r)[3]) / nrow(r_agg)

  # for plotting, we will place the 12 mini-tiles horizontally inside the cell bounding box
  tiles <- pts %>% pivot_longer(cols = starts_with("m"), names_to = "month", values_to = "temp") %>%
    mutate(month = as.integer(sub("m","",month))) %>%
    rowwise() %>%
    mutate(
      # compute left x for this month tile
      xleft = x - cell_w/2 + (month-1) * (cell_w / 12),
      xright = x - cell_w/2 + month * (cell_w / 12),
      ybottom = y - cell_h/2 + cell_h*0.25,  # place strip in lower half of cell
      ytop = y - cell_h/2 + cell_h*0.75
    ) %>%
    ungroup()

  # convert to polygons for geom_rect
  p <- ggplot() +
    geom_rect(data = tiles, aes(xmin = xleft, xmax = xright, ymin = ybottom, ymax = ytop, fill = temp),
              color = NA) +
    geom_sf(data = st_as_sf(as.polygons(r[[1]])), fill = NA, color = "gray80", size=0.2) +
    scale_fill_viridis_c(option = "viridis", limits = temp_limits, name = "Temp (°C)") +
    coord_sf() +
    theme_minimal() +
    labs(title = "WeaveGrid Strips: Aligned Monthly Temperature Strips",
         subtitle = "Each grid cell contains a 12-month horizontal strip; vertical alignment compares the same month across space") +
    theme(legend.position = "right")

  if(!is.null(out_png)){
    ggsave(out_png, p, width = width/ dpi, height = height/ dpi, dpi = dpi)
  }
  p
}

# Example usage (uncomment and run with your SpatRaster 'r'):
# r must be a SpatRaster with 12 layers (Jan..Dec)
stop_if_not_12(r)
p1 <- plot_chrono_rings(r, n_points = 500, out_png = "chrono_rings.png")
p2 <- plot_ribbon_timeline(r, n_points = 200, out_png = "ribbon_timeline.png")
p3 <- plot_weavegrid_strips(r, cell_deg = 2, out_png = "weavegrid_strips.png")
