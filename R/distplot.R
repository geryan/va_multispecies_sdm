distplot <- function(
    rst,
    sp,
    colscheme = c(
      "va",
      "mako",
      "rb",
      "magma",
      "rocket",
      "mono",
      "orchid",
      "brick"
    ),
    guide = c(
      "none",
      "prob",
      "abundance",
      "cv"
    ),
    limits = NULL,
    log_scale = FALSE,
    log_floor = 1e-3
){

  colscheme <- match.arg(colscheme)

  guide <- match.arg(guide)

  spname <- paste0("Anopheles ", sp)

  r <- subset(rst, sp)

  # distplotlist() passes one set of limits for every species; on its own,
  # abundance is scaled to this species' layer
  if (is.null(limits)) {
    limits <- switch(
      guide,
      none = NULL,
      prob = c(0, 1),
      abundance = abundance_limits(
        r,
        log_scale = log_scale
      ),
      cv = c(0, 1)
    )
  }

  if (log_scale) {

    if (!guide %in% c("prob", "abundance")) {
      stop(
        "distplot(): log_scale is only for guide = \"prob\" or \"abundance\"",
        call. = FALSE
      )
    }

    # zeros (masked cells) and anything below the floor are drawn at the floor,
    # so they take the palest colour instead of going to -Inf and vanishing
    limits[1] <- log_floor

    r <- terra::clamp(
      r,
      lower = log_floor,
      values = TRUE
    )

  }

  p <- ggplot() +
    geom_spatraster(
      data = r
    ) +
    theme_void() +
    labs(
      title = bquote(italic(.(spname)))
    )

  scale_args <- list(
    na.value = "transparent",
    limits = limits
  )

  # on a linear scale abundance and cv each have a long upper tail, so the scale
  # is capped: cells above the cap take the top colour and the top label reads
  # ">= cap". Otherwise a handful of extreme cells set the range and almost
  # every cell comes out in the palest colour. A log scale runs to the maximum
  # instead (see abundance_limits()), so its top is not a cap, but its floor
  # is, and the floor's label reads "<= floor"
  capped_top <- guide %in% c("abundance", "cv") && !log_scale

  if (capped_top || log_scale) {

    breaks <- if (log_scale) {
      unique(
        c(
          10^seq(
            ceiling(log10(limits[1])),
            floor(log10(limits[2]))
          ),
          limits[2]
        )
      )
    } else {
      pretty(limits)
    }

    breaks <- breaks[breaks >= limits[1] & breaks <= limits[2]]

    scale_args <- c(
      scale_args,
      list(
        oob = scales::squish,
        breaks = breaks,
        labels = function(x){
          # signif, because the top of a log scale is the raw maximum
          lab <- as.character(signif(x, 2))
          # breaks come back through the log transform, so compare with a
          # tolerance rather than exactly
          top <- capped_top & !is.na(x) & x >= limits[2] * (1 - 1e-9)
          bottom <- log_scale & !is.na(x) & x <= limits[1] * (1 + 1e-9)
          lab[top] <- paste0("≥", lab[top])
          lab[bottom] <- paste0("≤", lab[bottom])
          lab
        }
      )
    )

    if (log_scale) {
      scale_args$transform <- "log10"
    }

  }

  fill_scale <- switch(
    colscheme,
    va = do.call(
      scale_fill_gradient,
      c(
        list(
          low = grey(0.9),
          high = "navy"
        ),
        scale_args
      )
    ),
    mako = do.call(
      scale_fill_viridis_c,
      c(
        list(
          option = "G",
          direction = -1
        ),
        scale_args
      )
    ),
    rb = do.call(
      scale_colour_distiller,
      c(
        list(
          palette = "RdBu",
          type = "div",
          aesthetics = c("fill")
        ),
        scale_args
      )
    ),
    magma = do.call(
      scale_fill_viridis_c,
      c(
        list(
          option = "A",
          direction = -1
        ),
        scale_args
      )
    ),
    rocket = do.call(
      scale_fill_viridis_c,
      c(
        list(
          option = "F",
          direction = -1
        ),
        scale_args
      )
    ),
    mono = do.call(
      scale_id_continuous,
      c(
        list(
          cols = c("grey97", "black")
        ),
        scale_args
      )
    ),
    orchid = do.call(
      scale_id_continuous,
      c(
        list(
          cols = c("grey97", "darkorchid4")
        ),
        scale_args
      )
    ),
    brick = do.call(
      scale_id_continuous,
      c(
        list(
          cols = c("grey97", "firebrick")
        ),
        scale_args
      )
    )
  )

  p <- p + fill_scale

  p <- switch(
    guide,
    none = p +
      guides(
        fill = "none"
      ),
    prob = p +
      labs(
        fill = "Prob. of\noccurrence"
      ),
    abundance = p +
      labs(
        fill = "Relative\nabundance"
      ),
    cv = p +
      labs(
        fill = "CV of prob.\nof occurrence"
      )
  )

  p

}
