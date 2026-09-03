# Two-panel scatter of the arabiensis non-inferred records used as the MESS /
# exdet reference set (africa_mess_arabiensis, africa_exdet_arabiensis).
# Each panel shows a pair of fractional-cover covariates on a common 0-1 scale,
# with a box delimiting the min-max range of that pair -- i.e. the rectangle
# that MESS / exdet NT1 treat as "not novel" for those two variables.

library(targets)
library(dplyr)
library(ggplot2)
library(patchwork)

model_data_spatial <- tar_read(model_data_spatial)

arab <- model_data_spatial |>
  filter(species == "arabiensis") |>
  filter(!inferred)

panel <- function(data, xvar, yvar) {
  box <- data |>
    summarise(
      xmin = min(.data[[xvar]]),
      xmax = max(.data[[xvar]]),
      ymin = min(.data[[yvar]]),
      ymax = max(.data[[yvar]])
    )

  ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(
      alpha = 0.25,
      size = 1.2,
      colour = "grey20"
    ) +
    geom_rect(
      data = box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = NA,
      colour = "firebrick",
      linewidth = 0.7
    ) +
    # pad the limits past 0-1 so the box edges sitting at zero are drawn clear
    # of the axis lines rather than underneath them
    coord_equal(
      xlim = c(-0.06, 1.06),
      ylim = c(-0.06, 1.06),
      expand = FALSE
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.25)) +
    scale_y_continuous(breaks = seq(0, 1, 0.25)) +
    labs(x = xvar, y = yvar) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
}

arab_covariate_scatter <- panel(arab, "mangroves", "wetland") +
  panel(arab, "trees", "shrubs") +
  plot_annotation(
    title = "An. arabiensis records (non-inferred)",
    subtitle = sprintf(
      "n = %s records; red box = min-max range of the plotted pair",
      format(nrow(arab), big.mark = ",")
    )
  )

ggsave(
  filename = "outputs/figures/arabiensis_covariate_scatter.png",
  plot = arab_covariate_scatter,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white"
)
