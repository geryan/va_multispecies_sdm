# Plot the arabiensis MESS and ExDet novelty surfaces

library(targets.utils) # remotes::install_github("geryan/targets.utils")
library(bssdm)         # remotes::install_github("cebra-analytics/bssdm")
library(tmap)


## edit this to point to wherever this script is held
archive_dir <- "arabiensis_novelty"
figure_dir <- file.path(archive_dir, "figures")


## Read

# whole of Africa, all variables

# MESS
# > 0 (blue)  -> within univariate range of all variables
# < 0 (red)   -> extrapolating.
# ~ 0 (white) -> OK but near edge of one or more varibles.
mess_arabiensis <- read_terra_nested(
  file.path(archive_dir, "africa_mess_arabiensis.tar.gz")
)

# ExDet
# 1   (yellow) -> similar, within multivariate / combinatorial range of variables
# < 1 (red)    -> Type 1 Novelty, true extrapolation, same as mess <0
# >1  (blue)   -> Type 2 novelty, combinatorial extrapolation; variables are within the univariate range of variables but in unseen combinations
exdet_arabiensis <- read_terra_nested(
  file.path(archive_dir, "africa_exdet_arabiensis.tar.gz")
)

mess_nosea_arabiensis <- read_terra_nested(
  file.path(archive_dir, "africa_mess_nosea_arabiensis.tar.gz")
)
exdet_nosea_arabiensis <- read_terra_nested(
  file.path(archive_dir, "africa_exdet_nosea_arabiensis.tar.gz")
)

# masked to the core arabiensis expert range
mess_arabiensis_masked <- read_terra_nested(
  file.path(archive_dir, "africa_mess_arabiensis_masked.tar.gz")
)
mess_nosea_arabiensis_masked <- read_terra_nested(
  file.path(archive_dir, "africa_mess_nosea_arabiensis_masked.tar.gz")
)
exdet_arabiensis_masked <- read_terra_nested(
  file.path(archive_dir, "africa_exdet_arabiensis_masked.tar.gz")
)
exdet_nosea_arabiensis_masked <- read_terra_nested(
  file.path(archive_dir, "africa_exdet_nosea_arabiensis_masked.tar.gz")
)


## Plot
#
# The ExDet objects were built without mic, so only which = "exdet" is
# available for them.

# whole of Africa
plot(mess_arabiensis, which = "mess")
plot(mess_arabiensis, which = "mod")
plot(mess_arabiensis, which = "mos")

plot(mess_nosea_arabiensis, which = "mess")
plot(mess_nosea_arabiensis, which = "mod")
plot(mess_nosea_arabiensis, which = "mos")

plot(exdet_arabiensis, which = "exdet")

plot(exdet_nosea_arabiensis, which = "exdet")

# masked to the core arabiensis expert range
plot(mess_arabiensis_masked, which = "mess")
plot(mess_arabiensis_masked, which = "mod")
plot(mess_arabiensis_masked, which = "mos")

plot(mess_nosea_arabiensis_masked, which = "mess")
plot(mess_nosea_arabiensis_masked, which = "mod")
plot(mess_nosea_arabiensis_masked, which = "mos")

plot(exdet_arabiensis_masked, which = "exdet")

plot(exdet_nosea_arabiensis_masked, which = "exdet")


## Save each plot as a png
#
# Sized for a full 16:9 widescreen PowerPoint slide (13.333 x 7.5 in) at 300 dpi.
#
# ggsave() is not usable here: the bssdm plot methods print a tmap object and
# return the data invisibly, so there is no plot object to hand to ggsave() (or
# to tmap_save()), and tmap_last() fails on these because it re-evaluates the
# call outside the bssdm namespace. So capture the plots straight off a png
# device instead.

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(x, which, file) {
  png(
    filename = file.path(figure_dir, file),
    width = 13.333,
    height = 7.5,
    units = "in",
    res = 300
  )
  plot(x, which = which)
  dev.off()
}

# whole of Africa
save_plot(mess_arabiensis, "mess", "mess_arabiensis_mess.png")
save_plot(mess_arabiensis, "mod", "mess_arabiensis_mod.png")
save_plot(mess_arabiensis, "mos", "mess_arabiensis_mos.png")

save_plot(mess_nosea_arabiensis, "mess", "mess_nosea_arabiensis_mess.png")
save_plot(mess_nosea_arabiensis, "mod", "mess_nosea_arabiensis_mod.png")
save_plot(mess_nosea_arabiensis, "mos", "mess_nosea_arabiensis_mos.png")

save_plot(exdet_arabiensis, "exdet", "exdet_arabiensis_exdet.png")

save_plot(exdet_nosea_arabiensis, "exdet", "exdet_nosea_arabiensis_exdet.png")

# masked to the core arabiensis expert range
save_plot(mess_arabiensis_masked, "mess", "mess_arabiensis_mess_masked.png")
save_plot(mess_arabiensis_masked, "mod", "mess_arabiensis_mod_masked.png")
save_plot(mess_arabiensis_masked, "mos", "mess_arabiensis_mos_masked.png")

save_plot(mess_nosea_arabiensis_masked, "mess", "mess_nosea_arabiensis_mess_masked.png")
save_plot(mess_nosea_arabiensis_masked, "mod", "mess_nosea_arabiensis_mod_masked.png")
save_plot(mess_nosea_arabiensis_masked, "mos", "mess_nosea_arabiensis_mos_masked.png")

save_plot(exdet_arabiensis_masked, "exdet", "exdet_arabiensis_exdet_masked.png")

save_plot(exdet_nosea_arabiensis_masked, "exdet", "exdet_nosea_arabiensis_exdet_masked.png")
