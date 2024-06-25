##### model 6 / 7


gambiae <- preds_6[["gambiae"]]

gambiae_vals <- values(gambiae)
hist(gambiae_vals[which(gambiae_vals < 0.1)])
hist(gambiae_vals[which(gambiae_vals < 0.01)])
hist(gambiae_vals[which(gambiae_vals < 0.005)])

gambiae_scale <- gambiae

gambiae_vals_scale <- ifelse(gambiae_vals > 0.0025, 0.0025, gambiae_vals)/0.0025

gambiae_scale[] <- gambiae_vals_scale

plot(gambiae_scale)


ggplot() +
  geom_spatraster(
    data = gambiae_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles gambiae")))



gambiae <- preds_7[["gambiae"]]

gambiae_vals <- values(gambiae)
hist(gambiae_vals[which(gambiae_vals < 0.1)])
hist(gambiae_vals[which(gambiae_vals < 0.01)])
hist(gambiae_vals[which(gambiae_vals < 0.005)])

thold <- 0.0025

gambiae_scale <- gambiae

gambiae_vals_scale <- ifelse(gambiae_vals > thold, thold, gambiae_vals)/thold

gambiae_scale[] <- gambiae_vals_scale

plot(gambiae_scale)


ggplot() +
  geom_spatraster(
    data = gambiae_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles gambiae")))


## arabiensis

arabiensis <- preds_6[["arabiensis"]]

arabiensis_vals <- values(arabiensis)
hist(arabiensis_vals[which(arabiensis_vals < 0.3)])
hist(arabiensis_vals[which(arabiensis_vals < 0.2)])

thold <- 0.2

arabiensis_scale <- arabiensis

arabiensis_vals_scale <- ifelse(arabiensis_vals > thold, thold, arabiensis_vals)/thold

arabiensis_scale[] <- arabiensis_vals_scale

plot(arabiensis_scale)


ggplot() +
  geom_spatraster(
    data = arabiensis_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void()  +
  labs(title = expression(italic("Anopheles arabiensis")))


# coluzzii

coluzzii <- preds_6[["coluzzii"]]

coluzzii_vals <- values(coluzzii)
hist(coluzzii_vals[which(coluzzii_vals < 0.3)])
hist(coluzzii_vals[which(coluzzii_vals < 0.05)])
hist(coluzzii_vals[which(coluzzii_vals < 0.01)])


thold <- 0.01

coluzzii_scale <- coluzzii

coluzzii_vals_scale <- ifelse(coluzzii_vals > thold, thold, coluzzii_vals)/thold

coluzzii_scale[] <- coluzzii_vals_scale

plot(coluzzii_scale)


ggplot() +
  geom_spatraster(
    data = coluzzii_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles coluzzii")))



# funestus

funestus <- preds_6[["funestus"]]

funestus_vals <- values(funestus)
hist(funestus_vals[which(funestus_vals < 0.3)])
hist(funestus_vals[which(funestus_vals < 0.15)])
hist(funestus_vals[which(funestus_vals < 0.1)])


thold <- 0.1

funestus_scale <- funestus

funestus_vals_scale <- ifelse(funestus_vals > thold, thold, funestus_vals)/thold

funestus_scale[] <- funestus_vals_scale

plot(funestus_scale)


ggplot() +
  geom_spatraster(
    data = funestus_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles funestus")))


#### model 8

gambiae <- preds_8[["gambiae"]]

gambiae_vals <- values(gambiae)
hist(gambiae_vals[which(gambiae_vals < 0.1)])
hist(gambiae_vals[which(gambiae_vals < 0.01)])
hist(gambiae_vals[which(gambiae_vals < 0.005)])

thold <- 0.0025

gambiae_scale <- gambiae

gambiae_vals_scale <- ifelse(gambiae_vals > thold, thold, gambiae_vals)/thold

gambiae_scale[] <- gambiae_vals_scale

plot(gambiae_scale)


ggplot() +
  geom_spatraster(
    data = gambiae_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles gambiae")))


## arabiensis

arabiensis <- preds_8[["arabiensis"]]

arabiensis_vals <- values(arabiensis)
hist(arabiensis_vals[which(arabiensis_vals < 0.3)])
hist(arabiensis_vals[which(arabiensis_vals < 0.2)])

thold <- 0.3

arabiensis_scale <- arabiensis

arabiensis_vals_scale <- ifelse(arabiensis_vals > thold, thold, arabiensis_vals)/thold

arabiensis_scale[] <- arabiensis_vals_scale

plot(arabiensis_scale)


ggplot() +
  geom_spatraster(
    data = arabiensis_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void()  +
  labs(title = expression(italic("Anopheles arabiensis")))


# coluzzii

coluzzii <- preds_8[["coluzzii"]]

coluzzii_vals <- values(coluzzii)
hist(coluzzii_vals[which(coluzzii_vals < 0.3)])
hist(coluzzii_vals[which(coluzzii_vals < 0.05)])
hist(coluzzii_vals[which(coluzzii_vals < 0.01)])


thold <- 0.01

coluzzii_scale <- coluzzii

coluzzii_vals_scale <- ifelse(coluzzii_vals > thold, thold, coluzzii_vals)/thold

coluzzii_scale[] <- coluzzii_vals_scale

plot(coluzzii_scale)


ggplot() +
  geom_spatraster(
    data = coluzzii_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles coluzzii")))



# funestus

funestus <- preds_8[["funestus"]]

funestus_vals <- values(funestus)
hist(funestus_vals[which(funestus_vals < 0.3)])
hist(funestus_vals[which(funestus_vals < 0.15)])
hist(funestus_vals[which(funestus_vals < 0.1)])


thold <- 0.15

funestus_scale <- funestus

funestus_vals_scale <- ifelse(funestus_vals > thold, thold, funestus_vals)/thold

funestus_scale[] <- funestus_vals_scale

plot(funestus_scale)


ggplot() +
  geom_spatraster(
    data = funestus_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles funestus")))

#### model 9

gambiae <- preds_6[["gambiae"]]

gambiae_vals <- values(gambiae)
hist(gambiae_vals[which(gambiae_vals < 0.1)])
hist(gambiae_vals[which(gambiae_vals < 0.01)])
hist(gambiae_vals[which(gambiae_vals < 0.005)])

gambiae_scale <- gambiae

gambiae_vals_scale <- ifelse(gambiae_vals > 0.0025, 0.0025, gambiae_vals)/0.0025

gambiae_scale[] <- gambiae_vals_scale

plot(gambiae_scale)


ggplot() +
  geom_spatraster(
    data = gambiae_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles gambiae")))



gambiae <- preds_9[["gambiae"]]

gambiae_vals <- values(gambiae)
hist(gambiae_vals[which(gambiae_vals < 0.1)])
hist(gambiae_vals[which(gambiae_vals < 0.01)])
hist(gambiae_vals[which(gambiae_vals < 0.005)])

thold <- 0.0025

gambiae_scale <- gambiae

gambiae_vals_scale <- ifelse(gambiae_vals > thold, thold, gambiae_vals)/thold

gambiae_scale[] <- gambiae_vals_scale

plot(gambiae_scale)


ggplot() +
  geom_spatraster(
    data = gambiae_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles gambiae")))


## arabiensis

arabiensis <- preds_9[["arabiensis"]]

arabiensis_vals <- values(arabiensis)
hist(arabiensis_vals[which(arabiensis_vals < 0.3)])
hist(arabiensis_vals[which(arabiensis_vals < 0.2)])

thold <- 0.2

arabiensis_scale <- arabiensis

arabiensis_vals_scale <- ifelse(arabiensis_vals > thold, thold, arabiensis_vals)/thold

arabiensis_scale[] <- arabiensis_vals_scale

plot(arabiensis_scale)


ggplot() +
  geom_spatraster(
    data = arabiensis_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void()  +
  labs(title = expression(italic("Anopheles arabiensis")))


# coluzzii

coluzzii <- preds_9[["coluzzii"]]

coluzzii_vals <- values(coluzzii)
hist(coluzzii_vals[which(coluzzii_vals < 0.3)])
hist(coluzzii_vals[which(coluzzii_vals < 0.05)])
hist(coluzzii_vals[which(coluzzii_vals < 0.01)])


thold <- 0.01

coluzzii_scale <- coluzzii

coluzzii_vals_scale <- ifelse(coluzzii_vals > thold, thold, coluzzii_vals)/thold

coluzzii_scale[] <- coluzzii_vals_scale

plot(coluzzii_scale)


ggplot() +
  geom_spatraster(
    data = coluzzii_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles coluzzii")))



# funestus

funestus <- preds_9[["funestus"]]

funestus_vals <- values(funestus)
hist(funestus_vals[which(funestus_vals < 0.3)])
hist(funestus_vals[which(funestus_vals < 0.15)])
hist(funestus_vals[which(funestus_vals < 0.1)])


thold <- 0.1

funestus_scale <- funestus

funestus_vals_scale <- ifelse(funestus_vals > thold, thold, funestus_vals)/thold

funestus_scale[] <- funestus_vals_scale

plot(funestus_scale)


ggplot() +
  geom_spatraster(
    data = funestus_scale
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles funestus")))




points_plot <- ggplot(
) +
  geom_spatraster(data = new_mask) +
  geom_point(
    data = rcrds_2 |>
      filter(species %in% c("gambiae", "arabiensis", "funestus", "coluzzii")) |>
      mutate(presence = as.factor(presence)),
    aes(x = lon, y = lat, colour = presence)
  ) +
  facet_wrap(~species) +
  theme_void() +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0.6,
    na.value = "white"
  ) +
  theme_void() +
  guides(fill = "none") +
  scale_colour_manual(values = c("grey70", "hotpink"))

points_plot

ggsave(
  "outputs/figures/points_plot.png",
  plot = points_plot,
  width = 2000,
  height = 1600,
  units = "px"
)

## hacky plots

plot(mech)
hist(values(mech))
mvs <- values(mech)

mvs <- mvs*5
mvs <- ifelse(mvs>1, 1, mvs)

m <- mech
m[] <- mvs

plot(m)

mapmask <- rast("data/raster/am_1km_pfec.tif")

mapmask[which(values(mapmask <1))] <- NA

mapmask <- match_ref(mapmask, new_mask)


# gambiae


gambiae <- preds_9[["gambiae"]]

gambiae_vals <- values(gambiae)
hist(gambiae_vals[which(gambiae_vals < 0.1)])
hist(gambiae_vals[which(gambiae_vals < 0.01)])
hist(gambiae_vals[which(gambiae_vals < 0.005)])

thold <- 0.004

gambiae_scale <- gambiae

gambiae_vals_scale <- ifelse(gambiae_vals > thold, thold, gambiae_vals)/thold

gambiae_scale[] <- gambiae_vals_scale

plot(gambiae_scale)


ggplot() +
  geom_spatraster(
    data = gambiae_scale * m
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles gambiae"))) +
  guides(fill = "none")

ggsave(
  "outputs/figures/gambiae_hack.png",
  width = 2000,
  height = 1600,
  units = "px"
)


## arabiensis

arabiensis <- preds_9[["arabiensis"]]

arabiensis_vals <- values(arabiensis)
hist(arabiensis_vals[which(arabiensis_vals < 0.4)])
hist(arabiensis_vals[which(arabiensis_vals < 0.2)])

thold <- 0.4

arabiensis_scale <- arabiensis

arabiensis_vals_scale <- ifelse(arabiensis_vals > thold, thold, arabiensis_vals)/thold

arabiensis_scale[] <- arabiensis_vals_scale

plot(arabiensis_scale)


ggplot() +
  geom_spatraster(
    data = arabiensis_scale * m
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void()  +
  labs(title = expression(italic("Anopheles arabiensis"))) +
  guides(fill = "none")

ggsave(
  "outputs/figures/arabiensis_hack.png",
  width = 2000,
  height = 1600,
  units = "px"
)


# coluzzii

coluzzii <- preds_9[["coluzzii"]]

coluzzii_vals <- values(coluzzii)
hist(coluzzii_vals[which(coluzzii_vals < 0.3)])
hist(coluzzii_vals[which(coluzzii_vals < 0.05)])
hist(coluzzii_vals[which(coluzzii_vals < 0.01)])


thold <- 0.01

coluzzii_scale <- coluzzii

coluzzii_vals_scale <- ifelse(coluzzii_vals > thold, thold, coluzzii_vals)/thold

coluzzii_scale[] <- coluzzii_vals_scale

plot(coluzzii_scale)


ggplot() +
  geom_spatraster(
    data = coluzzii_scale*m
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles coluzzii")))  +
  guides(fill = "none")

ggsave(
  "outputs/figures/coluzzii_hack.png",
  width = 2000,
  height = 1600,
  units = "px"
)



# funestus

funestus <- preds_9[["funestus"]]

funestus_vals <- values(funestus)
hist(funestus_vals[which(funestus_vals < 0.3)])
hist(funestus_vals[which(funestus_vals < 0.15)])
hist(funestus_vals[which(funestus_vals < 0.1)])


thold <- 0.15

funestus_scale <- funestus

funestus_vals_scale <- ifelse(funestus_vals > thold, thold, funestus_vals)/thold

funestus_scale[] <- funestus_vals_scale

plot(funestus_scale)


ggplot() +
  geom_spatraster(
    data = funestus_scale*m
  ) +
  scale_fill_viridis_c(
    option = "G",
    begin = 1,
    end = 0,
    na.value = "white"
  ) +
  theme_void() +
  labs(title = expression(italic("Anopheles funestus"))) +
  guides(fill = "none")

ggsave(
  "outputs/figures/funestus_hack.png",
  width = 2000,
  height = 1600,
  units = "px"
)


