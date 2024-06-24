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
