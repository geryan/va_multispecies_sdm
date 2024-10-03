## data pipeline

library(dplyr)
library(tidyr)
library(terra)
library(tidyterra)
library(sdmtools)
library(ggplot2)
library(readr)

# library(readr)
# tar_load(raw_data)
# write_csv(
#   raw_data,
#   "data/raw_data.csv"
# )

source("R/clean_species.R")

raw_data <- read_csv("data/raw_data.csv")

# layer with empty zero values for gambiae mechanistic
#modlyr <- rast("outputs/model_layers_mech.tif")


# alt layer with low values for mechanistic replaced with exp(-30)
# this also is aggregated by a factor of 10 for speed
# static_vars <- rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars_std.tif")
#
# mmv <- aggregate(
#   static_vars,
#   fact = 10,
#   filename = "data/raster/static_vars_agg.tif",
#   overwrite = TRUE,
#   cores = 4
# )
#
# mechvals <- values(mmv[["ag_microclim"]])
#
# lowidx <- which(mechvals <= exp(-30))
#
# mechvals[lowidx] <- exp(-30)
#
# mmv[["ag_microclim"]] <- mechvals
#
# aridvals <- values(mmv[["arid"]])
#
# mmv[["arid"]] <- round(aridvals, digits = 0)
#
# modlyr <- writereadrast(
#   mmv,
#   "data/raster/static_vars_agg_mech_nonzero.tif"
# )

modlyr <- rast("data/raster/static_vars_agg_mech_nonzero.tif")

data_records <- raw_data |>
  dplyr::select(
    source_ID,
    occ_data,
    bio_data,
    binary.presence,
    binary.absence,
    adult.data,
    larval.site.data,
    lon = longitude_1,
    lat = latitude_1,
    area.type,
    insecticide.control,
    ITN.use,
    starts_with("sampling.method"),
    starts_with("n_"),
    binary.presence,
    binary.absence,
    month_st,
    month_end,
    year_st,
    year_end,
    species
  )  |>
  # remove missing lat longs
  dplyr::filter(
    !is.na(lon) &
      !is.na(lat)
  )|>
  # remove points where insecticide is used
  dplyr::mutate(
    no_ic = case_when(
      is.na(insecticide.control) ~ TRUE,
      insecticide.control == "yes" ~ FALSE,
      TRUE ~ TRUE
    ),
    no_itn = case_when(
      is.na(ITN.use) ~ TRUE,
      ITN.use == "yes" ~ FALSE,
      TRUE ~ TRUE
    )
  ) |>
  dplyr::filter(
    no_ic & no_itn
  ) |>
  filter(occ_data == 1) |> # consider whether occ_data == 0 could be PO data
  rowwise() |> # NB this rowwise is necessary for the below `any` to work by row, but may be slow on a very large dataset
  mutate(
    any_sm_na_count = any(
      !is.na(sampling.method_1) & is.na(n_1),
      !is.na(sampling.method_2) & is.na(n_2),
      !is.na(sampling.method_3) & is.na(n_3),
      !is.na(sampling.method_4) & is.na(n_4)
    ), # this checks if there are any non-empty sampling methods with a NA count
    all_sm_na = all(is.na(c_across(starts_with("sampling.method")))) # check if all survey methods are NA so no zero count
  ) |>
  rename(entered_n_tot = n_tot) |> # renaming because want to keep for checking but will get double-counted by the sum(c_across(...)) below if left with a name beginning "n_"
  mutate(
    count = case_when(
      any_sm_na_count ~ NA, # assign NA n_tot if there is a non-empty sampling method that is NA
      all_sm_na ~ NA,
      TRUE ~ sum(c_across(starts_with("n_")), na.rm = TRUE) # otherwise sum up the values ignoring NAs
    )
  ) |>
#select(-entered_n_tot) |>
mutate(
  presence = case_when(
    #binary.absence == "yes" ~ 0, ignore this and only use
    count == 0 ~ 0,
    TRUE ~ 1
  )
) |>
  group_by(source_ID) |>
  mutate(
    pa = ifelse(any(presence == 0), "pa", "po")
  )|>
  ungroup() |>
  select(
    source_ID,
    species,
    lon,
    lat,
    #binary.presence,
    #binary.absence,
    #starts_with("n_"),
    count,
    presence,
    pa,
    starts_with("sampling.method_")
  ) |>
  mutate(
    species = clean_species(species)
  ) |>
  arrange(species, pa, presence) |>
  distinct()


# z <- data_records
# table(z$species, z$pa) |>
#   as.data.frame() |>
#   as_tibble() |>
#   rename(
#     sp = Var1,
#     p = Var2,
#     n = Freq
#   ) |>
#   pivot_wider(
#     names_from = p,
#     values_from = n
#   ) |>
#   mutate(n = pa+po) |>
#   arrange(desc(n))
#
# library(terra)
# library(tidyterra)
# library(ggplot2)
#
# new_mask <- rast("data/new_mask.tif")
#
# z |>
#   filter(
#     species %in% c(
#       "gambiae_complex",
#       "arabiensis",
#       "funestus",
#       "gambiae",
#       "pharoensis",
#       "coluzzii",
#       "melas",
#       "nili",
#       "merus",
#       "moucheti"
#     )
#   ) |>
#   mutate(
#     presence = as.factor(presence)
#   ) |>
#   filter(lon > -10) |>
#   ggplot() +
#   geom_spatraster(
#     data = new_mask,
#   ) +
#   scale_fill_viridis_c(
#     option = "G",
#     begin = 1,
#     end = 0.9,
#     na.value = "transparent"
#   ) +
#   geom_point(
#     aes(
#       x = lon,
#       y = lat,
#       col = presence,
#       shape = pa
#     ),
#     size = 0.5
#   ) +
#   facet_wrap(~species)
#
#
# z |>
#   filter(pa == "pa") |>
#   filter(
#     species %in% c(
#       "gambiae_complex",
#       "arabiensis",
#       "funestus",
#       "gambiae",
#       "pharoensis",
#       "coluzzii",
#       "melas",
#       "nili",
#       "merus",
#       "moucheti"
#     )
#   ) |>
#   dplyr::select(lon, lat, species, presence) |>
#   group_by(lon, lat, species) |>
#   summarise(presence = max(presence), .groups = "drop") |>
#   tidyr::pivot_wider(
#     names_from = species,
#     values_from = presence
#   ) |>
#   # mutate(
#   #   across(
#   #     c(-lat, -lon),
#   #     ~ ifelse(is.na(.x), 0, .x)
#   #   )
#   # ) |>
#   # mutate(
#   #   n = arabiensis +
#   #     pharoensis +
#   #     funestus +
#   #     gambiae_complex +
#   #     melas +
#   #     nili +
#   #     coluzzii +
#   #     gambiae +
#   #     merus
#   # ) |>
#   mutate(
#     n = as.numeric(!is.na(arabiensis)) +
#       as.numeric(!is.na(pharoensis)) +
#       as.numeric(!is.na(funestus)) +
#       as.numeric(!is.na(gambiae_complex)) +
#       as.numeric(!is.na(melas)) +
#       as.numeric(!is.na(nili)) +
#       as.numeric(!is.na(coluzzii)) +
#       as.numeric(!is.na(gambiae)) +
#       as.numeric(!is.na(merus))
#   ) |>
#   arrange(desc(n))


model_records <- data_records  |>
  filter(
    species %in% c(
      #"gambiae_complex",
      "arabiensis",
      "funestus",
      "gambiae",
      #"pharoensis",
      "coluzzii"#,
      # "melas",
      # "nili",
      # "merus",
      # "moucheti"
    )
  )

pa_unfilled <- model_records |>
  filter(pa == "pa") |>
  dplyr::select(lon, lat, species, presence) |>
  group_by(lon, lat, species) |>
  summarise(presence = max(presence), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from = species,
    values_from = presence
  )

pa_not_na_idx <- which(
  !is.na(pa_unfilled |>
           select(-lon, -lat) |>
           as.matrix()),
  arr.ind = TRUE
)


pa_filled <- pa_unfilled |>
  mutate(
      across(
        c(-lat, -lon),
        ~ ifelse(is.na(.x), 0, .x)
      )
  )

po <- model_records |>
  filter(pa == "po") |>
  select(lon, lat, species)


pa_covs <- extract_covariates(
  covariates = modlyr,
  presences_and_absences = pa_filled |>
    rename(x = lon, y = lat)
) |>
  # select(
  #   tcw,
  #   tcb,
  #   built_volume,
  #   everything()
  # ) |>
  drop_na() |>
  as.data.frame()

po_covs <- extract_covariates(
  covariates = modlyr,
  presences = po |>
    rename(x = lon, y = lat)
) |>
  bind_cols(po) |>
  select(-presence, -lat, -lon) |>
  drop_na() |>
  make_mpp_list(species)


# library(targets)
# tar_load(bg_points)
# bg_points |>
#   as.data.frame() |>
#   write_csv("data/bg_points.csv")
bg_points <- read_csv("data/bg_points.csv") |>
  as.matrix()

bg <- extract_covariates(
  covariates = modlyr,
  presences = bg_points |>
    as_tibble()
) |>
  select(-presence) |>
  drop_na() |>
  as.data.frame()



# convert to counts
# anything that is not a count and isn't an absence is PO
# otherwise convert counts to PA
# retain sampling method
#
save.image("outputs/dataect.RData")
