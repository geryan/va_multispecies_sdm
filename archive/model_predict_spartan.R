.libPaths("~/home/user/R/gr_lib/")


rm(list = ls())

warmup1 <- 1000
nsamples1 <- 1000
warmup2 <- 5000
nsamples2 <- 5000


library(dplyr)
library(tidyr)
library(terra)
library(readr)
library(greta)

###########
### data
##########


raw_data <- read_csv("data/raw_data.csv")

modlyr <- rast("outputs/model_layers_mech.tif")


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


bg <- extract_covariates(
  covariates = modlyr,
  presences = background |>
    as_tibble()
) |>
  select(-presence) |>
  drop_na() |>
  as.data.frame()


##############
# fit
############



all_locations <- bind_rows(
  pa_covs |>
    select(
      ag_microclim,
      research_tt_by_country,
      arid,
      built_volume,
      cropland,
      elevation,
      evi_mean,
      footprint,
      lst_day_mean,
      lst_night_mean,
      pop,
      pressure_mean,
      rainfall_mean,
      soil_clay,
      solrad_mean,
      surface_water,
      tcb_mean,
      tcw_mean,
      windspeed_mean,
      easting,
      northing
    ),
  bg |>
    select(
      ag_microclim,
      research_tt_by_country,
      arid,
      built_volume,
      cropland,
      elevation,
      evi_mean,
      footprint,
      lst_day_mean,
      lst_night_mean,
      pop,
      pressure_mean,
      rainfall_mean,
      soil_clay,
      solrad_mean,
      surface_water,
      tcb_mean,
      tcw_mean,
      windspeed_mean,
      easting,
      northing
    ),
  tibble(po = po_covs) |>
    unnest(po) |>
    as.data.frame() |>
    select(
      ag_microclim,
      research_tt_by_country,
      arid,
      built_volume,
      cropland,
      elevation,
      evi_mean,
      footprint,
      lst_day_mean,
      lst_night_mean,
      pop,
      pressure_mean,
      rainfall_mean,
      soil_clay,
      solrad_mean,
      surface_water,
      tcb_mean,
      tcw_mean,
      windspeed_mean,
      easting,
      northing
    )
)

mech_dat <- all_locations$ag_microclim

idx <- which(mech_dat <= exp(-30))

mech_dat[idx] <- exp(-30)

log_mech_dat <- log(mech_dat) |>
  matrix(data = _, ncol = 1)


npa <- nrow(pa_covs)
nbg <- nrow(bg)
npo <- tibble(po = mppdat$po) |>
  unnest(po) |>
  nrow()

#pa.samp <- 1:npa
bg.samp <- (npa + 1):(npa + nbg)
po.samp <- (npa + nbg +1):(npa + npo + nbg)

#area_bg <- sum(!is.na(values(model_layers_mech[[1]])))/nbg
#area_bg <- 825.1676 # this is the above with 30k bg points
area_bg <- 3713.069 # with 6kish points

npospp <- sapply(po_covs, nrow)

x <- all_locations |>
  select(
    arid,
    built_volume,
    cropland,
    elevation,
    evi_mean,
    footprint,
    lst_day_mean,
    lst_night_mean,
    pop,
    pressure_mean,
    rainfall_mean,
    soil_clay,
    solrad_mean,
    surface_water,
    tcb_mean,
    tcw_mean,
    windspeed_mean,
    easting,
    northing
  ) |>
  as.matrix()

z <- all_locations |>
  select(research_tt_by_country) |>
  as.matrix()

n.pixel <- nrow(x)

# numbers of covariates in use
n_cov_abund <- ncol(x)
n_cov_bias <- ncol(z)

# number of species to model
n_species <- 4

# tidy up PA data
pa <-pa_filled |>
  select(arabiensis, funestus, coluzzii, gambiae) |>
  as.matrix()

# generate po count matrix from all locations by assigning 1 to po per spp.
po.count <- matrix(
  data = 0,
  nrow = n.pixel,
  ncol = n_species,
  dimnames = list(NULL, names(pa))
)

po.count[(npa+nbg+1):(npa+nbg+npospp[1]), 1] <- 1
po.count[(npa+nbg+npospp[1]+1):(npa+nbg+npospp[1]+npospp[2]), 2] <- 1
po.count[(npa+nbg+npospp[1]+npospp[2]+1):(npa+nbg+npospp[1]+npospp[2]+npospp[3]), 3] <- 1
po.count[(npa+nbg+npospp[1]+npospp[2]+npospp[3]+1):(npa+nbg+npospp[1]+npospp[2]+npospp[3]+npospp[4]), 4] <- 1


# define parameters with normal priors, matching the ridge regression setup in
# multispeciesPP defaults
penalty.l2.sdm <- penalty.l2.bias <- 0.1
penalty.l2.intercept <- 1e-4

# trying others
# penalty.l2.sdm <- penalty.l2.bias <- 0.2
# penalty.l2.intercept <- 1e-2


intercept_sd <- sqrt(1 / penalty.l2.intercept)
beta_sd <- sqrt(1 / penalty.l2.sdm)
delta_sd <- sqrt(1 / penalty.l2.bias)

# intercept and shared slope for selection bias
gamma <- normal(0, intercept_sd, dim = n_species)
delta <- normal(0, delta_sd, dim = c(n_cov_bias))

# intercept and slopes for abundance rate
alpha <- normal(0, intercept_sd, dim = n_species)
beta <- normal(0, beta_sd, dim = c(n_cov_abund, n_species))

# log rates across all sites

log_lambda_larval_habitat <- sweep(x %*% beta, 2, alpha, FUN = "+")

log_lambda_adults <- log_mech_dat

log_lambda <- sweep(log_lambda_larval_habitat, 1, log_lambda_adults, "+")

# can easily replace this model with something more interesting, like a low-rank
# GP on covariate space or something mechanistic

# bias across pixels (shared coefficient) and species (different intercepts)
log_bias_coef <- sweep(zeros(n.pixel, n_species), 1, z %*% delta, FUN = "+")
log_bias <- sweep(log_bias_coef, 2, gamma, FUN = "+")

# rates across all sites and species
lambda <- exp(log_lambda)

# offset stuff
bias <- exp(log_bias)

# define likelihoods

# compute probability of presence (and detection) at the PA sites, assuming
# area/effort of 1 in all these sites, for identifiability

### can compute separately for each PA PO and RELABUND
area_pa <- 1
#area_pa <- uniform(0,1)

# for relabund
# log_lambda ~ unscaled abundance
# need in scaling factor plus random effect of trap type
# per covid model to get size and prob for negbin
# expected_abundance <- exp(log_lambda[abund,] + trap_effect + species_scaling_factor)
#
# sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = 1)
# size <- 1 / sqrt(sqrt_inv_size)
# prob <- 1 / (1 + expected_abundance / size)
# distribution(abundance) <- negative_binomial(size = size, prob = prob)

# predict for each species lambda + species_scaling_factor
# mod to per relative abundance paper also for output
# i.e. lambda / rowsums(lambda)

# p <- icloglog(log_lambda[pa.samp, ] + log(area_pa))
# distribution(pa) <- bernoulli(p)

p <- icloglog(log_lambda[pa_not_na_idx] + log(area_pa))
distribution(pa[pa_not_na_idx]) <- bernoulli(p)


## compute (biased) expected numbers of presence-only observations across all
## presence and background sites, assuming presence-only count aggregation area
## of 1 (as in multispeciesPP definition). Not that when these are all the same,
## this value only changes all the gamma parameters by a fixed amount, and these
## are not the parameters of interest


area_po <- 1/nbg # 1/10000 # very small
po_idx <- which(po.count == 1, arr.ind = TRUE)
po_rate_po <- exp(log_lambda[po_idx] + log_bias[po_idx] + log(area_po))
distribution(po.count[po_idx]) <- poisson(po_rate_po)

# area_bg <- 3654.515 <- whole area / nnbg
po_rate_bg <- exp(log_lambda[bg.samp, ] + log_bias[bg.samp,] + log(area_bg))
distribution(po.count[bg.samp, ]) <- poisson(po_rate_bg)


# define and fit the model by MAP and MCMC
m <- model(alpha, beta, gamma, delta)



inits <- function(){

  nsp <- 4
  ncv <- 19
  n_a <- nsp
  n_b <- nsp * ncv
  n_g <- nsp

  ina <- -1e-2
  inb <- -1e-1
  ing <- 0 #1e-4
  ind <- 1e-4

  initials(
    alpha = rep(ina, n_a),
    beta = matrix(
      data = rep(inb, n_b),
      ncol = ,
      nrow = ncv
    ),
    gamma = rep(ing, n_g),
    delta = ind
  )

}




draws <- mcmc(m, warmup = warmup1, n_samples = nsamples1, initial_values = inits())


posterior <- calculate(alpha, beta, gamma, delta, values = draws, nsim = 100)

inits_alpha <- apply(posterior$alpha, MARGIN = 2, FUN = mean)
inits_beta <- apply(posterior$beta, MARGIN = c(2,3), FUN = mean)
inits_gamma <- apply(posterior$gamma, MARGIN = 2, FUN = mean)
inits_delta <- mean(posterior$delta)



inits_2 <- initials(
  alpha = inits_alpha,
  beta = inits_beta,
  gamma = inits_gamma,
  delta = inits_delta
)



draws <- mcmc(m, warmup = warmup2, n_samples = nsamples2, initial_values = inits_2)

r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
max(r_hats$psrf[, 2])
r_hats



##############

## predict

##############


## Predictinos

split_rast <- function(
    x,
    grain = 4,
    write_temp = FALSE
){

  dimx <- dim(x)[1]
  dimy <- dim(x)[2]

  if(grain > dimx | grain > dimy){
    stop("grain is > x or y dimension.\nCannot split into rasters smaller than cells.")
  }

  resx <- terra::res(x)[1]
  resy <- terra::res(x)[2]

  xminx <- terra::xmin(x)
  yminx <- terra::ymin(x)

  xseq <- seq(
    from = 1,
    to = dimx + 1,
    length.out = grain + 1
  ) |>
    round()

  yseq <- seq(
    from = 1,
    to = dimy + 1,
    length.out = grain + 1
  ) |>
    round()

  xminseq <- xseq[1:grain]
  xmaxseq <- xseq[2:(grain + 1)]

  yminseq <- yseq[1:grain]
  ymaxseq <- yseq[2:(grain + 1)]

  tidyr::expand_grid(
    tibble::tibble(
      xmin = xminseq,
      xmax = xmaxseq
    ),
    tibble::tibble(
      ymin = yminseq,
      ymax = ymaxseq
    )
  ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::starts_with("x"), ~ (.x - 1)*resx + xminx),
      dplyr::across(tidyselect::starts_with("y"), ~ (.x - 1)*resy + yminx)
    ) |>
    dplyr::mutate(
      r = purrr::pmap(
        .l = list(xmin, xmax, ymin, ymax),
        .f = function(xmin, xmax, ymin, ymax, x, write_temp){

          xt <- terra::ext(c(xmin, xmax, ymin, ymax))

          z <- terra::crop(x, xt)

          if(write_temp){

            tempname <- tempfile(fileext = ".tif")
            writeRaster(z, filename = tempname)
            z <- rast(tempname)

          }

          z

        },
        x,
        write_temp
      )
    ) |>
    dplyr::pull(r)

}


mlm_split <- split_rast(
  model_layers_mech,
  grain = 10,
  write_temp = TRUE
)



predict_greta_sdm <- function(
    r,
    n,
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha,
    beta,
    draws
){

  nspp <- length(spp)
  layer_values <- values(r)
  naidx <- is.na(layer_values[,1])

  ncells <- length(naidx)

  if(ncells == sum(naidx)){

    preds <- rep(r[[1]], times = nspp)
    preds[] <- NA
    names(preds) <- spp

    writeRaster(
      preds,
      filename = sprintf(
        "outputs/predstemp/pred_%s.tif",
        n
      ),
      overwrite = TRUE
    )
    return(print(n))
  }

  x_predict <- layer_values[
    !naidx,
    c(
      "arid",
      "built_volume",
      "cropland",
      "elevation",
      "evi_mean",
      "footprint",
      "lst_day_mean",
      "lst_night_mean",
      "pop",
      "pressure_mean",
      "rainfall_mean",
      "soil_clay",
      "solrad_mean",
      "surface_water",
      "tcb_mean",
      "tcw_mean",
      "windspeed_mean",
      "easting",
      "northing"
    )
  ]

  mech_pred <- layer_values[!naidx, "ag_microclim"]

  lowidx <- which(mech_pred <= exp(-30))

  mech_pred[lowidx] <- exp(-30)

  log_mech_pred <- log(mech_pred) |>
    matrix(data = _, ncol = 1)

  log_lambda_adults_predict <- log_mech_pred

  log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")

  log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")

  p_predict <- icloglog(log_lambda_predict + log(area_pa))

  preds <- calculate(p_predict, values = draws, nsim = 100)

  preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)

  preds_rast <- rep(r[[1]], times = nspp)

  for(i in 1:nspp) {
    preds_rast[[i]][!naidx] <- preds_mean[,i]
  }

  names(preds_rast) <- spp

  writeRaster(
    preds_rast,
    filename = sprintf(
      "outputs/predstemp/pred_%s.tif",
      n
    ),
    overwrite = TRUE
  )
  gc()

  print(n)

}

for(i in 1:100){

  predict_greta_sdm(
    mlm_split[[i]],
    n = i,
    spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
    alpha = alpha,
    beta = beta,
    draws = draws
  )
}


# library(future)
# library(future.apply)
#
# plan("multicore", workers = 4)
# predlist <- mapply(
#   FUN = predict_greta_sdm,
#   r = mlm_split,
#   n = 1:length(mlm_split),
#   MoreArgs = list(
#     spp = c("arabiensis", "funestus",  "coluzzii", "gambiae"),
#     alpha = alpha,
#     beta = beta,
#     draws = draws
#   )
# )



# pred#all_layer_values <- values(model_layers_mech)
# all_layer_values <- values(mlm_split[[51]])
# naidx <- is.na(all_layer_values[,1])
#
#
# x_predict <- all_layer_values[
#   !naidx,
#   c(
#     "arid",
#     "built_volume",
#     "cropland",
#     "elevation",
#     "evi_mean",
#     "footprint",
#     "lst_day_mean",
#     "lst_night_mean",
#     "pop",
#     "pressure_mean",
#     "rainfall_mean",
#     "soil_clay",
#     "solrad_mean",
#     "surface_water",
#     "tcb_mean",
#     "tcw_mean",
#     "windspeed_mean",
#     "easting",
#     "northing"
#   )
# ]
#
# mech_pred <- all_layer_values[!naidx, "ag_microclim"]
#
# lowidx <- which(mech_pred <= exp(-30))
#
# mech_pred[lowidx] <- exp(-30)
#
# log_mech_pred <- log(mech_pred) |>
#   matrix(data = _, ncol = 1)
#
# log_lambda_adults_predict <- log_mech_pred
#
# log_lambda_larval_habitat_predict <- sweep(x_predict %*% beta, 2, alpha, FUN = "+")
#
# log_lambda_predict <- sweep(log_lambda_larval_habitat_predict, 1, log_lambda_adults_predict, "+")
#
# p_predict <- icloglog(log_lambda_predict + log(area_pa))
#
# #rm(all_layer_values)
#
# preds <- calculate(p_predict, values = draws, nsim = 100)
#
# preds_mean <- apply(preds$p_predict, MARGIN = c(2,3), median, na.rm = TRUE)
#
# preds_rast <- rep(mlm_split[[51]][[1]], times = 4)
#
#
#
# preds_rast[[1]][!naidx] <- preds_mean[,1]
# preds_rast[[2]][!naidx] <- preds_mean[,2]
# preds_rast[[3]][!naidx] <- preds_mean[,3]
# preds_rast[[4]][!naidx] <- preds_mean[,4]
#
# preds_rast

# library(lubridate)
# predictions <- predlist() |>
#   sprc() |>
#   merge(
#     filename = sprintf(
#       "outputs/preds_all_%s.tif",
#       today() |>
#         gsub(
#           pattern = "_",
#           replacement = "",
#           x = _
#         )
#     )
#   )



# add in rel abund data where available as an additional likelihood
# use a multinomial obs model where p is from predicted abundances
#

