# simlulate data for this model and try to recover it

library(targets.utils)
tl()
library(terra)

# raster of covariate values
covariate_rast

r <- covariate_rast[[target_covariate_names]]

global(r, mean, na.rm = TRUE)
global(r, range, na.rm = TRUE)


# make up some coeffs for intercepts and slopes for three species
alphas <- c(-2, -3, -3.5)

betas <- matrix(
  data = c(
    rep(0.2, 6),
    rep(0.6, 6),
    rep(-0.9, 6)
  ),
  ncol = 3
)

# simulate intensity surfaces for three species
simulate_intensity <- function(r, alpha, beta){
  exp(
    alpha +
      r[[1]] * beta[1] +
      r[[2]] * beta[2] +
      r[[3]] * beta[3] +
      r[[4]] * beta[4] +
      r[[5]] * beta[5] +
      r[[6]] * beta[6]
  )
}

sc1 <- simulate_intensity(r, alphas[1], betas[,1])
sc2 <- simulate_intensity(r, alphas[2], betas[,2])
sc3 <- simulate_intensity(r, alphas[3], betas[,3])

intensity_rast <- c(sc1, sc2, sc3) |>
  sdmtools::set_layer_names(layernames = c("sp1", "sp2", "sp3"))

plot(intensity_rast)
intensity_rast

# simulate bias
# get bias correlate from distance from taveltime
bias_corr <- covariate_rast[["research_tt_by_country"]] |>
  sdmtools::set_layer_names(layernames = "bias")

# just scale it directly with the layer
gammas <- c(-0.01, 0, 0.01)
delta <- 0.99

# log bias
spp_log_bias <- delta*log(bias_corr) + gammas

# effective bias
spp_eff_bias <- exp(spp_log_bias)  |>
  sdmtools::set_layer_names(layernames = c("sp1", "sp2", "sp3"))
plot(spp_eff_bias)


# sample data using bias as weighting

# get samples of intensity in each cell
not_na_idx <- which(!is.na(values(r[[1]])))
data_sample_cells <- sample(
  x = not_na_idx,
  size = 900,
  prob = values(bias_corr)[not_na_idx]
)

bg_sample_cells  <- sample(
  x = not_na_idx,
  size = 100
)

sample_cells <- c(data_sample_cells, rep(bg_sample_cells, times = 3))

intensitiy_samples <- values(intensity_rast)[sample_cells,]|>
  as_tibble() |>
  mutate(n = row_number()) |>
  pivot_longer(
    cols = starts_with("sp"),
    names_to = "species",
    values_to = "intensity"
  )

# also extract the values of bias at those cells
bias_samples <- values(spp_eff_bias)[sample_cells,]|>
  as_tibble() |>
  mutate(n = row_number()) |>
  pivot_longer(
    cols = starts_with("sp"),
    names_to = "species",
    values_to = "bias"
  )

# also extract the covariate values at those cells
cov_values <- values(r)[sample_cells,] |>
  as_tibble()

# match these intensities and simulate data from intensity
area_po <- 1
area_bg <- 1e5

inverse_cloglog <-function(loglambda){
  1 - exp(-exp(loglambda))
}

library(purrr)
sim_data <- expand_grid(
  type = c("count", "pa", "po", "bg"),
  species = c("sp1", "sp2", "sp3"),
  n = 1:100
) |>
  mutate(
    n = row_number()
  ) |>
  left_join(
    y = intensitiy_samples,
    by = c("n", "species")
  ) |>
  left_join(
    y = bias_samples,
    by = c("n", "species")
  ) |>
  mutate(
    area = ifelse(type == "bg", area_bg, area_po)
  ) |>
  rowwise() |>
  mutate(
    data_value = pmap(
      .l = list(
        intensity,
        type,
        bias,
        area
      ),
      .f = function(intensity, type, bias, area){

        if(type == "count"){
          rpois(1, intensity)
        } else if (type == "pa") {
          rbernoulli(
            n = 1,
            p = inverse_cloglog(log(intensity))
          )
        } else if(type == "po"){
          rpois(
            n = 1,
            lambda = exp(
              log(intensity) +
                log(bias) +
                log(area)
            )
          )
        } else if(type == "bg"){
          0
        }
      }
    ) |>
      unlist()
  ) |>
  bind_cols(
    cov_values
  )

table(
  sim_data$data_value,
  sim_data$species,
  sim_data$type
)






##########################
# link functions because logs hurt my brain
p <- seq(from = 0, to = 1, by = 0.1)
x <- seq(from = -10, to = 10, by = 2)

# link: probability to natural
# logit
# use logit for 0,1 outcomes
log(p/(1-p))

# cloglog
# cloglog(x) equals the log of the expected distribution of count data following
# a poisson distribution
log(-log(1 - p))

# inverse link: natural to probability
# inverse logit
exp(x)/(1 + exp(x))
1/(1 + exp(-x)) # same

#icloglog
1 - exp(-exp(x))
