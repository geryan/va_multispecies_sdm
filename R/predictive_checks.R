predictive_checks <- function(
    preds,
    dat
){

  pa_dat <- dat$pa_dat
  po_dat <- dat$po_dat
  count_dat <- dat$count_dat

  pa_pred <- preds$pa_pred
  po_pred <- preds$po_pred
  count_pred <- preds$count_pred


  ###### PO checks ########

  # count of zeroes and ones vs predicted
  # ppc_bars(
  #   y = po_dat,
  #   yrep = po_pred
  # ) # really effing slow for this
  # seems to be throwing up horrendously high simulations

  # check the mean number of zeroes vs the predicted mean in each sim
  ppc_stat(
    y = po_dat,
    yrep = po_pred,
    stat = function(x){mean(x == 0)},
    bins = 50
  ) +
    xlim(0, 1) +
    labs(
      title = "PO data mean v predicted proportion of zeroes"
    )

  # check the mean number of ones vs the predicted mean in each sim
  ppc_stat(
    y = po_dat,
    yrep = po_pred,
    stat = function(x){mean(x == 1)},
    bins = 50
  )  +
    xlim(0, 1) +
    labs(title = "PO data mean v predicted proportion of ones")

  # check the mean number of values greater than one vs the predicted mean in each sim
  # they both should be zero
  ppc_stat(
    y = po_dat,
    yrep = po_pred,
    stat = function(x){mean(x > 1)},
    bins = 50
  )  +
    xlim(0, 1) +
    labs(title = "PO data mean v predicted proportion greater than one")

  ###### PA checks ########

  # check the mean number of ones vs the predicted mean in each sim
  ppc_stat(
    y = pa_dat,
    yrep = pa_pred,
    stat = function(x){mean(x == 1)},
    bins = 50
  )  +
    xlim(0, 1) +
    labs(title = "PA data mean v predicted proportion of ones")

  # check the mean number of zeroes vs the predicted mean in each sim
  ppc_stat(
    y = pa_dat,
    yrep = pa_pred,
    stat = function(x){mean(x == 0)},
    bins = 50
  ) +
    xlim(0, 1) +
    labs(title = "PA data mean v predicted proportion of zeroes")

  ###### Count checks ########

  # check the mean number of zeroes vs the predicted mean in each sim
  ppc_stat(
    y = count_dat,
    yrep = count_pred,
    stat = function(x){mean(x == 0)},
    bins = 50
  ) +
    xlim(0, 1) +
    labs(
      title = "count data mean v predicted propotrtion of zeroes"
    )

  # check the mean number of ones vs the predicted mean in each sim
  ppc_stat(
    y = count_dat,
    yrep = count_pred,
    stat = function(x){mean(x == 1)},
    bins = 50
  )  +
    xlim(0, 1) +
    labs(title = "count data mean v predicted proportion of ones")

  # check the mean number of values greater than one vs the predicted mean in each sim
  ppc_stat(
    y = count_dat,
    yrep = count_pred,
    stat = function(x){mean(x > 1)},
    bins = 50
  )  +
    xlim(0, 1) +
    labs(title = "count data mean v predicted proportion greater than one")


  # check the mean number of values greater than 1000 vs the predicted mean in each sim
  ppc_stat(
    y = count_dat,
    yrep = count_pred,
    stat = function(x){mean(x > 1000)},
    bins = 50
  )  +
    xlim(0, 1) +
    labs(title = "count data mean v predicted proportion greater than 1000")

  # count data density all of it
  ppc_dens_overlay(
    y = count_dat,
    yrep = count_pred
  ) +
    labs(title = "count data density all")

  # count data density truncated
  ppc_dens_overlay(
    y = count_dat,
    yrep = count_pred
  ) +
    xlim(0, 20) +
    labs(title = "count data density truncated")

  # count data ecdf truncated
  ppc_ecdf_overlay(
    y = count_dat,
    yrep = count_pred
  ) +
    xlim(0, 20) +
    labs(title = "count data ecdf truncated")


 # prepare count data for rootgrams
  count_rg_data <- rootgram_data(
    y = count_dat,
    yrep = count_pred,
    truncate = 1000
  )

  ppc_rootogram(
    y = count_rg_data$y,
    yrep = count_rg_data$yrep,
    style = "hanging",
    prob = 0.95,
    size = 1
  ) +
    xlim(0, 1000) +
    labs(title = "count data rootgram, prediction data truncated to 1000")

  ppc_rootogram(
    y = count_rg_data$y,
    yrep = count_rg_data$yrep,
    style = "hanging",
    prob = 0.95,
    size = 1
  ) +
    xlim(0, 20) +
    labs(title = "count data rootgram, prediction data truncated to 100, axis truncated")

}
