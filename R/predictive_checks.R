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
  ppc_dens_overlay(
    y = po_dat,
    yrep = po_pred
  )

  ppc_dens_overlay(
    y = po_dat,
    yrep = po_pred
  ) +
    xlim(0, 10)

  ppc_ecdf_overlay(
    y = po_dat,
    yrep = po_pred
  ) +
    xlim(0, 10)


  ppc_stat(
    y = po_dat,
    yrep = po_pred,
    stat = function(x){mean(x == 0)},
    bins = 50
  ) +
    xlim(0, 1)



  ###### PA checks ########

  ppc_bars(
    y = pa_dat,
    yrep = pa_pred
  )

  ppc_rootogram(
    y = pa_dat,
    yrep = pa_pred,
    prob = 0.9,
    style = "standing"
  )

  ppc_stat(
    y = pa_dat,
    yrep = pa_pred,
    stat = function(x){mean(x == 0)},
    bins = 50
  ) +
    xlim(0, 1)



  ###### Count checks ########
  ppc_dens_overlay(
    y = count_dat,
    yrep = count_pred
  )

  ppc_dens_overlay(
    y = count_dat,
    yrep = count_pred
  ) +
    xlim(0, 20)

  ppc_ecdf_overlay(
    y = count_dat,
    yrep = count_pred
  ) +
    xlim(0, 20)


  ppc_stat(
    y = count_dat,
    yrep = count_pred,
    stat = function(x){mean(x == 0)},
    bins = 50
  ) +
    xlim(0, 1)


  ppc_bars(
    y = count_dat,
    yrep = count_pred
  )


  rg_sample <- sample(
    x = length(count_dat),
    size = 100,
    replace = FALSE
  )

  ppc_rootogram(
    y = count_dat[rg_sample],
    yrep = count_pred[1:10,rg_sample],
    prob = 0.9,
    style = "hanging"
  )
  # Error in 0L:ymax : result would be too long a vector



}
