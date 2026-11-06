# In-sample goodness of fit of the offset-strength ("os") model against the current
# model, by approximate leave-one-out cross-validation (PSIS-LOO; Vehtari, Gelman &
# Gabry 2017). Written 2026-10-19.
#
# Each record's log-likelihood at every posterior draw (os_record_loglik()) gives its
# expected log predictive density as if it had been left out of the fit, `elpd_loo`,
# without refitting. Higher is better. The models are compared record by record, as
# loo::loo_compare() does: the difference (os - current) is summed over records, and its
# standard error is sqrt(n * var(pointwise difference)). Reported overall, by data type,
# and by species within data type.
#
# Pareto k flags records where the approximation is unreliable (k > 0.7); they are
# counted, and kept in the sums. Background points carry very large weights, so their
# k is often high.
#
# The relative efficiency of the draws (`r_eff`) comes from the chain ids; where it cannot
# be computed (e.g. a likelihood that underflows to 0 at every draw) it is set to 1, as
# for independent draws.
#
# Records are processed one data type and one model at a time, in chunks, to bound memory.
# Writes three CSVs to `output_dir` and returns them as tibbles:
#   loo_by_type.csv       elpd of each model and the difference, by data type and overall
#   loo_by_species.csv    the same by species within data type
#   loo_pointwise.csv     every record's elpd and Pareto k under both models
os_compare_loo <- function(
    draws_file_current,
    draws_file_os,
    model_data_spatial,
    output_dir,
    chunk_size = 20000
){

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  pointwise_loo <- function(draws_file, data_type){

    ll <- os_record_loglik(
      draws_file = draws_file,
      model_data_spatial = model_data_spatial,
      data_types = data_type
    )

    loglik <- ll[[data_type]]$loglik
    chain <- ll$chain
    n_rec <- ncol(loglik)

    chunks <- split(
      seq_len(n_rec),
      ceiling(seq_len(n_rec) / chunk_size)
    )

    res <- lapply(
      chunks,
      function(cols){

        ll_chunk <- loglik[, cols, drop = FALSE]

        r_eff <- suppressWarnings(
          loo::relative_eff(
            exp(ll_chunk),
            chain_id = chain
          )
        )
        r_eff[!is.finite(r_eff)] <- 1

        fit_loo <- suppressWarnings(
          loo::loo(
            ll_chunk,
            r_eff = r_eff
          )
        )

        tibble::tibble(
          elpd_loo = fit_loo$pointwise[, "elpd_loo"],
          pareto_k = fit_loo$diagnostics$pareto_k
        )

      }
    )

    dplyr::bind_cols(
      ll[[data_type]]$records,
      dplyr::bind_rows(res)
    )

  }

  pointwise <- lapply(
    c("count", "pa", "pobg"),
    function(dt){

      current <- pointwise_loo(draws_file_current, dt)
      os <- pointwise_loo(draws_file_os, dt)

      # the same records in the same order, or the differences mean nothing
      stopifnot(
        identical(
          current |> select(-elpd_loo, -pareto_k),
          os |> select(-elpd_loo, -pareto_k)
        )
      )

      current |>
        rename(
          elpd_current = elpd_loo,
          k_current = pareto_k
        ) |>
        mutate(
          elpd_os = os$elpd_loo,
          k_os = os$pareto_k
        )

    }
  ) |>
    bind_rows() |>
    mutate(
      likelihood = case_when(
        data_type == "count" ~ "count",
        data_type == "pa" ~ "presence-absence",
        .default = "presence-only + background"
      ),
      diff = elpd_os - elpd_current
    )

  summarise_diff <- function(x){
    x |>
      summarise(
        records = n(),
        elpd_current = sum(elpd_current),
        elpd_os = sum(elpd_os),
        elpd_diff = sum(diff),
        se_diff = sqrt(n() * stats::var(diff)),
        z = elpd_diff / se_diff,
        k_over_0.7_current = sum(k_current > 0.7),
        k_over_0.7_os = sum(k_os > 0.7),
        .groups = "drop"
      )
  }

  by_type <- bind_rows(
    pointwise |>
      group_by(likelihood) |>
      summarise_diff(),
    pointwise |>
      mutate(likelihood = "all") |>
      group_by(likelihood) |>
      summarise_diff()
  )

  by_species <- pointwise |>
    group_by(
      likelihood,
      species
    ) |>
    summarise_diff() |>
    arrange(
      likelihood,
      desc(elpd_diff)
    )

  readr::write_csv(
    by_type,
    file.path(output_dir, "loo_by_type.csv")
  )

  readr::write_csv(
    by_species,
    file.path(output_dir, "loo_by_species.csv")
  )

  readr::write_csv(
    pointwise,
    file.path(output_dir, "loo_pointwise.csv")
  )

  list(
    by_type = by_type,
    by_species = by_species,
    pointwise = pointwise
  )

}
