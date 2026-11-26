# Shared by the overhaul checks in this folder.

# TRUE for objects that hash reproducibly: plain data, with no environment
# anywhere in them. greta arrays, the greta model, R6 objects and functions all
# carry environments, which serialise differently from one session to the next.
is_plain_data <- function(x) {

  if (is.environment(x) || is.function(x) || inherits(x, "greta_array")) {
    return(FALSE)
  }

  attribute_is_plain <- vapply(
    attributes(x),
    is_plain_data,
    logical(1)
  )

  if (!all(attribute_is_plain)) {
    return(FALSE)
  }

  if (is.list(x)) {
    element_is_plain <- vapply(
      x,
      is_plain_data,
      logical(1)
    )
    return(all(element_is_plain))
  }

  TRUE

}

# Digest every plain-data object in a fit image loaded into `fit`. The MCMC
# draws are left out: they are the result, not the data, and are several GB.
# Returns the per-object table and, for the model data, a hash per column so a
# mismatch can be traced to the column that changed.
digest_image <- function(
    fit,
    skip = c("draws", "m")
) {

  object_names <- sort(
    ls(
      fit,
      all.names = TRUE
    )
  )

  objects <- lapply(
    object_names,
    function(object_name) {

      x <- get(
        object_name,
        envir = fit
      )

      hashable <- !(object_name %in% skip) && is_plain_data(x)

      tibble::tibble(
        name = object_name,
        class = class(x)[1],
        bytes = as.numeric(object.size(x)),
        hash = if (hashable) rlang::hash(x) else NA_character_
      )

    }
  ) |>
    dplyr::bind_rows()

  model_data <- fit$model_data_spatial

  list(
    objects = objects,
    model_data_dim = dim(model_data),
    model_data_columns = vapply(
      as.list(model_data),
      rlang::hash,
      character(1)
    )
  )

}

# Old target name -> new target name, for the targets carried over from the
# _rep_cx section. Targets that kept their name are not listed.
overhaul_rename_map <- function(old_names) {

  special <- c(
    rep_tag_cx = "run_tag",
    rep_fit_image_cx = "fit_image",
    rep_pred_prefix_cx = "pred_prefix",
    rep_validation_dir_cx = "validation_dir",
    rep_plot_dir_cx = "plot_dir",
    model_fit_sre_rep_cx = "model_fit",
    resids_and_rhats_sre_rep_cx = "resids_and_rhats",
    preds_sm_rep_cx = "preds",
    plot_bioregion_slopes_rep_cx = "plot_bioregion_multipliers"
  )

  suffixed <- setdiff(
    grep(
      "_rep_cx$",
      old_names,
      value = TRUE
    ),
    names(special)
  )

  c(
    special,
    setNames(
      sub(
        "_rep_cx$",
        "",
        suffixed
      ),
      suffixed
    )
  )

}

# Functions renamed in R/ during the overhaul.
overhaul_function_map <- function() {
  c(
    fit_model_multispecies_pp_count_source_effect_reparam = "fit_model_multispecies_pp_count",
    predict_lambda_reparam = "predict_lambda"
  )
}

# Every target upstream of `nodes`, from a tar_network() edge table.
ancestors <- function(nodes, edges) {
  seen <- character()
  queue <- nodes
  while (length(queue)) {
    parents <- setdiff(
      unique(edges$from[edges$to %in% queue]),
      seen
    )
    seen <- c(seen, parents)
    queue <- parents
  }
  seen
}

# The plain-data objects in the fit image at `image_path` against those
# digested from the pre-overhaul image (`before_image`, from the snapshot).
# One row per object; `same` is NA where the object could not be hashed.
compare_image_to_snapshot <- function(
    image_path,
    before_image
) {

  fit <- new.env(parent = emptyenv())

  load(
    image_path,
    envir = fit
  )

  now <- digest_image(fit)

  objects <- dplyr::full_join(
    before_image$objects |>
      dplyr::select(
        name,
        class,
        hash_before = hash
      ),
    now$objects |>
      dplyr::select(
        name,
        hash_now = hash
      ),
    by = "name"
  ) |>
    dplyr::mutate(
      same = hash_before == hash_now
    )

  list(
    objects = objects,
    model_data_dim_same = identical(
      before_image$model_data_dim,
      now$model_data_dim
    ),
    model_data_columns_differing = names(before_image$model_data_columns)[
      before_image$model_data_columns != now$model_data_columns[names(before_image$model_data_columns)]
    ]
  )

}

# Named tables out of a fit image, without keeping the rest of it in memory.
image_tables <- function(
    image_path,
    table_names
) {

  fit <- new.env(parent = emptyenv())

  load(
    image_path,
    envir = fit
  )

  mget(
    table_names,
    envir = fit
  )

}

# Which columns of two same-shaped tables differ, on how many rows, and, where
# the table has a `data_type` column, on which data types.
table_differences <- function(
    before,
    now
) {

  if (!identical(dim(before), dim(now)) || !identical(names(before), names(now))) {
    return(
      tibble::tibble(
        column = "<different shape or column names>",
        rows_differing = NA_integer_,
        data_types = NA_character_
      )
    )
  }

  lapply(
    names(before),
    function(column) {

      a <- before[[column]]
      b <- now[[column]]

      differs <- !((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))

      tibble::tibble(
        column = column,
        rows_differing = sum(differs),
        data_types = if ("data_type" %in% names(before)) {
          paste(
            sort(unique(before$data_type[differs])),
            collapse = ", "
          )
        } else {
          NA_character_
        }
      )

    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::filter(rows_differing > 0)

}
