# Split `model_data_spatial` into the training and held-out sets for one fold.
#
# THE SINGLE SOURCE OF TRUTH FOR THE SPLIT. It is called in two places that must agree
# exactly -- inside the callr subprocess that fits the fold, and again at scoring time to
# work out what was held out -- so it lives in one function rather than being written twice.
#
# The split is by COORDINATE, never by row. Every record at a coordinate goes the same way,
# which is what makes it safe to hand the training frame straight to the unmodified
# production fit function: `distinct_idx` then picks the same physical row per retained
# coordinate as it would on the full data, so the design matrix and offsets a fold sees are
# bit-identical to the ones the full fit saw. cv_designmat() carries the evidence for that.
#
# Row order is preserved on both sides, because `distinct_idx` takes the FIRST row at each
# coordinate and reordering would change which row that is.
split_cv_fold <- function(
    model_data_spatial,
    cv_folds,
    fold,
    target_species = NULL
){

  if (is.character(cv_folds)) {
    cv_folds <- readr::read_csv(
      grep("cv_folds\\.csv$", cv_folds, value = TRUE),
      show_col_types = FALSE
    )
  }

  # joined on the 9 dp string key, never on the raw doubles -- see cv_coord_key()
  row_fold <- tibble::tibble(
    coord_key = cv_coord_key(
      model_data_spatial$latitude,
      model_data_spatial$longitude
    )
  ) |>
    left_join(
      cv_folds |>
        select(coord_key, fold),
      by = "coord_key"
    ) |>
    pull(fold)

  if (anyNA(row_fold)) {
    stop(
      sprintf(
        "split_cv_fold(): %i records have coordinates absent from cv_folds",
        sum(is.na(row_fold))
      ),
      call. = FALSE
    )
  }

  if (!fold %in% cv_folds$fold) {
    stop(
      sprintf(
        "split_cv_fold(): fold %s is not one of %s",
        fold,
        paste(sort(unique(cv_folds$fold)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  is_test <- row_fold == fold

  train <- model_data_spatial[!is_test, ]
  test <- model_data_spatial[is_test, ]

  ############
  # assertions. Leakage would invalidate the whole exercise without erroring, and the
  # species check guards a silent index misalignment: the fit derives n_species from the
  # data it is given (fit_..._reparam.R:114-115) but species_id is matched against the
  # passed-in target_species, so losing a species shrinks alpha/gamma_nb while the index
  # still reaches 18
  ############

  train_coords <- paste(train$latitude, train$longitude)
  test_coords <- paste(test$latitude, test$longitude)

  shared <- intersect(train_coords, test_coords)

  if (length(shared) > 0) {
    stop(
      sprintf(
        "split_cv_fold(): %i coordinates appear in BOTH train and test",
        length(shared)
      ),
      call. = FALSE
    )
  }

  if (nrow(test) == 0) {
    stop(
      sprintf(
        "split_cv_fold(): fold %s holds out no records",
        fold
      ),
      call. = FALSE
    )
  }

  if (sum(test$data_type == "count") == 0) {
    stop(
      sprintf(
        "split_cv_fold(): fold %s holds out no count records, so there is nothing to score",
        fold
      ),
      call. = FALSE
    )
  }

  if (!is.null(target_species)) {

    missing_species <- setdiff(
      target_species,
      unique(train$species)
    )

    if (length(missing_species) > 0) {
      stop(
        sprintf(
          "split_cv_fold(): fold %s drops %s from training entirely, which misaligns species_id",
          fold,
          paste(missing_species, collapse = ", ")
        ),
        call. = FALSE
      )
    }

  }

  list(
    train = train,
    test = test,
    fold = fold,
    n_train = nrow(train),
    n_test = nrow(test),
    n_test_count = sum(test$data_type == "count"),
    n_test_coords = length(unique(test_coords)),
    sum_bg_weight_train = sum(train$weight[train$data_type == "bg"])
  )

}
