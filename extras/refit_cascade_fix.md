# Refitting `model_fit_sre_rep` does not update the prediction maps

Noted 2026-09-29. Applied 2026-10-14, in the pipeline overhaul that left one
model set. The text below is as written and uses the names of the time; in
`_targets.R` they are now `model_fit` (was `model_fit_sre_rep`),
`model_fit_image` (`model_fit_sre_rep_image`), `preds` (`preds_sm_rep`),
`resids_and_rhats` and `pred_prefix`. `coef_draws` reads the image target too.
Tested on a tiny real fit by `extras/overhaul_checks/03_smoke_pipeline.R`.

## Problem

After `model_fit_sre_rep` is refit, `tar_make(plot_pred_p_rep)` does not rerun
`preds_sm_rep` or the maps below it. The chain breaks in two places.

1. **The fit.** `model_fit_sre_rep` returns `rep_fit_image`, the image path.
   That string is the same after every refit, so the target's hash never
   changes. Neither `preds_sm_rep` nor `resids_and_rhats_sre_rep` sees the refit.
2. **The predictions.** `predict_lambda_reparam()` returns
   `list(lambda_no_offset, p, p_cv)`. These are paths built from
   `rep_pred_prefix`, and they are identical on every run. So even when
   `preds_sm_rep` reruns, its hash is unchanged, and
   `pred_dist_not_masked_rep`, `pred_lambda_rep` and `pred_cv_rep` are skipped.

targets only reruns a downstream target when an upstream **hash** changes. An
upstream target that rebuilds and returns the same value does not count.

The older `model_fit` -> `preds_sm` chain has the same problem. It is stale, so
it is left alone here.

## Fix

**1. Track the fit image by its contents.** Add this target next to
`model_fit_sre_rep`:

```r
 # tracks the fit image by its CONTENTS. model_fit_sre_rep returns the same
 # path after every refit, so its hash never changes and nothing downstream
 # reruns. A file target rehashes the image whenever it is rewritten (~3 s for
 # 6.5 GB) and passes the change on. Kept as its own target because adding
 # format = "file" to model_fit_sre_rep itself would invalidate the fit.
 tar_target(
   model_fit_sre_rep_image,
   model_fit_sre_rep,
   format = "file"
 ),
```

**2. Point both consumers at it.** In `resids_and_rhats_sre_rep`, change
`model_fit_image_multisp_pp_count_sm = model_fit_sre_rep` to
`= model_fit_sre_rep_image`. Then replace `preds_sm_rep` with:

```r
 # the output paths never change, so the files' md5s are added to the value;
 # without them a rerun returns an identical list and the maps never update.
 # preds_sm_rep$p etc. still work downstream
 tar_target(
   preds_sm_rep,
   {
     preds <- predict_lambda_reparam(
       image_name = model_fit_sre_rep_image,
       prediction_layer = covariate_rast_10, # use 10k for faster preds
       target_species,
       output_file_prefix = rep_pred_prefix,
       offset = offsets_avg_10,
       sm = TRUE, # if predict survey method
       nsims = 100 # lower for faster preds
     )
     c(
       preds,
       md5 = list(
         tools::md5sum(
           unlist(preds)
         )
       )
     )
   }
 ),
```

## Why not just `format = "file"`

- **On `model_fit_sre_rep`:** changing a target's storage format makes targets
  treat it as outdated. That means a full refit, about 10 h at 40 chains; the
  last one ran 36,053 s and finished 2026-09-29 04:38. The separate
  `model_fit_sre_rep_image` target gets file tracking without touching the fit.
  It has a second benefit: if the image is deleted, only this cheap target
  fails. The fit is not relaunched.
- **On `preds_sm_rep`:** a `format = "file"` target must be a plain character
  vector, and targets drops its names when downstream targets read it (checked:
  `x[["p"]]` gives "subscript out of bounds"). So `preds_sm_rep$p`,
  `$lambda_no_offset` and `$p_cv` would all break. Adding the md5s keeps the
  list, and those are the only three ways the target is used.

## What reruns when it is applied

- Does not rerun: `model_fit_sre_rep`.
- `model_fit_sre_rep_image`: a new target; it only hashes the image.
- `preds_sm_rep`: its command changed; about 4 min last time.
- `pred_dist_not_masked_rep`, `pred_p_rep`, `pred_lambda_rep`, `pred_cv_rep`
  and their plots.
- `resids_and_rhats_sre_rep`: its command changed.

## Tested

The test pipeline used stand-in functions with the same shape. The "fit"
writes an image and returns its fixed path. The "predict" writes three files
and returns a list of fixed paths. It had its own store, separate from this
project's.

| Scenario | Current code | With the fix |
|---|---|---|
| Refit with a different result | only the fit rebuilt; the map stayed stale | fit -> image -> preds -> map all rebuilt; the map shows the new fit |
| Switch an up-to-date pipeline to the new layout | -- | image target, preds and map rebuilt; fit not rebuilt |
| Rerun with nothing changed | nothing rebuilt | nothing rebuilt |

Hashing the real 6.5 GB image with targets' own file hash took 2.6 s.
