# Is there a field in the raw Vector Atlas data that measures the effort behind
# a count (trap-nights, person-nights, houses), which a count likelihood could
# use? Reads the raw csv only (the file `_targets.R` reads into `raw_data`).
#
#   Rscript extras/count_misfit/07_effort_fields.R
suppressPackageStartupMessages({
  library(data.table)
})

f <- "data/tabular/va.data_20260616.csv"
hdr <- names(
  fread(
    f,
    nrows = 0
  )
)
num <- function(x) suppressWarnings(as.numeric(x))

hbr_cols <- grep(
  "hbr",
  hdr,
  value = TRUE
)
cols <- c(
  "source_id",
  "species",
  "country",
  "occurrence_n_total",
  "occurrence_n_1",
  "study_sampling_design",
  "biting_number_of_sampling_nights_indoors",
  "biting_number_of_sampling_nights_outdoors",
  "indoor_biting_total",
  hbr_cols
)
d <- fread(
  f,
  select = intersect(
    unique(cols),
    hdr
  ),
  showProgress = FALSE
)

cat("rows:", nrow(d), "\n")
has_n <- !is.na(num(d$occurrence_n_total)) | !is.na(num(d$occurrence_n_1))
cat("rows with an occurrence count:", sum(has_n), "\n")

cat("\n== fields that could carry effort: rows filled, and rows filled that also have a count ==\n")
for (cc in intersect(c("biting_number_of_sampling_nights_indoors", "biting_number_of_sampling_nights_outdoors", "hbr_sampling_indoor", "indoor_biting_total"), names(d))) {
  v <- d[[cc]]
  ok <- !is.na(v) & v != ""
  cat(
    sprintf(
      "%-45s filled: %6d ; and a count: %6d\n",
      cc,
      sum(ok),
      sum(ok & has_n)
    )
  )
}

cat("\nstudy_sampling_design (rows with a count):\n")
print(
  head(
    sort(
      table(d$study_sampling_design[has_n]),
      decreasing = TRUE
    ),
    10
  )
)

cat("\n== human biting rate (HBR) ==\n")
cat("HBR columns:", hbr_cols, "\n")
vals <- intersect(c("indoor_hbr", "outdoor_hbr", "combined_hbr"), names(d))
any_val <- Reduce(`|`, lapply(vals, function(v) !is.na(num(d[[v]]))))
cat(
  "rows with a numeric HBR value:",
  sum(any_val),
  "; of which hbr_unit == hbr_night:",
  sum(any_val & d$hbr_unit == "hbr_night", na.rm = TRUE),
  "\n"
)
cat(
  "sources:",
  length(unique(d$source_id[any_val])),
  "; countries:",
  length(unique(d$country[any_val])),
  "\n"
)
cat("hbr_unit values (all rows):\n")
print(
  head(
    sort(
      table(d$hbr_unit),
      decreasing = TRUE
    ),
    8
  )
)
cat("species on the HBR rows:\n")
print(
  head(
    sort(
      table(d$species[any_val]),
      decreasing = TRUE
    ),
    12
  )
)
