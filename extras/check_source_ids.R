# Checks of the sources and rows in the raw Vector Atlas data.
#
# Combines the main data file (va_data) with the new records file (va_new),
# then lists:
#   1. source_ids with more than one version of their source details, i.e.
#      more than one row per source_id in
#        all_raw_data |>
#          select(source_id, citation_doi, author, journal_title, publication_year) |>
#          distinct()
#   2. DOIs that appear under more than one source_id.
#   3. Article titles that appear under more than one source_id.
#   4. source_ids that are in both files.
#   5. Rows that are exact copies of another row.
# 2, 3 and 5 are possible errors only: one paper may have been entered as
# separate sources on purpose, and identical rows may be separate real records.
#
# Every table says which file(s) each source_id is in (in_file: va_data,
# va_new or both), and rows_va_data and rows_va_new give numbers of rows in
# each file.
#
# Values are compared as read_csv() reads them: leading and trailing spaces are
# stripped (as in the pipeline's raw_data), but case is kept, so "Smith" and
# "smith" count as different. DOIs and titles are the exception (see checks 2
# and 3).
#
# Results are printed at the end and written to outputs/source_id_checks/.

library(tidyverse)

options(width = 1000)

# read every column as character, so the files bind and no value is changed by
# parsing
va_data <- read_csv(
  file = "data/tabular/va.data_20261116.csv",
  col_types = cols(.default = col_character())
) |>
  mutate(file = "va_data")

va_new <- read_csv(
  file = "data/tabular/va.new.COMBINED_20261103.csv",
  col_types = cols(.default = col_character())
) |>
  mutate(file = "va_new")

all_raw_data <- bind_rows(
  va_data,
  va_new
)

# which file(s) each source_id is in, and its number of rows in each
source_id_files <- all_raw_data |>
  group_by(source_id) |>
  summarise(
    rows_va_data = sum(file == "va_data"),
    rows_va_new = sum(file == "va_new")
  ) |>
  mutate(
    in_file = case_when(
      rows_va_data > 0 & rows_va_new > 0 ~ "both",
      rows_va_data > 0 ~ "va_data",
      rows_va_new > 0 ~ "va_new"
    )
  )

source_id_in_file <- source_id_files |>
  select(
    source_id,
    in_file
  )


# 1. source_ids with more than one version of their source details -----------

# each distinct version of each source, with its number of rows in each file
source_versions <- all_raw_data |>
  group_by(
    source_id,
    citation_doi,
    author,
    journal_title,
    publication_year
  ) |>
  summarise(
    rows_va_data = sum(file == "va_data"),
    rows_va_new = sum(file == "va_new"),
    .groups = "drop"
  )

source_id_conflicts <- source_versions |>
  group_by(source_id) |>
  filter(n() > 1) |>
  ungroup() |>
  left_join(
    source_id_in_file,
    by = "source_id"
  ) |>
  arrange(source_id)

# one row per source_id and field that differs, with the values it takes
source_id_conflict_fields <- source_id_conflicts |>
  pivot_longer(
    c(
      citation_doi,
      author,
      journal_title,
      publication_year
    ),
    names_to = "field",
    values_to = "value"
  ) |>
  group_by(
    source_id,
    field
  ) |>
  filter(n_distinct(value) > 1) |>
  summarise(
    values = paste(
      unique(value),
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  left_join(
    source_id_files,
    by = "source_id"
  )


# 2. DOIs under more than one source_id ---------------------------------------

# some DOIs were entered with spaces in place of hyphens, so DOIs are matched on
# their letters and digits only, in lower case
doi_conflicts <- source_versions |>
  filter(!is.na(citation_doi)) |>
  mutate(
    doi_letters = citation_doi |>
      str_to_lower() |>
      str_remove_all("[^a-z0-9]")
  ) |>
  group_by(doi_letters) |>
  filter(n_distinct(source_id) > 1) |>
  ungroup() |>
  left_join(
    source_id_in_file,
    by = "source_id"
  ) |>
  arrange(
    doi_letters,
    source_id
  ) |>
  select(-doi_letters)


# 3. Titles under more than one source_id -------------------------------------

# the same title was often entered with different line breaks, spaces or
# hyphens, so titles are matched on their letters and digits only, in lower
# case. "personal communication" is used as a title by several sources, so it
# is left out
title_conflicts <- all_raw_data |>
  filter(
    !is.na(article_title),
    str_to_lower(article_title) != "personal communication"
  ) |>
  mutate(
    title_letters = article_title |>
      str_to_lower() |>
      str_remove_all("[^a-z0-9]")
  ) |>
  group_by(title_letters) |>
  filter(n_distinct(source_id) > 1) |>
  group_by(
    title_letters,
    source_id,
    citation_doi,
    author,
    publication_year,
    article_title
  ) |>
  summarise(
    rows_va_data = sum(file == "va_data"),
    rows_va_new = sum(file == "va_new"),
    .groups = "drop"
  ) |>
  left_join(
    source_id_in_file,
    by = "source_id"
  ) |>
  arrange(
    title_letters,
    source_id
  ) |>
  select(-title_letters) |>
  relocate(
    article_title,
    .after = last_col()
  )


# 4. source_ids in both files -------------------------------------------------

source_ids_in_both <- source_id_files |>
  filter(in_file == "both")


# 5. Duplicated rows ----------------------------------------------------------

# rows that are identical in every column (other than file): one row per set
# of identical rows, with its number of copies in each file
duplicated_rows <- all_raw_data |>
  group_by(across(-file)) |>
  filter(n() > 1) |>
  summarise(
    rows_va_data = sum(file == "va_data"),
    rows_va_new = sum(file == "va_new"),
    .groups = "drop"
  ) |>
  mutate(
    copies_in = case_when(
      rows_va_data > 0 & rows_va_new > 0 ~ "both",
      rows_va_data > 0 ~ "va_data",
      rows_va_new > 0 ~ "va_new"
    )
  )

# number of sets of identical rows per source_id, by where the copies are
duplicated_rows_by_source_id <- duplicated_rows |>
  count(
    source_id,
    copies_in,
    name = "sets_of_identical_rows"
  ) |>
  left_join(
    source_id_in_file,
    by = "source_id"
  )


# Write results ---------------------------------------------------------------

dir.create(
  "outputs/source_id_checks",
  showWarnings = FALSE,
  recursive = TRUE
)

write_csv(
  source_id_conflict_fields,
  file = "outputs/source_id_checks/1_source_id_conflict_fields.csv"
)

write_csv(
  source_id_conflicts,
  file = "outputs/source_id_checks/1_source_id_conflict_versions.csv"
)

write_csv(
  doi_conflicts,
  file = "outputs/source_id_checks/2_doi_conflicts.csv"
)

write_csv(
  title_conflicts,
  file = "outputs/source_id_checks/3_title_conflicts.csv"
)

write_csv(
  source_ids_in_both,
  file = "outputs/source_id_checks/4_source_ids_in_both_files.csv"
)

write_csv(
  duplicated_rows,
  file = "outputs/source_id_checks/5_duplicated_rows.csv"
)

write_csv(
  duplicated_rows_by_source_id,
  file = "outputs/source_id_checks/5_duplicated_rows_by_source_id.csv"
)


# Print results ---------------------------------------------------------------

cat(
  "\nRows in va_data:",
  nrow(va_data),
  "\nRows in va_new:",
  nrow(va_new),
  "\nRows with no source_id:",
  sum(is.na(all_raw_data$source_id)),
  "\nsource_ids:",
  nrow(source_id_files),
  "\n  only in va_data:",
  sum(source_id_files$in_file == "va_data"),
  "\n  only in va_new:",
  sum(source_id_files$in_file == "va_new"),
  "\n  in both:",
  sum(source_id_files$in_file == "both"),
  "\n\n1. source_ids with more than one version:",
  n_distinct(source_id_conflicts$source_id),
  "\n2. source_ids sharing a DOI with another source_id:",
  n_distinct(doi_conflicts$source_id),
  "\n3. source_ids sharing a title with another source_id:",
  n_distinct(title_conflicts$source_id),
  "\n4. source_ids in both files:",
  nrow(source_ids_in_both),
  "\n5. sets of identical rows:",
  nrow(duplicated_rows),
  "\n  copies all in va_data:",
  sum(duplicated_rows$copies_in == "va_data"),
  "\n  copies all in va_new:",
  sum(duplicated_rows$copies_in == "va_new"),
  "\n  copies in both files:",
  sum(duplicated_rows$copies_in == "both"),
  "\n\n"
)

cat("1. source_ids with more than one version: the fields that differ\n")
print(
  source_id_conflict_fields,
  n = Inf,
  width = Inf
)

cat("\n1. source_ids with more than one version: every version\n")
print(
  source_id_conflicts,
  n = Inf,
  width = Inf
)

cat("\n2. DOIs under more than one source_id\n")
print(
  doi_conflicts,
  n = Inf,
  width = Inf
)

cat("\n3. Titles under more than one source_id\n")
print(
  title_conflicts,
  n = Inf,
  width = Inf
)

cat("\n4. source_ids in both files\n")
print(
  source_ids_in_both,
  n = Inf,
  width = Inf
)

cat("\n5. Sets of identical rows, by source_id and where the copies are\n")
print(
  duplicated_rows_by_source_id,
  n = Inf,
  width = Inf
)
