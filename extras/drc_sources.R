raw_data |>
  select(
    source_id,
    citation_doi,
    author,
    article_title,
    journal_title,
    publication_year,
    country
  ) |>
  filter(country == "dr congo") |>
  distinct() |>
  write_csv(
    file = "extras/drc_sources.csv",
  )
