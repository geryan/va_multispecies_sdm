g <- full_data_records |>
  filter(species == "gambiae") |>
  filter(is.na(binary_absence) | binary_absence != "yes") |>
  filter(is.na(occurrence_n) | occurrence_n != 0)


vg <- vect(g)


plot(expert_maps |> filter(species == "gambiae"))
points(vg)


exids <- presences_outside_expert_range |>
  filter(species == "gambiae") |>
  as.data.frame(geom = "xy") |>
  as_tibble() |>
  filter((x > 20 & y > 0) | y > 15) |>
  pull(source_id) |>
  unique()


raw_data |>
  filter(source_id %in% exids) |>
  select(
    source_id,
    citation_doi,
    author,
    article_title,
    journal_title,
    publication_year
  ) |>
  distinct()


# ## gambiae outside expert range
#
# # Ethiopia / horn of Africa
# Tadesse   2021 ID 1000207 DOI 10.3201/eid2702.200019 - morph id gambiae in Ethiopia from standard ento surveil - remove
# Chanyalew 2022 ID 1001328 DOI 10.1186/s12936-022-04150-5 - gambiae s.l. - remove
# Jaleta    2022 ID 1001472 DOI 10.1002/vms3.941 - gambiae and arabiensis from morph id - remove
#
# # Sudan
# Ismail    2018 ID 1002196 DOI 10.1186/s13071-018-2732-9 - genetic ID two gambiae ss - waay outside range of other species - remove?
#
# # Niger Sahel
# Soumaila  2022 ID 1000130 DOI 10.1186/s12936-022-04410-4 - genetic ID of gambiae ss - ... keep?
#
