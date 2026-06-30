
# regenerate model_data_spatial with source IDs

tar_load(full_data_records)
tar_load(target_species)
model_data_records <- generate_model_data_records(
    full_data_records,
    target_species = target_species
  )

tar_load(covariate_rast_5_all)
tar_load(project_mask_5)
record_data_spatial_all <- get_spatial_values(
  lyrs = covariate_rast_5_all,
  dat = model_data_records,
  #dat = model_data_records_ni,
  project_mask = project_mask_5
)

tar_load(target_covariate_names)
tar_load(subrealm_names)
tar_load(bioregion_names)
tar_load(soiltype_names)
tar_load(offset_names)
tar_load(bias_names)
record_data_spatial <- record_data_spatial_all |>
  select(
    - all_of(
      names(covariate_rast_5_all)[
        !names(covariate_rast_5_all) %in%
          c(
            target_covariate_names,
            subrealm_names,
            bioregion_names,
            soiltype_names,
            offset_names,
            bias_names
          )
      ]
    ),
  )

record_data_spatial_subsample <- record_data_spatial

tar_load(bg_kmeans_df)
model_data_spatial_no_offset <-
  bind_rows(
    record_data_spatial_subsample |>
      mutate(weight = 1),
    bg_kmeans_df |>
      mutate(
        data_type = "bg",
        presence = 0,
        n = 0
      )
  )

tar_load(offsets_5)
model_data_spatial <- match_offset_data(
  model_data_spatial_no_offset,
  offsets_5
)


# resids check

library(targets)
tar_load_globals()

resids <- read_csv("resids_for_checking.csv")
splocs <- read_csv("data/species_locations.csv")
sources <- read_csv("data/va_sources.csv")

sources_trimmed <- sources |>
  group_by(source_id) |>
  mutate(rn = row_number()) |>
  filter(rn == 1) |>
  select(-rn)

tar_load(full_data_records)
name_matches <- names(full_data_records)[names(full_data_records) %in% names(resids)]
name_matches


mds <- model_data_spatial |>
  select(
    source_id,
    !!name_matches
  ) |>
  filter(
    species == "arabiensis"
  ) |>
  distinct()


arabiensis_resids <- resids |>
  filter(
    species == "arabiensis",
    data_type != "bg",
    inferred == FALSE
  ) |>
  select(
    species,
    latitude,
    longitude,
    sampling_method,
    sampling_method_detailed,
    start_date,
    end_date,
    model_date,
    study_months,
    data_type,
    presence,
    count,
    n,
    residual,
    residual_norm
  ) |>
  left_join(
    mds
  ) |>
  left_join(
    sources_trimmed,
    by = "source_id"
  ) |>
  select(
    source_id,
    species,
    count,
    residual,
    residual_norm,
    longitude,
    latitude,
    citation_doi,
    author,
    article_title,
    journal_title,
    publication_year,
    sampling_method,
    sampling_method_detailed,
    start_date,
    end_date,
    model_date,
    study_months,
    data_type,
    presence
  ) |>
  group_by(source_id) |>
  mutate(
    maxresid = max(residual)
  ) |>
  arrange(
    desc(maxresid),
    desc(source_id)
  ) |>
  select(-maxresid)


arabiensis_resids |>
  print(n = 300)

write_csv(
  arabiensis_resids,
  "arabiensis_resids.csv"
)

ars_sources <- arabiensis_resids |>
  select(
    source_id,
    citation_doi,
    author,
    article_title,
    journal_title,
    publication_year#,
    #sampling_method,
  ) |>
  distinct()

write_csv(
  ars_sources,
  "arabiensis_resids_sources.csv"
)


plot(
  arabiensis_resids$residual_norm,
  arabiensis_resids$count
)


ggplot() +
  geom_spatraster(
    data = bioregion_mask_10
  ) +
  # scale_fill_viridis_c(
  #   option = "B",
  #   direction = 1,
  #   na.value = "transparent",
  # ) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    panel.grid.minor = element_line()
  )


sources |>
  group_by(source_id) |>
  mutate(n = n()) |>
  filter(n != 1)

