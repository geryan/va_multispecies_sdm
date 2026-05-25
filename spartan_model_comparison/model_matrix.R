model_matrix <- tribble(
  ~model, ~micro_off, ~ temp_off, ~sub_main, ~subr_inter, ~bio_main, ~bio_int, ~soil_int,
       1,       TRUE,       TRUE,      TRUE,        TRUE,      TRUE,     TRUE,      TRUE,
       2,       TRUE,       TRUE,     FALSE,       FALSE,     FALSE,     TRUE,      TRUE,
       3,       TRUE,      FALSE,     FALSE,       FALSE,     FALSE,     TRUE,      TRUE,
       4,       TRUE,       TRUE,     FALSE,       FALSE,     FALSE,     TRUE,     FALSE,
       5,       TRUE,      FALSE,     FALSE,       FALSE,     FALSE,     TRUE,     FALSE,
       6,       TRUE,       TRUE,     FALSE,        TRUE,     FALSE,    FALSE,      TRUE,
       7,       TRUE,      FALSE,     FALSE,        TRUE,     FALSE,    FALSE,      TRUE,
       8,       TRUE,       TRUE,     FALSE,        TRUE,     FALSE,    FALSE,     FALSE,
       9,       TRUE,      FALSE,     FALSE,        TRUE,     FALSE,    FALSE,     FALSE

)
