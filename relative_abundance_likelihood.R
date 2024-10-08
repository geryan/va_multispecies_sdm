library(tidyverse)
library(lme4)

file_url <- "https://www.dropbox.com/scl/fi/ijwppuf2sjrj4w01oaz36/intervention.csv?rlkey=9l92pdsypmis0d4m4kobj7sdn&dl=1"
filename <- tempfile()
download.file(url = file_url, destfile = filename)
data_raw <- read_csv(filename)
# data_raw <- read.csv("C:/.../intervention.csv")

strata <- c("TREAT_BASELINE",
            "CONTROL_BASELINE",
            "TREAT_POST",
            "CONTROL_POST")

data <- data_raw %>%
  # remove a redundant column
  select(-LONG_SPAT) %>%
  # remove a bunch of completely duplicated rows
  filter(
    !duplicated(.)
  ) %>%
  # add in record numbers for paired observations
  mutate(
    ID = row_number(),
    .after = PDF
  ) %>%
  # rename the estimates as such, so we can split on these
  rename_with(
    ~paste0(.x, "_ESTIMATE"),
    all_of(strata)
  ) %>%
  # split apart the Arm (Control or Treatment), PHASE (Baseline or
  # Post-intervention), and the Variable (Estimate or Number of samples)
  pivot_longer(
    cols = c(
      starts_with(strata[1]) |
        starts_with(strata[2]) |
        starts_with(strata[3]) |
        starts_with(strata[4])
    ),
    names_pattern = "(.*)_(.*)_(.*)",
    names_to = c("ARM", "PHASE", "VARIABLE"),
    values_to = "VALUE"
  ) %>%
  filter(
    # drop some missing values (either the estimate or number of samples)
    !is.na(VALUE),
    # drop those where the observation unit is not recorded
    !is.na(UNIT)
  ) %>%
  pivot_wider(
    names_from = "VARIABLE",
    values_from = "VALUE"
  ) %>%
  # put the data values for these in lower case, to be consistent with the others
  mutate(
    ARM = tolower(ARM),
    PHASE = tolower(PHASE),
  ) %>%
  # standardise the units into the total numbers of mosquitoes caught
  mutate(
    COUNT = case_when(
      # if it's a mean, multiply by number of samples to get the estimated count
      str_starts(UNIT, "mean") ~ round(ESTIMATE * NSAMP),
      # if it's n, n/day or count/month, it's a total count
      UNIT %in% c("n", "n/day", "count/month") ~ ESTIMATE
    ),
    .before = ESTIMATE
  ) %>%
  # for n/count data there's no NSAMP, it's just 1
  mutate(
    NSAMP = replace_na(NSAMP, 1)
  ) %>%
  # drop records with no count (means with unreported effort)
  filter(!is.na(COUNT)) %>%
  # drop the estimate column, now we have clean counts
  select(-ESTIMATE) %>%
  # combine some of the species information
  mutate(
    SPECIES = case_when(
      SPECIES == "gambiae ss (M)" ~ "gambiae",
      .default = SPECIES
    ),
    # tidy up mislabelled sampling method
    SAMPLING = case_when(
      SAMPLING == "winexit" ~ "WinExit",
      .default = SAMPLING
    )
  )

# prep data for modelling composition rates of dominant vectors before and after
# interventions
composition <- data %>%
  # flag whether a record is from samples taken after an intervention
  mutate(
    POST_INTERVENTION = ARM == "treat" & PHASE == "post"
  ) %>%
  # subset to the three (mutually-exclusive) dominant vector species, i.e. drop
  # gambiae complex, since it contains gambiae s.s. and arabiensis
  filter(
    SPECIES %in% c("funestus", "arabiensis", "gambiae")
  ) %>%
  # make these random levels factors
  mutate(
    PDF = factor(PDF),
    ID = factor(ID)
  )

composition_preintervention <- composition %>%
  # drop the post-intervention arm/phase data, keep only counts unaffected by
  # recent interventions
  filter(!POST_INTERVENTION) %>%
  select(
    SPECIES, SAMPLING, COUNT, NSAMP,
    # add in lat long
  ) %>%
  mutate(
    SAMPLING_INDEX = match(SAMPLING, unique(SAMPLING))
  )

# make this hierarchical, random effect on sampling method
sampling_effect <- normal(0, 1, dim = max(composition_preintervention$SAMPLING_INDEX))

# make it so that index pulls out the log lambda for the appropriate species at the lat/long corresponding to each of these rows
index <- ?

log_expected_count <- log_lambda[index] +
  log(composition_preintervention$NSAMP) +
  sampling_effect[composition_preintervention$SAMPLING_INDEX]
distribution(composition_preintervention$COUNT) <- exp(log_expected_count)


