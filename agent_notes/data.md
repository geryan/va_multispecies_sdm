# Records, data types and summary tables

Read when working on records, data types, inferred zeros, `_cx`, the data
summary tables or the manuscript numbers.

## Chain

`raw_data` (`data/tabular/va.data_20260616.csv`) → `full_data_records`
(`clean_full_data_records()`, `clean_species()`) → `model_data_records`
(`generate_model_data_records()`: data types and inferred zeros) →
`model_data_spatial` (background points added; offset matched on year-month,
land cover and footprint on each record's year: `covariates.md`) →
`model_data_spatial_cx` (`drop_contradicted_inferred_zeros()`), the model's
data.

## Cleaning (`clean_full_data_records()`)

- `reduce_sampling_methods()` folds larvae, `human_baited_net_unk` / `_ind` /
  `_out`, `window_exit`, `odour`, `animal_baited_net` and `tent_trap` into
  "other". The original method stays in `sampling_method_detailed`.
- `filter(!ic_ki, !prob_bioassay)` (~line 265) drops rows with bioassay results
  (`mosquitoes_tested_n`, `mosquitoes_dead_n` or `percent_mortality`) whatever
  the method. Comments there and in `_targets.R` (~line 1167) call it "larval
  bioassay" data. **Larval sampling is not excluded** (checked 2026-11-20; the
  user thought it was): of 13,496 larval rows it drops 5,914 and keeps 7,582 (rerun without the
  filter, its kept rows matched `full_data_records`, 44,438).
- `clean_species()` maps "funestus-like" and "rivulorum-like" with hyphens; the
  data spell them with spaces. Not fixed: fixing it invalidates everything
  downstream, including the fit.

## Data types (`generate_model_data_records()`)

- Set per **location-date**: source × sampling method × detailed method ×
  location × start date × duration, the same grouping zeros are inferred in.
  New rules use this grouping (the user: "be consistent with previous grouping
  for inference").
- A row with its own count is a count, zeros included (the user's choice).
- Where some species were counted and others recorded without a count, the
  uncounted rows (those presences and the inferred zeros) are presence-absence:
  the `full_count` rule in the `data_type` `case_when` (since 2026-11-06).
  Where everything recorded was counted, inferred zeros are zero counts.
- Counts under the "other" method are kept only as presence-absence (open item
  below).
- The user's filter at the end (2026-11-21, uncommitted, reason not recorded)
  drops every coluzzii record from sources 2676 and 1001743.

**Why the `full_count` rule** (fix of 2026-11-06; refitted 2026-11-07): before
it, species recorded present at a location-date where others were counted, but
with no count of their own (`occurrence_n` NA), were made counts with
`count = 0`, so the fit saw a zero catch for a species that was there. That
was 1,113 rows in `model_data_spatial_cx`, 32% of the present count rows
(arabiensis 359, funestus 294, pharoensis 176, gambiae 65 the most). The fix
moved 1,115 presences to pa and 6,063 inferred zeros to pa 0, at 437
location-dates from 67 sources (30 of them at 2 location-dates where the
uncounted row is a recorded absence). In `model_data_records`, counts went from
26,677 / 3,603 (absent / present) to 20,614 / 2,488, and pa from 77,675 / 11,484
to 83,738 / 12,599. Test: `extras/check_uncounted_pa.R`. Don't check greta data
with `calculate()`: it simulates them.

## Contradicted inferred zeros (`model_data_spatial_cx`)

- Drops inferred zeros for a species at locations where that species, or a
  complex or group containing it, was recorded present at any time. Membership:
  `complex_member_defs()`, from Harbach's Mosquito Taxonomic Inventory (2024)
  and WRBU. It dropped 16,503 of 96,270 inferred rows (2026-10).
- Keeps its `_cx` name because `model_data_spatial` still exists as its parent.
  Some side targets (novelty maps, older tables and plots) read
  `model_data_spatial`.
- `_noinf` (all inferred rows dropped) was fitted and removed earlier; it
  showed the inferred zeros matter, especially for species with few data.

## Current numbers (from the store, 2026-11-24)

These include the coluzzii source filter.

- `model_data_spatial_cx`: counts 15,904 absent / 2,436 present; pa 66,868 /
  12,127; po 3,468; background 249. No present count row has a count of 0.
- `data_summary_totals_cx`: 100,803 records = counts 18,340 + pa 78,995 +
  po 3,468.
  - counts: 2,436 above 0, 1,961 recorded zeros, 13,943 inferred;
  - pa: 12,127 present, 1,062 recorded absent, 65,806 inferred;
  - 21,054 records not inferred; `p_n` 18,031; `p_unique` 3,321.
- **The numbers given to the user on 2026-11-20 for the manuscript predate the
  filter:** 100,832 records; counts 18,348 (2,437 / 1,968 / 13,943); pa 79,016
  (12,130 / 1,062 / 65,824); 21,065 not inferred; `p_unique` 3,324 (the column
  sum over species was 6,297). The filter removed 29 coluzzii records: 8 counts
  (1 present, 7 recorded zeros) and 21 pa (3 present, 18 inferred absences).
- Present records by species (`p_n`), 2026-11-24: arabiensis 4,804, funestus
  3,193, gambiae 2,947, coluzzii 2,032 (2,036 before the filter); under 200:
  leesoni 134, pretoriensis 142, rivulorum 142, maculipalpis 145, merus 157,
  quadriannulatus 199. The user's data paragraph was updated to these numbers
  on 2026-11-24.
- Wording: the user's draft says "imputed", the docx says "inferred". A count
  of 1 is present, so say "above 0", not ">1".

## Summary tables

- `data_summary_table_cx` (+ `_csv`), `make_data_summary_table()`, per species:
  `count`, `count_p`, `count_0`, `count_imp`, `pa`, `pa_1`, `pa_0`, `pa_imp`,
  `po`, `p_n` (all present records), `p_unique` (distinct latitude-longitude
  pairs of those). `count_p`, `pa_1`, `p_n`, `p_unique` were added 2026-11-06.
- `data_summary_totals_cx` (`make_data_summary_totals()`): one row, "All
  species": `make_data_summary_table()` run with every record given one species,
  so the definitions can't drift. Record columns are sums over species;
  `p_unique` counts each location once.
- `data_summary_table_cx_docx` (`make_data_summary_docx()`):
  `outputs/tables/data_summary_table_cx.docx`, for the manuscript. Caption (the
  key written out as prose), the table with the totals as its last row, then
  the key as a table. A4, Arial, booktabs rules, layout from the 2026-11-19 Node
  prototype (`outputs/tables/docx_prototype/`). Rows have a 0.1 in minimum
  height (flextable's 0.25 in default looked tall); "Presence-only" wraps in its
  header cell. Needs officer and flextable, called with `::` and not in
  `tar_option_set(packages)`: installed on the laptop (0.7.9, 0.10.4), not on
  Spartan. The
  earlier options docx (full table plus options A–D) is
  `data_summary_table_cx_options.docx`.
- Key definitions: `count_imp` = not recorded, at a location-date where every
  recorded species had a count and the method is not "other"; `pa_imp` = not
  recorded, where not every record had a count or the method was "other"
  (inferred absences under "other" are always pa).
- The roxygen of `make_data_summary_table()` still says a count record can be
  present with a count of 0. Wrong since 2026-11-06; the user knows; leave it.
- A record is not one species per location-date: 7,897 rows share a species ×
  location-date key (2026-11-19; cause not investigated).

## Open: counts under the "other" method are thrown away

Found 2026-11-09. The user: "these should still be counts regardless", to
revisit. Don't change it unprompted; the rule is the user's decision.

- **The code:** `generate_model_data_records()` ~line 71,
  `!is.na(occurrence_n) & sampling_method %in% c("larvae", "other") ~ "pa"`,
  makes a counted row pa (presence = count > 0); its roxygen says "except for
  larvae and "other" methods". Lines ~173–174 (`count` NA for larvae/other
  count rows) never fire: every row at a location-date has the same method.
  Both from 3032ecf (2025-07-25). "larvae" never matches, because cleaning
  already folded it into "other", so the rule applies to all of "other".
- **What it throws away** (recorded target-species rows after the georef
  filters): "other" has 7,894 rows, 1,891 with a count (619 zeros, 1,272 above
  0). With a count, by detailed method: larvae 1,009 (max 1,774), other 168,
  odour 163 (max 13,498), human_baited_net_unk 151, animal_baited_net 143,
  tent_trap 122, window_exit 107 (max 200,170), human_baited_net_out 20,
  human_baited_net_ind 8. In `model_data_spatial_cx` (2026-11-09), "other" is pa
  734 recorded absences, 5,716 presences, 32,626 inferred absences, and po
  1,121; no counts.
- **Larval records** (2026-11-20, before the coluzzii filter): 23,481 of 100,832
  model records have `sampling_method_detailed == "larvae"`: pa present 3,211,
  recorded absent 335, inferred absent 19,545 (of the 65,824 `pa_imp`), po 390,
  no counts.
- **To decide before changing it:**
  - one `sampling_re` would cover larval dips, window-exit and odour traps and
    baited nets (the per-source `zeta` absorbs some of it); larvae, and maybe
    others, could get their own method by taking them out of
    `reduce_sampling_methods()`;
  - larval counts are larvae per dip or site, not adults per trap-night;
  - under the `full_count` rule, inferred zeros at fully counted "other"
    location-dates would become zero counts;
  - check the window-exit count of 200,170 first;
  - any change reruns `model_data_records` onward, including the refit.

## New raw data (2026-11): not in the pipeline yet

`va.data_20261116.csv` replaces `va.data_20260616.csv` (`raw_data_file` still
points to the old one); `va.new.COMBINED_20261103.csv` adds records (here
"va_new"). `extras/check_source_ids.R` reads both and writes
`outputs/source_id_checks/*.csv` for the data team (run 2026-11-25). Findings:

- 13 source_ids have more than one version of (DOI, author, journal, year),
  none of them only in va_data: drag-filled DOIs (1002380, 1390), typos (1332,
  1226), year 2007 vs 2008 (2952), two different papers (1003565), and fields
  blank in some rows (2429, 2465, 2484, 3478, 3734, 4164).
- DOIs and titles shared across source_ids (matched on lower-case letters and
  digits, since entries differ in line breaks, spaces and hyphens): 55 and 58
  source_ids. Some pairs look like the same paper entered twice, others like a
  wrong DOI or title (e.g. 3874 titled "annual report for the year 1999" but
  dated 1986).
- 239 source_ids are in both files; 5,455 sets of rows identical in all
  columns (3,899 within va_data, 1,243 within va_new, 313 across files). The
  pipeline does not drop identical rows, so the va_data copies are probably in
  the current fit, unless they are real separate records.
- Before switching `raw_data_file`: decide how to combine the files without
  double counting, and apply the data team's fixes. Any change to the raw data
  reruns everything down to the fit.

## Records worth checking

- funestus 342,645 in one indoor CDC light trap (Mozambique, 2020-05), against
  an expected ~1: possibly a pooled count.
- window_exit 200,170 (an "other" count, used only as a presence).
- ziemanni in Morocco: 11 presence-only locations from one source (1002703),
  each entered twice (indoor HLC and "other"). ziemanni is not usually reported
  from North Africa: check the identification.
