#' The data summary table as a Word document, for the manuscript
#'
#' Writes the records of each species by data type
#' (`make_data_summary_table()`) with the totals over all species
#' (`make_data_summary_totals()`) as its last row. Above the table is its
#' caption, with the key written out as prose; below it, the key as a table.
#' A4 portrait, 2 cm margins, Arial, booktabs-style rules.
#'
#' @title Data summary table docx
#' @param summary_table one row per species, from `make_data_summary_table()`
#' @param totals one row, from `make_data_summary_totals()`
#' @param file path of the .docx to write
#' @return `file`
#' @author geryan
#' @export
make_data_summary_docx <- function(
    summary_table,
    totals,
    file
) {

  n_species <- nrow(summary_table)

  tab <- bind_rows(
    summary_table,
    totals
  )

  # widths in DXA (1/1440 in), from Arial metrics at 8 pt: narrower columns
  # break the labels mid-word. They total 9600, against 9638 between margins
  widths <- c(
    species = 1400,
    count = 620,
    count_p = 760,
    count_0 = 700,
    count_imp = 780,
    pa = 620,
    pa_1 = 760,
    pa_0 = 700,
    pa_imp = 780,
    po = 960,
    p_n = 620,
    p_unique = 900
  )

  rule_outer <- officer::fp_border(
    color = "black",
    width = 1
  )
  rule_inner <- officer::fp_border(
    color = "black",
    width = 0.5
  )

  ft <- flextable::flextable(
    tab,
    col_keys = names(widths)
  ) |>
    flextable::set_header_labels(
      values = list(
        species = "Species",
        count = "n",
        count_p = "present",
        count_0 = "absent",
        count_imp = "inferred",
        pa = "n",
        pa_1 = "present",
        pa_0 = "absent",
        pa_imp = "inferred",
        po = "n",
        p_n = "n",
        p_unique = "locations"
      )
    ) |>
    flextable::add_header_row(
      values = c(
        "",
        "Count",
        "Presence-absence",
        "Presence-only",
        "All presences"
      ),
      colwidths = c(
        1,
        4,
        4,
        1,
        2
      ),
      top = TRUE
    ) |>
    flextable::colformat_int(
      big.mark = ","
    ) |>
    format_summary_flextable(
      rule_outer = rule_outer,
      rule_inner = rule_inner
    ) |>
    flextable::hline(
      i = 1,
      j = 2:12,
      border = rule_inner,
      part = "header"
    ) |>
    # the rule above the totals row
    flextable::hline(
      i = n_species,
      border = rule_inner,
      part = "body"
    ) |>
    flextable::align(
      j = 1,
      align = "left",
      part = "all"
    ) |>
    flextable::align(
      j = 2:12,
      align = "right",
      part = "all"
    ) |>
    flextable::align(
      i = 1,
      align = "center",
      part = "header"
    ) |>
    flextable::italic(
      i = seq_len(n_species),
      j = 1,
      part = "body"
    ) |>
    flextable::width(
      j = names(widths),
      width = widths / 1440,
      unit = "in"
    )

  key <- data_summary_key()

  key_widths <- c(
    2700,
    1150,
    9638 - 2700 - 1150
  )

  ft_key <- flextable::flextable(key) |>
    format_summary_flextable(
      rule_outer = rule_outer,
      rule_inner = rule_inner
    ) |>
    flextable::align(
      align = "left",
      part = "all"
    ) |>
    flextable::valign(
      valign = "top",
      part = "body"
    ) |>
    flextable::font(
      j = "Variable",
      fontname = "Courier New",
      part = "body"
    ) |>
    flextable::width(
      width = key_widths / 1440,
      unit = "in"
    )

  doc <- officer::read_docx() |>
    officer::body_set_default_section(
      officer::prop_section(
        page_size = officer::page_size(
          orient = "portrait",
          width = 11906 / 1440,
          height = 16838 / 1440
        ),
        page_margins = officer::page_mar(
          top = 1134 / 1440,
          bottom = 1134 / 1440,
          left = 1134 / 1440,
          right = 1134 / 1440,
          header = 0.5,
          footer = 0.5,
          gutter = 0
        ),
        type = "continuous"
      )
    ) |>
    officer::body_add_fpar(
      data_summary_caption(n_species)
    ) |>
    flextable::body_add_flextable(
      ft,
      align = "left"
    ) |>
    officer::body_add_fpar(
      officer::fpar(
        summary_run(
          "Key to Table 1.",
          bold = TRUE
        ),
        fp_p = officer::fp_par(
          padding.top = 12,
          padding.bottom = 5,
          keep_with_next = TRUE
        )
      )
    ) |>
    flextable::body_add_flextable(
      ft_key,
      align = "left"
    )

  dir.create(
    dirname(file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  print(
    doc,
    target = file
  )

  file

}

# the rules, type and spacing both tables share
format_summary_flextable <- function(
    ft,
    rule_outer,
    rule_inner
) {

  ft |>
    flextable::border_remove() |>
    flextable::hline_top(
      border = rule_outer,
      part = "header"
    ) |>
    flextable::hline_bottom(
      border = rule_inner,
      part = "header"
    ) |>
    flextable::hline_bottom(
      border = rule_outer,
      part = "body"
    ) |>
    flextable::bold(
      part = "header"
    ) |>
    flextable::font(
      fontname = "Arial",
      part = "all"
    ) |>
    flextable::fontsize(
      size = 8,
      part = "all"
    ) |>
    flextable::valign(
      valign = "bottom",
      part = "all"
    ) |>
    flextable::padding(
      padding.top = 1.5,
      padding.bottom = 1.5,
      padding.left = 3.5,
      padding.right = 3.5,
      part = "all"
    ) |>
    flextable::line_spacing(
      space = 1,
      part = "all"
    ) |>
    # flextable's default minimum row height (0.25 in) is taller than an 8 pt
    # row: Word ignores it, but Pages and Quick Look draw the rows that tall
    flextable::height_all(
      height = 0.1,
      part = "all"
    ) |>
    flextable::set_table_properties(
      layout = "fixed",
      opts_word = list(
        split = FALSE,
        keep_with_next = TRUE,
        repeat_headers = TRUE
      )
    )

}

# a run of caption text: Arial 10 pt
summary_run <- function(
    text,
    bold = FALSE,
    italic = FALSE
) {

  officer::ftext(
    text,
    prop = officer::fp_text(
      font.family = "Arial",
      font.size = 10,
      bold = bold,
      italic = italic
    )
  )

}

# the table's caption, with the key written out as prose
data_summary_caption <- function(n_species) {

  officer::fpar(
    summary_run(
      "Table 1.",
      bold = TRUE
    ),
    summary_run(" Records of each "),
    summary_run(
      "Anopheles",
      italic = TRUE
    ),
    summary_run(
      paste0(
        " species in the model data by data type, with the totals over all ",
        n_species,
        " species in the last row. ",
        "A record is a species recorded in the raw data, or an absence ",
        "inferred for a species not recorded at a location-date (source × ",
        "sampling method × detailed method × location × start date × ",
        "duration) where other species were. ",
        "Count: records with a count of the species (n), with a count above ",
        "0 (present) or of 0, recorded or inferred (absent), and, of the ",
        "absent, those inferred where every species recorded at the ",
        "location-date had a count, under a sampling method other than ",
        "“other” (inferred). ",
        "Presence-absence: records of the species’ presence or absence (n), ",
        "recorded present (present) or absent, recorded or inferred ",
        "(absent), and, of the absent, those inferred where not every ",
        "record at the location-date had a count, or the sampling method ",
        "was “other” (inferred). Counted records under the “other” method ",
        "are kept as presence-absence. ",
        "Presence-only: presence-only records (n). ",
        "All presences: the presence records of all three types (n), and ",
        "their distinct locations, as latitude–longitude pairs (locations). ",
        "In the last row the record columns are sums over species, but each ",
        "location counts once whichever species were recorded there, so its ",
        "locations are fewer than the sum of the column. ",
        "Background points are left out, and so are inferred absences of a ",
        "species at locations where it, or a complex containing it, was ",
        "recorded present at any time."
      )
    ),
    fp_p = officer::fp_par(
      padding.bottom = 5,
      line_spacing = 1.15,
      keep_with_next = TRUE
    )
  )

}

# the key to the table's columns
data_summary_key <- function() {

  tribble(
    ~Column, ~Variable, ~Meaning,
    "Count: n", "count",
    "Records with a count of the species, zeros included.",
    "Count: present", "count_p",
    "Count above 0.",
    "Count: absent", "count_0",
    "Count of 0, recorded or inferred.",
    "Count: inferred", "count_imp",
    paste0(
      "Of the absent, the inferred zeros: the species was not recorded at a ",
      "location-date where every species recorded had a count (not under ",
      "the “other” method)."
    ),
    "Presence-absence: n", "pa",
    paste0(
      "Presence-absence records. Includes counted records under the ",
      "“other” sampling method, which are kept as presence-absence."
    ),
    "Presence-absence: present", "pa_1",
    "Recorded present.",
    "Presence-absence: absent", "pa_0",
    "Absent, recorded or inferred.",
    "Presence-absence: inferred", "pa_imp",
    paste0(
      "Of the absent, the inferred absences: the species was not recorded ",
      "at a location-date where other species were, and not every record ",
      "there had a count, or the method was “other”."
    ),
    "Presence-only: n", "po",
    "Presence-only records.",
    "All presences: n", "p_n",
    "count_p + pa_1 + po.",
    "All presences: locations", "p_unique",
    paste0(
      "Distinct latitude–longitude pairs among those presence records. In ",
      "the All species row each location counts once, whichever species ",
      "were recorded there, so it is less than the sum of the column."
    )
  )

}
