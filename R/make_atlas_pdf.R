#' Bind atlas pages into one PDF with a clickable table of contents
#'
#' Writes a LaTeX document with one page per map file and compiles it with
#' xelatex (TinyTeX). hyperref makes the table of contents and cross-references
#' clickable and fills the PDF's bookmark sidebar; every page footer links back
#' to the contents.
#'
#' The structure is read from the page file names, as written by
#' render_species_pages() and make_country_pages():
#'
#' - type "species": `<species>__<layer>.pdf`. One section per species, one
#'   subsection per map; the printed contents list both levels
#' - type "country": `<ISO3>__<species>__<layer>.pdf`, `<ISO3>__traveltime.pdf`
#'   and `<ISO3>__nodata.pdf`. One section per country, opening with a
#'   clickable index of its species; a subsection per species (and one for
#'   travel time) with a subsubsection per map. The printed contents list
#'   countries only -- all three levels would run to ~20 pages -- while the
#'   bookmark sidebar holds all three
#'
#' @param pages paths of the page files, PDF
#' @param file path of the atlas to write
#' @param type "species" or "country"
#' @param title,subtitle for the title page
#' @param countries for type "country": tibble with `iso3` and `country`, in
#'   the order the countries should appear
#' @return `file`
#' @author geryan
#' @export
make_atlas_pdf <- function(
    pages,
    file,
    type = c("species", "country"),
    title = "Anopheles atlas",
    subtitle = NULL,
    countries = NULL
){

  type <- match.arg(type)

  pages <- normalizePath(pages)

  if (any(grepl(" ", pages))) {
    stop(
      "make_atlas_pdf(): page paths must not contain spaces",
      call. = FALSE
    )
  }

  layer_title <- c(
    abundance = "Relative abundance",
    distribution = "Probability of occurrence",
    cv = "Uncertainty: CV of the probability of occurrence",
    points = "Probability of occurrence, with survey records"
  )

  layer_order <- names(layer_title)

  parts <- strsplit(
    tools::file_path_sans_ext(basename(pages)),
    "__",
    fixed = TRUE
  )

  species_heading <- function(sp){
    sprintf(
      "\\texorpdfstring{\\textit{An. %s}}{An. %s}",
      sp,
      sp
    )
  }

  image <- function(path){
    c(
      "\\begin{center}",
      sprintf(
        "\\includegraphics[width=\\linewidth,height=0.82\\textheight,keepaspectratio]{%s}",
        path
      ),
      "\\end{center}",
      "\\clearpage"
    )
  }

  body <- if (type == "species") {

    tab <- data.frame(
      path = pages,
      species = vapply(parts, `[`, "", 1),
      layer = vapply(parts, `[`, "", 2)
    )

    check_layers(tab$layer, layer_order)

    unlist(
      lapply(
        unique(tab$species),
        function(sp){
          sp_tab <- tab[tab$species == sp, ]
          sp_tab <- sp_tab[order(match(sp_tab$layer, layer_order)), ]
          c(
            sprintf(
              "\\section{%s}\\label{%s}",
              species_heading(sp),
              sp
            ),
            unlist(
              lapply(
                seq_len(nrow(sp_tab)),
                function(i){
                  c(
                    sprintf(
                      "\\subsection{%s}",
                      layer_title[[sp_tab$layer[i]]]
                    ),
                    image(sp_tab$path[i])
                  )
                }
              )
            )
          )
        }
      )
    )

  } else {

    if (is.null(countries)) {
      stop(
        "make_atlas_pdf(): type = \"country\" needs `countries`",
        call. = FALSE
      )
    }

    tab <- data.frame(
      path = pages,
      iso3 = vapply(parts, `[`, "", 1),
      # the second part is the species, or "traveltime" / "nodata"
      species = vapply(parts, `[`, "", 2),
      layer = vapply(parts, function(p) if (length(p) == 3) p[3] else NA_character_, "")
    )

    check_layers(tab$layer[!is.na(tab$layer)], layer_order)

    unknown <- setdiff(tab$iso3, countries$iso3)

    if (length(unknown)) {
      stop(
        sprintf(
          "make_atlas_pdf(): pages for countries not in `countries`: %s",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    unlist(
      lapply(
        intersect(countries$iso3, tab$iso3),
        function(iso){

          ct <- tab[tab$iso3 == iso, ]
          name <- tex_escape(countries$country[countries$iso3 == iso])

          nodata <- ct$path[ct$species == "nodata"]
          traveltime <- ct$path[ct$species == "traveltime"]

          sp_tab <- ct[!is.na(ct$layer), ]
          spp <- unique(sp_tab$species)

          index <- c(
            "\\noindent Maps for this country:",
            "\\begin{itemize}",
            sprintf(
              "\\item \\hyperref[%s-%s]{\\textit{An. %s}}",
              iso,
              spp,
              spp
            ),
            if (length(traveltime)) {
              sprintf(
                "\\item \\hyperref[%s-traveltime]{Travel time from research facilities}",
                iso
              )
            },
            "\\end{itemize}",
            "\\clearpage"
          )

          c(
            sprintf(
              "\\section{%s}\\label{%s}",
              name,
              iso
            ),
            if (length(nodata)) image(nodata) else index,
            unlist(
              lapply(
                spp,
                function(sp){
                  s <- sp_tab[sp_tab$species == sp, ]
                  s <- s[order(match(s$layer, layer_order)), ]
                  c(
                    sprintf(
                      "\\subsection{%s}\\label{%s-%s}",
                      species_heading(sp),
                      iso,
                      sp
                    ),
                    unlist(
                      lapply(
                        seq_len(nrow(s)),
                        function(i){
                          c(
                            sprintf(
                              "\\subsubsection{%s}",
                              layer_title[[s$layer[i]]]
                            ),
                            image(s$path[i])
                          )
                        }
                      )
                    )
                  )
                }
              )
            ),
            if (length(traveltime)) {
              c(
                sprintf(
                  "\\subsection{Travel time from research facilities}\\label{%s-traveltime}",
                  iso
                ),
                image(traveltime)
              )
            }
          )

        }
      )
    )

  }

  doc <- c(
    "\\documentclass[a4paper,11pt]{article}",
    "\\usepackage[margin=1.5cm,footskip=0.8cm]{geometry}",
    "\\usepackage{fontspec}",
    "\\usepackage{graphicx}",
    "\\usepackage{xcolor}",
    "\\usepackage{fancyhdr}",
    "\\usepackage[colorlinks=true,linkcolor=blue!50!black,bookmarksdepth=3]{hyperref}",
    "\\usepackage{bookmark}",
    "\\setcounter{secnumdepth}{0}",
    sprintf(
      "\\setcounter{tocdepth}{%d}",
      if (type == "species") 2 else 1
    ),
    "\\pagestyle{fancy}",
    "\\fancyhf{}",
    "\\renewcommand{\\headrulewidth}{0pt}",
    "\\fancyfoot[L]{\\hyperlink{contents}{Contents}}",
    "\\fancyfoot[R]{\\thepage}",
    sprintf("\\title{%s}", tex_escape(title)),
    "\\author{}",
    sprintf(
      "\\date{%s}",
      if (is.null(subtitle)) "" else tex_escape(subtitle)
    ),
    "\\begin{document}",
    "\\maketitle",
    "\\hypertarget{contents}{}",
    "\\tableofcontents",
    "\\clearpage",
    body,
    "\\end{document}"
  )

  build <- tempfile("atlas_build_")

  dir.create(build)

  tex <- file.path(
    build,
    "atlas.tex"
  )

  con <- file(
    tex,
    open = "w",
    encoding = "UTF-8"
  )

  writeLines(
    doc,
    con
  )

  close(con)

  pdf <- tinytex::xelatex(tex)

  dir.create(
    dirname(file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  file.copy(
    pdf,
    file,
    overwrite = TRUE
  )

  unlink(
    build,
    recursive = TRUE
  )

  file

}

check_layers <- function(layers, layer_order){

  unknown <- setdiff(layers, layer_order)

  if (length(unknown)) {
    stop(
      sprintf(
        "make_atlas_pdf(): unrecognised page layer(s): %s",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }

}

# escape the characters LaTeX treats specially in plain text
tex_escape <- function(x){
  gsub(
    "([&%$#_{}])",
    "\\\\\\1",
    x
  )
}
