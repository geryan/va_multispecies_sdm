#' Enumerate the Mu et al. (2022) annual Human Footprint files on figshare
#'
#' The figshare item is revised: the paper and the item's own description both
#' say 2000-2018, but the deposit has been extended and at version 8 holds
#' 2000-2024. The year range is therefore *never* hardcoded here -- it is read
#' off the API. Anything that needs the years should take them from this
#' manifest.
#'
#' ## Why the version is pinned
#'
#' `version` defaults to a specific figshare version rather than "whatever is
#' current". This target's value feeds the per-year download targets, so if it
#' silently changed when the depositor added a year, every branch would
#' invalidate and {targets} would re-download the whole series. Pinning makes
#' the manifest a deterministic function of its arguments. To take up new
#' years, raise `version` deliberately -- and expect the rebuild.
#'
#' ## Pagination
#'
#' `GET /articles/{id}/files` is paginated and defaults to 10 per page, which
#' silently returns only 2000-2009 and looks like a complete answer. The
#' version endpoint returns the whole file list in one payload, which is the
#' other reason to use it.
#'
#' @param article figshare article id. 16571064 is the Mu et al. HFP deposit
#' @param version figshare version to read. `NULL` reads whatever is current,
#'   which is reproducible only until the depositor next touches the item
#' @return a data frame, one row per annual zip (`Validation.xlsx` and any
#'   other non-`hfpXXXX.zip` file is dropped), ordered by year, with columns
#'   `year`, `name`, `member` (the GeoTIFF inside the zip), `size_bytes`,
#'   `supplied_md5`, `download_url`, `article` and `version`.
#'
#'   Deliberately carries no retrieval timestamp: this data frame is a
#'   {targets} value that every download branch depends on, so anything in it
#'   that changed with the clock would invalidate and re-download the whole
#'   11 GiB series daily. The retrieval date is recorded in the written
#'   manifest instead
#' @author geryan
#' @export
mu_hfp_figshare_manifest <- function(
    article = 16571064L,
    version = 8L
  ) {

  url <- if (is.null(version)) {
    sprintf("https://api.figshare.com/v2/articles/%d", article)
  } else {
    sprintf("https://api.figshare.com/v2/articles/%d/versions/%d", article, version)
  }

  payload <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  files <- payload$files

  if (!length(files)) {
    stop(
      sprintf("mu_hfp_figshare_manifest(): no files listed at %s", url),
      call. = FALSE
    )
  }

  out <- do.call(
    rbind,
    lapply(
      files,
      function(f) {
        data.frame(
          name = f$name,
          size_bytes = as.numeric(f$size),
          supplied_md5 = if (is.null(f$supplied_md5)) NA_character_ else f$supplied_md5,
          download_url = f$download_url,
          is_link_only = isTRUE(f$is_link_only),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  # the annual rasters only: the deposit also carries Validation.xlsx
  out$year <- suppressWarnings(
    as.integer(sub("^hfp([0-9]{4})\\.zip$", "\\1", out$name))
  )

  out <- out[!is.na(out$year), ]

  if (!nrow(out)) {
    stop(
      "mu_hfp_figshare_manifest(): no files matching hfpXXXX.zip",
      call. = FALSE
    )
  }

  # a file with no checksum cannot be verified after download, and the whole
  # point of the manifest is that it can be
  if (anyNA(out$supplied_md5)) {
    stop(
      sprintf(
        "mu_hfp_figshare_manifest(): no supplied_md5 for %s",
        paste(out$name[is.na(out$supplied_md5)], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (any(out$is_link_only)) {
    stop(
      sprintf(
        "mu_hfp_figshare_manifest(): link-only (not downloadable) file: %s",
        paste(out$name[out$is_link_only], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(out$year)) {
    stop(
      sprintf(
        "mu_hfp_figshare_manifest(): more than one file for year %s",
        paste(unique(out$year[duplicated(out$year)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- out[order(out$year), ]

  # a gap would break the time series silently, so say so here rather than
  # letting a record match to the wrong year later
  gaps <- setdiff(seq(min(out$year), max(out$year)), out$year)

  if (length(gaps)) {
    warning(
      sprintf(
        "mu_hfp_figshare_manifest(): missing year(s) %s",
        paste(gaps, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data.frame(
    year = out$year,
    name = out$name,
    member = sub("\\.zip$", ".tif", out$name),
    size_bytes = out$size_bytes,
    supplied_md5 = out$supplied_md5,
    download_url = out$download_url,
    article = as.integer(article),
    version = if (is.null(version)) as.integer(payload$version) else as.integer(version),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

}
