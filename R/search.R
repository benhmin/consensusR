# Exported search functions. consensus_search() fetches a single page,
# matching the raw API contract closely so results are predictable.
# consensus_search_all() is a convenience wrapper that follows pagination
# automatically, which is what most interactive/analysis use cases want.

#' Search the Consensus Academic Literature Database
#'
#' Queries the Consensus API (`GET /v1/search`) for peer-reviewed papers
#' relevant to `query`, with optional filters for publication date, study
#' design, methodological rigor, and journal quality. See
#' <https://docs.consensus.app/reference/v1_search> for the authoritative,
#' up-to-date parameter reference.
#'
#' This function returns a single page of results (20 by default, up to
#' 1000 via `page_size`). Use [consensus_search_all()] if you want every
#' page combined into one tibble.
#'
#' @param query Character. The natural-language research question to
#'   search for, e.g. `"Does creatine improve cognition?"`. Required.
#' @param year_min,year_max Integer. Restrict results to papers published
#'   on or after/before these years.
#' @param month_min,month_max Integer. Restrict results to papers
#'   published on or after/before these months (1-12), combined with the
#'   year filters.
#' @param study_types Character vector. One or more study designs to
#'   include, e.g. `c("rct", "meta-analysis")`. Must be values from
#'   [consensus_study_types()] - an invalid value is rejected locally,
#'   with the full list of valid values in the error message, before any
#'   request is sent.
#' @param human Logical. If `TRUE`, restrict to studies in human subjects
#'   (as opposed to animal/in-vitro studies).
#' @param controlled Logical. If `TRUE`, restrict to controlled studies.
#' @param sample_size_min Integer. Minimum study sample size.
#' @param exclude_preprints Logical. If `TRUE`, exclude preprints that
#'   have not undergone peer review.
#' @param sjr_min,sjr_max Integer. Restrict by SCImago Journal Rank (SJR)
#'   quartile, where `1` is the top quartile.
#' @param citation_min Integer. Minimum citation count.
#' @param medical_mode Logical. If `TRUE`, restrict the search to a
#'   curated set of top medical journals and clinical guidelines (~8
#'   million documents), which is useful for clinical questions where
#'   journal quality matters more than corpus breadth.
#' @param clinical_guideline Logical. If `TRUE`, restrict to clinical
#'   guideline documents specifically.
#' @param domain Character. Restrict to a subject domain. Must be a value
#'   from [consensus_domains()] (e.g. `"med"`, `"bio"`, `"cs"`,
#'   `"psych"`, `"econ"`) - invalid values are rejected locally with the
#'   full list of valid values in the error message.
#' @param country Character. Restrict by country of study.
#' @param journal_name,publisher_name Character. Restrict to a specific
#'   journal or publisher.
#' @param open_access Logical. If `TRUE`, restrict to open-access papers.
#' @param page Integer. Zero-indexed page number to fetch. Defaults to 0
#'   (the first page).
#' @param page_size Integer. Number of results per page, up to 1000.
#'   Defaults to 20.
#' @param include_semantic_score Logical. If `TRUE`, include a relevance
#'   score for each result.
#' @param include_full_text_chunks Logical. If `TRUE`, include
#'   query-relevant excerpts from the licensed full text of each paper
#'   (available on paid Consensus plans only).
#'
#' @return A [tibble::tibble()], one row per paper. The exact set of
#'   columns can vary slightly by paper (a field the API has nothing to
#'   report for a given paper is simply absent from that paper's JSON
#'   object, and becomes `NA` after row-binding), but as of 2026-11-04
#'   the API returns:
#'
#'   Always present for a normal result:
#'   * `title`, `abstract`, `doi`, `url` - character. `url` is a link to
#'     the paper's Consensus page, not the publisher's page.
#'   * `journal_name`, `publisher_name`, `volume`, `pages` - character.
#'   * `publish_year` - integer; `publish_date` - character (`"YYYY-MM-DD"`).
#'   * `citation_count`, `influential_citation_count` - integer.
#'   * `study_type` - character; one of [consensus_study_types()].
#'   * `is_preprint` - logical.
#'   * `takeaway` - character; a short, plain-language AI-generated
#'     summary of the paper's finding. Convenient for display, but it is
#'     a generated summary, not a quote - verify against `abstract` (or
#'     `full_text_chunks`) before treating it as a citable claim.
#'
#'   Present when the API has the data (`NA` otherwise - do not assume
#'   these are populated for every row):
#'   * `sjr_best_quartile` - integer, 1 = top journal quartile.
#'   * `sample_size`, `study_count`, `study_duration_days` - integer.
#'   * `population_type` - character (e.g. `"human"`, `"animal"`).
#'
#'   List-columns (variable-length per paper - see Details):
#'   * `authors` - character vector of author names per paper.
#'   * `authors_str` - character scalar, `authors` collapsed with
#'     `"; "`. Added for callers (reports, LLM agents) that need a flat
#'     string rather than a list-column.
#'   * `countries_of_study`, `institutions` - character vectors.
#'   * `full_text_chunks` - only present when
#'     `include_full_text_chunks = TRUE` **and** the paper's full text is
#'     licensed for your plan; a list of chunk objects (each with its own
#'     text and section label), or `NULL` for a paper where full text
#'     isn't available even though the flag was set.
#'
#'   Opt-in:
#'   * `semantic_score` - numeric; only present when
#'     `include_semantic_score = TRUE`.
#'
#'   `authors`, `countries_of_study`, `institutions`, and
#'   `full_text_chunks` are list-columns because each paper has a
#'   variable number of entries; unnest one with
#'   `tidyr::unnest(result, authors)` if you need one row per
#'   author/country/institution/chunk instead.
#'
#'   Pagination metadata from the API response (`page`, `page_size`,
#'   `is_end`, `next_page`) is attached as attributes on the returned
#'   tibble (e.g. `attr(result, "is_end")`), so callers can check whether
#'   more pages are available without the metadata showing up as columns
#'   on every row.
#' @export
#'
#' @examples
#' \dontrun{
#' consensus_search(
#'   "Does creatine improve cognition?",
#'   year_min = 2015,
#'   study_types = c("rct", "meta-analysis")
#' )
#' }
consensus_search <- function(query,
                              year_min = NULL,
                              year_max = NULL,
                              month_min = NULL,
                              month_max = NULL,
                              study_types = NULL,
                              human = NULL,
                              controlled = NULL,
                              sample_size_min = NULL,
                              exclude_preprints = NULL,
                              sjr_min = NULL,
                              sjr_max = NULL,
                              citation_min = NULL,
                              medical_mode = NULL,
                              clinical_guideline = NULL,
                              domain = NULL,
                              country = NULL,
                              journal_name = NULL,
                              publisher_name = NULL,
                              open_access = NULL,
                              page = 0,
                              page_size = 20,
                              include_semantic_score = NULL,
                              include_full_text_chunks = NULL) {
  if (missing(query) || !is.character(query) || length(query) != 1 || identical(query, "")) {
    cli::cli_abort("{.arg query} must be a single, non-empty character string.")
  }

  # Validate against the API's known enums locally before sending the
  # request. This matters most for callers that build these arguments
  # programmatically (e.g. an LLM agent choosing a study_types value from
  # a prompt) - they get an immediate, complete list of valid values
  # instead of an API round-trip ending in an opaque 422 error.
  consensus_validate_enum(study_types, consensus_study_types(), "study_types")
  consensus_validate_enum(domain, consensus_domains(), "domain")

  body <- consensus_request(
    "/v1/search",
    params = list(
      query = query,
      year_min = year_min,
      year_max = year_max,
      month_min = month_min,
      month_max = month_max,
      study_types = study_types,
      human = human,
      controlled = controlled,
      sample_size_min = sample_size_min,
      exclude_preprints = exclude_preprints,
      sjr_min = sjr_min,
      sjr_max = sjr_max,
      citation_min = citation_min,
      medical_mode = medical_mode,
      clinical_guideline = clinical_guideline,
      domain = domain,
      country = country,
      journal_name = journal_name,
      publisher_name = publisher_name,
      open_access = open_access,
      page = page,
      page_size = page_size,
      include_semantic_score = include_semantic_score,
      include_full_text_chunks = include_full_text_chunks
    )
  )

  results_tbl <- consensus_results_to_tibble(body$results)

  # Add a plain scalar-character version of authors alongside the
  # authors list-column. list-columns are awkward for two of this
  # package's main downstream consumers: knitr/gt tables, which can't
  # print a list-column directly, and LLM agents, which generally reason
  # more reliably over a flat string ("Smith, J.; Lee, K.") than a
  # nested list structure. The list-column is kept too, for callers who
  # want one row per author via tidyr::unnest().
  if ("authors" %in% names(results_tbl)) {
    results_tbl$authors_str <- vapply(
      results_tbl$authors,
      function(a) paste(a, collapse = "; "),
      character(1)
    )
  }

  # Pagination metadata is attached as attributes rather than extra
  # columns, so the return value stays one-row-per-paper and plays nicely
  # with dplyr pipelines, while still exposing is_end/next_page for
  # callers that want to paginate manually.
  attr(results_tbl, "page") <- body$page
  attr(results_tbl, "page_size") <- body$page_size
  attr(results_tbl, "is_end") <- body$is_end
  attr(results_tbl, "next_page") <- body$next_page

  results_tbl
}

#' Search the Consensus API and Automatically Follow Pagination
#'
#' A convenience wrapper around [consensus_search()] that repeatedly
#' requests subsequent pages (respecting the Consensus API's documented
#' rate limit by waiting one second between requests) until either the
#' API reports `is_end = TRUE` or `max_pages` is reached, then combines
#' every page into a single tibble.
#'
#' Use [consensus_search()] directly instead if you only want one page,
#' or want to control pagination/rate-limiting yourself.
#'
#' @param query Character. The natural-language research question to
#'   search for. Required.
#' @param max_pages Integer. The maximum number of pages to fetch.
#'   Defaults to `10`, which at the default `page_size` of 20 retrieves
#'   up to 200 papers; raise this (or `page_size`) for a more exhaustive
#'   search, keeping in mind each page is a separate billed API call.
#' @param ... Additional filter arguments passed through to
#'   [consensus_search()] (e.g. `year_min`, `study_types`, `page_size`).
#'
#' @return A single [tibble::tibble()] combining every fetched page, in
#'   the same column format as [consensus_search()].
#' @export
#'
#' @examples
#' \dontrun{
#' consensus_search_all(
#'   "Does creatine improve cognition?",
#'   study_types = c("rct", "meta-analysis"),
#'   max_pages = 3
#' )
#' }
consensus_search_all <- function(query, ..., max_pages = 10) {
  pages <- list()
  page_number <- 0

  repeat {
    page_result <- consensus_search(query, ..., page = page_number)
    pages[[length(pages) + 1]] <- page_result

    is_end <- isTRUE(attr(page_result, "is_end"))
    page_number <- page_number + 1

    if (is_end || page_number >= max_pages) {
      break
    }

    # The Consensus API documents a rate limit of roughly one request per
    # second on standard plans; pausing here avoids 429 responses when
    # pulling many pages in a row.
    Sys.sleep(1)
  }

  dplyr::bind_rows(pages)
}

#' Convert a List of Consensus Paper Results into a Tibble
#'
#' Internal helper that turns the `results` element of a parsed Consensus
#' API response (a list of per-paper lists, as returned by
#' `jsonlite`/`httr2` with `simplifyVector = FALSE`) into a tidy tibble.
#' Fields that are naturally variable-length per paper (e.g. `authors`,
#' `countries_of_study`, `full_text_chunks`) are kept as list-columns
#' rather than being dropped or forced into a fixed number of columns.
#'
#' @param results A list of paper result lists, or `NULL`/empty if the
#'   search returned no matches.
#' @return A [tibble::tibble()], one row per paper.
#' @keywords internal
consensus_results_to_tibble <- function(results) {
  if (length(results) == 0) {
    return(tibble::tibble())
  }

  rows <- purrr::map(results, function(paper) {
    fields <- purrr::map(paper, function(field) {
      if (is.null(field)) {
        return(NA)
      }

      if (is.list(field)) {
        # A JSON array (e.g. authors) arrives as a list of length-1
        # scalars; collapse those into a plain atomic vector so the
        # list-column holds simple character/numeric vectors instead of
        # nested lists-of-lists. Arrays of objects (e.g.
        # full_text_chunks, which are themselves structured records) are
        # left as-is.
        is_simple_array <- all(purrr::map_lgl(field, function(el) {
          length(el) == 1 && !is.list(el)
        }))
        if (is_simple_array) {
          field <- unlist(field)
        }
        return(list(field))
      }

      field
    })

    tibble::as_tibble(fields)
  })

  dplyr::bind_rows(rows)
}
