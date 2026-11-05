# The Consensus API validates `study_types` and `domain` against fixed
# enums, but does not publish the full list of valid values anywhere in
# its public documentation. These lists were captured directly from the
# API's own 422 validation error messages (observed 2026-11-04) when an
# invalid value was deliberately sent. If Consensus adds new categories,
# calls using an outdated value here will still work - validation against
# these lists happens locally only to give a fast, clear error instead of
# a round-trip 422; the API's own validation remains authoritative.

#' Valid `study_types` Values for [consensus_search()]
#'
#' Returns the fixed set of study-design values the Consensus API accepts
#' for the `study_types` filter. Exists so callers - including LLM agents
#' composing a search without having memorized the API's enum - can look
#' these up programmatically instead of guessing and hitting a 422 error.
#'
#' @return A character vector of valid `study_types` values.
#' @export
#' @examples
#' consensus_study_types()
consensus_study_types <- function() {
  c(
    "bench experiment", "case report", "case study", "case-control study",
    "cohort study", "commentary or perspective", "cross-sectional study",
    "field study", "historical or archival analysis", "interview study",
    "literature review", "longitudinal / panel data study", "meta-analysis",
    "mixed methods study", "non-randomized experimental study",
    "non-rct in vitro", "other", "rct", "systematic review",
    "theoretical, modeling, or simulation study", "non-rct experimental",
    "non-rct observational study", "non-rct non-biomedplus", "non-rct other",
    "threshold not met", "animal"
  )
}

#' Valid `domain` Values for [consensus_search()]
#'
#' Returns the fixed set of subject-domain values the Consensus API
#' accepts for the `domain` filter.
#'
#' @return A character vector of valid `domain` values.
#' @export
#' @examples
#' consensus_domains()
consensus_domains <- function() {
  c(
    "med", "bio", "cs", "chem", "psych", "phys", "mat", "eng", "env", "bus",
    "econ", "math", "poli", "agri", "edu", "soc", "geol", "geog", "hist",
    "art", "philo", "law", "ling"
  )
}

#' Validate a Filter Value Against a Known Enum
#'
#' Internal helper. Checks that every element of `value` appears in
#' `valid`, and aborts with a clear, complete list of allowed values if
#' not. This exists so an invalid `study_types` or `domain` value fails
#' immediately with an actionable message, rather than round-tripping to
#' the API to get the same information back inside a 422 response body.
#'
#' @param value Character vector supplied by the caller, or `NULL`.
#' @param valid Character vector of accepted values.
#' @param arg_name The argument name to use in the error message.
#' @return Invisibly, `NULL` if valid; otherwise aborts.
#' @keywords internal
consensus_validate_enum <- function(value, valid, arg_name) {
  if (is.null(value)) {
    return(invisible(NULL))
  }

  bad <- setdiff(value, valid)
  if (length(bad) > 0) {
    # Built as a plain comma-separated string rather than via cli's
    # `{.val {valid}}` vector formatting, which truncates long vectors
    # (e.g. the 25-value study_types enum) with "..." by default. The
    # whole point of this message is to hand the caller - frequently an
    # LLM agent parsing the error text, not a human reading a console -
    # the complete list of valid values, so truncation here would be
    # self-defeating. Same reasoning rules out `{?s}` auto-pluralization
    # above: with both `arg_name` and `bad` interpolated into one
    # bullet, cli can't tell which one's length should drive it anyway.
    valid_list <- paste(sprintf("\"%s\"", valid), collapse = ", ")
    cli::cli_abort(c(
      "Invalid value for {.arg {arg_name}}: {.val {bad}}.",
      "i" = "Valid values are: {valid_list}."
    ))
  }

  invisible(NULL)
}
