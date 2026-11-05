# Internal request plumbing shared by every exported search function.
# Centralizing this here means the base URL, authentication header, and
# error handling only need to be correct in one place.

#' Build and Perform a Request Against the Consensus API
#'
#' Internal helper that constructs a GET request to a Consensus API
#' endpoint, attaches the `x-api-key` authentication header, sends it, and
#' returns the parsed JSON body. `NULL`-valued parameters are dropped
#' before the request is sent, so callers can pass every optional filter
#' through as `NULL` by default without those filters appearing in the
#' query string (the Consensus API treats a present-but-empty parameter
#' differently from an absent one for some filters).
#'
#' @param endpoint Path appended to `https://api.consensus.app`, e.g.
#'   `"/v1/search"`.
#' @param params A named list of query parameters. Elements that are
#'   `NULL` are removed. Elements that are vectors (e.g. `study_types =
#'   c("rct", "meta-analysis")`) are sent as a repeated query parameter
#'   (`study_types=rct&study_types=meta-analysis`) rather than a
#'   comma-joined string - the API rejects a comma-joined value with a
#'   422 error, since it validates each `study_types` entry individually
#'   against a fixed enum.
#' @return The response body, parsed from JSON into a list.
#' @keywords internal
consensus_request <- function(endpoint, params = list()) {
  # Drop NULL filters so they are omitted from the query string entirely,
  # rather than being sent as empty strings.
  params <- purrr::compact(params)

  req <- httr2::request("https://api.consensus.app") |>
    httr2::req_url_path_append(endpoint) |>
    # .multi = "explode" repeats a parameter name once per vector element
    # (e.g. study_types = c("rct", "meta-analysis") becomes two separate
    # study_types= entries in the query string) instead of joining the
    # vector into one comma-separated value, which is the format this
    # API's own parameter validation expects.
    httr2::req_url_query(!!!params, .multi = "explode") |>
    httr2::req_headers(`x-api-key` = consensus_api_key()) |>
    httr2::req_user_agent("consensusR (https://github.com/orwollb/consensusR)") |>
    # Let httr2 retry automatically on 429 (rate limited) and 5xx responses,
    # honoring the Consensus API's documented ~1 request/second rate limit.
    httr2::req_retry(max_tries = 3, backoff = ~ 2^.x)

  resp <- httr2::req_perform(req)

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}
