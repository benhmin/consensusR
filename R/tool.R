# This file exists because the package is expected to be used heavily by
# LLM agents (via ellmer tool-calling, or any other agent framework that
# can wrap an R function), not just called directly by a human analyst.
# An agent choosing arguments from a prompt needs the same enum
# constraints that consensus_search() enforces, but expressed as a
# structured schema it can see before calling, rather than discovered via
# a validation error after calling. consensus_search_tool() builds that
# schema once, so every agent integration uses the same contract instead
# of each one re-describing the API from scratch.

#' Wrap [consensus_search()] as an ellmer Tool for LLM Agents
#'
#' Returns an [ellmer::tool()] object that exposes Consensus literature
#' search to an LLM agent (via the ellmer package's tool-calling support).
#' Register it with an ellmer chat and the model can search peer-reviewed
#' literature and ground its answers in cited, published sources.
#'
#' The schema passed to the model enforces the same constraints as
#' [consensus_search()] itself (e.g. `study_types` and `domain` are
#' declared as enums using [consensus_study_types()] and
#' [consensus_domains()]), so a well-behaved model should rarely pick an
#' invalid value in the first place. Compared to calling
#' [consensus_search()] directly, the tool function:
#'
#' * Only exposes a deliberately small subset of filters (the ones most
#'   useful for a model to choose from a natural-language request) to
#'   keep the schema simple to reason about.
#' * Returns a plain list of records containing only scalar fields -
#'   `title`, `authors_str`, `journal_name`, `publish_year`, `doi`,
#'   `url`, `study_type`, `sample_size`, `citation_count`, `takeaway`,
#'   `abstract` - dropping list-columns and pagination attributes, since
#'   most chat models reason far more reliably over a flat list of
#'   records than an R tibble with list-columns, and tool results are
#'   serialized to the model as text/JSON regardless.
#' * Defaults `page_size` to 10 rather than 20, since each extra result
#'   costs tokens in the model's context; raise it if you want the agent
#'   to see more candidates per search.
#'
#' @return An [ellmer::tool()] object, ready to pass to
#'   `chat$register_tool()` (or equivalent for your ellmer chat object).
#' @export
#'
#' @examples
#' \dontrun{
#' chat <- ellmer::chat_anthropic()
#' chat$register_tool(consensus_search_tool())
#' chat$chat("Does creatine improve cognition? Cite RCTs or meta-analyses.")
#' }
consensus_search_tool <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg ellmer} package is required to build this tool.",
      "i" = "Install it with {.code install.packages(\"ellmer\")}."
    ))
  }

  search_for_agent <- function(query,
                                year_min = NULL,
                                year_max = NULL,
                                study_types = NULL,
                                human = NULL,
                                exclude_preprints = NULL,
                                domain = NULL,
                                open_access = NULL,
                                page_size = 10) {
    result <- consensus_search(
      query = query,
      year_min = year_min,
      year_max = year_max,
      study_types = study_types,
      human = human,
      exclude_preprints = exclude_preprints,
      domain = domain,
      open_access = open_access,
      page_size = page_size
    )

    # Keep only scalar, always-serializable fields. countries_of_study,
    # institutions, and full_text_chunks are dropped here (not from
    # consensus_search() itself) purely because this is the tool-calling
    # path, where the result has to serialize cleanly to the model.
    keep <- intersect(
      c(
        "title", "authors_str", "journal_name", "publish_year", "doi",
        "url", "study_type", "sample_size", "citation_count", "takeaway",
        "abstract"
      ),
      names(result)
    )

    # as.list() on a data frame subset yields one list per column; flip
    # it to one list per paper, which is the shape a model can read as a
    # straightforward array of records.
    result <- result[, keep, drop = FALSE]
    purrr::transpose(as.list(result))
  }

  ellmer::tool(
    search_for_agent,
    description = paste(
      "Search peer-reviewed academic literature using the Consensus API.",
      "Returns relevance-ranked papers with citation counts, study",
      "design, and a short plain-language takeaway for each result.",
      "Use this to find and cite actual published research instead of",
      "answering a research question from memory."
    ),
    arguments = list(
      query = ellmer::type_string(
        "The research question to search for, as a natural-language question, e.g. 'Does creatine improve cognition?'. Required."
      ),
      year_min = ellmer::type_integer(
        "Only include papers published in this year or later.",
        required = FALSE
      ),
      year_max = ellmer::type_integer(
        "Only include papers published in this year or earlier.",
        required = FALSE
      ),
      study_types = ellmer::type_array(
        ellmer::type_enum(consensus_study_types(), description = "A single study design."),
        description = "One or more study designs to restrict results to, e.g. ['rct', 'meta-analysis'].",
        required = FALSE
      ),
      human = ellmer::type_boolean(
        "If true, restrict to studies in human subjects (excludes animal/in-vitro studies).",
        required = FALSE
      ),
      exclude_preprints = ellmer::type_boolean(
        "If true, exclude preprints that have not undergone peer review.",
        required = FALSE
      ),
      domain = ellmer::type_enum(
        consensus_domains(),
        description = "Restrict to a subject domain.",
        required = FALSE
      ),
      open_access = ellmer::type_boolean(
        "If true, restrict to open-access papers.",
        required = FALSE
      ),
      page_size = ellmer::type_integer(
        "Number of papers to return, default 10. Keep this small (under ~20) to limit token usage per search.",
        required = FALSE
      )
    ),
    name = "consensus_search"
  )
}
