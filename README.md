# consensusR

A lightweight R client for the [Consensus](https://consensus.app) academic literature search API (`api.consensus.app`). No official or community R package existed for this API as of November 2026, so this package fills that gap for use across Ben Orwoll's R projects.

## Installation

From any R project, install directly from the local source:

```r
pak::pak("local::~/Documents/R/consensusR")
# or
devtools::install("~/Documents/R/consensusR")
```

## Setup: API key

This package reads your Consensus API key from the `CONSENSUS_API_KEY` environment variable. It never accepts the key as a function argument, so it can't be typed into a script or saved in `.Rhistory`/git history by accident.

1. Get your API key from your Consensus account dashboard.
2. Run `consensusR::consensus_setup()` for instructions, which also opens `~/.Renviron` for editing.
3. Add this line to `~/.Renviron` (your user-level environment file, so it's available to every R project on this machine, not just this one):

   ```
   CONSENSUS_API_KEY=your-key-here
   ```

4. Save the file and restart R.

> Note: a Consensus web-app subscription (e.g. an institutional license) does not automatically include API access. Confirm your plan includes API usage (Pro/Deep/Teams include a monthly allowance; Enterprise is custom) before relying on this package for production work.

## Usage

```r
library(consensusR)

# A single page of results (20 by default)
results <- consensus_search(
  "Does creatine improve cognition?",
  year_min = 2015,
  study_types = c("rct", "meta-analysis")
)

# Pagination metadata is attached as attributes, not extra columns
attr(results, "is_end")
attr(results, "next_page")

# Automatically follow pagination and combine every page into one tibble
all_results <- consensus_search_all(
  "Does creatine improve cognition?",
  study_types = c("rct", "meta-analysis"),
  max_pages = 5
)
```

`authors`, `countries_of_study`, and (if requested via `include_full_text_chunks = TRUE`) `full_text_chunks` are returned as list-columns, since they vary in length per paper. Unnest them with `tidyr::unnest()` if you need one row per author/chunk. A flat `authors_str` column (authors collapsed into one `"; "`-separated string) is included too, for display and for anything handed to an LLM.

See `vignette("consensusr", package = "consensusR")` for the full output schema (every column, which ones can be `NA`, and why) and for LLM agent integration - including `consensus_search_tool()`, a ready-made [ellmer](https://ellmer.tidyverse.org) tool with the API's `study_types`/`domain` enums built into its schema.

## Rate limits

The Consensus API documents a limit of roughly one request per second on standard plans. `consensus_search_all()` waits one second between pages automatically. If you're issuing many single-page `consensus_search()` calls in a loop, add your own `Sys.sleep(1)` between calls.

## What this does not cover

This package wraps the public `/v1/search` REST endpoint only. It does not implement the Consensus MCP server (a separate product for use inside AI assistants) or any full-text-retrieval endpoints beyond the `include_full_text_chunks` search parameter. See <https://docs.consensus.app> for the authoritative, up-to-date API reference if Consensus adds new endpoints or parameters.
