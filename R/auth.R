# This file handles retrieval of the Consensus API key from the user's
# environment. The key itself is never written into this package or any
# tracked file - it must be supplied by the user via an environment
# variable (CONSENSUS_API_KEY), typically set in ~/.Renviron so it is
# available across all R projects/sessions without being stored in any
# project-specific, git-tracked file.

#' Retrieve the Consensus API Key from the Environment
#'
#' Internal helper used by every function in this package that calls the
#' Consensus API. It reads the `CONSENSUS_API_KEY` environment variable
#' (set via `~/.Renviron`, see [consensus_setup()]) rather than accepting
#' the key as a function argument, so the key is never typed into scripts,
#' saved in `.Rhistory`, or accidentally committed to a repository.
#'
#' @return A length-1 character string containing the API key.
#' @keywords internal
consensus_api_key <- function() {
  key <- Sys.getenv("CONSENSUS_API_KEY", unset = "")

  if (identical(key, "")) {
    cli::cli_abort(c(
      "No Consensus API key found.",
      "i" = "Set the {.envvar CONSENSUS_API_KEY} environment variable, e.g. in {.file ~/.Renviron}.",
      "i" = "Run {.run consensusR::consensus_setup()} for step-by-step instructions, then restart R."
    ))
  }

  key
}

#' Show Instructions for Configuring the Consensus API Key
#'
#' Prints the steps needed to store a Consensus API key so that
#' `consensusR` (and any other R project on this machine) can find it.
#' The key is stored once, in the user-level `~/.Renviron` file, rather
#' than per-project, since the same key is meant to be reused across
#' multiple R projects as described in the package README.
#'
#' This function only prints instructions and optionally opens the file
#' for editing - it never reads, writes, or transmits the key itself.
#'
#' @param open_file If `TRUE` (the default), opens `~/.Renviron` in the
#'   RStudio/Positron editor (via [usethis::edit_r_environ()]) so the key
#'   can be pasted in directly.
#' @return Invisibly, `NULL`. Called for its side effects (printed
#'   instructions and, optionally, opening a file for editing).
#' @export
consensus_setup <- function(open_file = TRUE) {
  cli::cli_h1("Consensus API key setup")
  cli::cli_ol(c(
    "Create or copy your API key from your Consensus account dashboard.",
    "Add the following line to {.file ~/.Renviron} (your user-level R environment file, not a project file):",
    "Save the file and restart R (environment variables are only read at session startup)."
  ))
  cli::cli_code("CONSENSUS_API_KEY=your-key-here")
  cli::cli_alert_info("Storing the key in {.file ~/.Renviron} keeps it out of git-tracked project files and makes it available to every R project on this machine.")

  if (isTRUE(open_file) && requireNamespace("usethis", quietly = TRUE)) {
    usethis::edit_r_environ("user")
  }

  invisible(NULL)
}
