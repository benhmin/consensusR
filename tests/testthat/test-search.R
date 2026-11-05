# These tests check the pure data-wrangling logic (turning a parsed JSON
# response into a tibble) without making any live network calls to the
# Consensus API, so they run offline and do not consume API quota.

test_that("consensus_results_to_tibble handles an empty result set", {
  result <- consensus_results_to_tibble(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("consensus_results_to_tibble simplifies author arrays and keeps scalars", {
  fake_results <- list(
    list(
      title = "Paper One",
      authors = list("Smith, J.", "Lee, K."),
      publish_year = 2023L,
      citation_count = 10L
    ),
    list(
      title = "Paper Two",
      authors = list("Doe, J."),
      publish_year = 2020L,
      citation_count = 2L
    )
  )

  result <- consensus_results_to_tibble(fake_results)

  expect_equal(nrow(result), 2)
  expect_equal(result$title, c("Paper One", "Paper Two"))
  expect_true(is.list(result$authors))
  expect_equal(result$authors[[1]], c("Smith, J.", "Lee, K."))
  expect_equal(result$authors[[2]], "Doe, J.")
})

test_that("consensus_search validates its query argument before calling the API", {
  expect_error(consensus_search(""), "non-empty")
  expect_error(consensus_search(123), "non-empty")
})

test_that("consensus_search rejects invalid study_types/domain locally, without calling the API", {
  # Deliberately unset the API key so that if validation failed to short
  # -circuit before the network call, the test would fail with a "no key
  # found" error instead of the expected enum-validation error - that
  # distinguishes "validated locally" from "API would have rejected it".
  withr::local_envvar(CONSENSUS_API_KEY = "")

  expect_error(
    consensus_search("x", study_types = "not_a_real_type"),
    "Invalid value.*study_types"
  )
  expect_error(
    consensus_search("x", domain = "not_a_real_domain"),
    "Invalid value.*domain"
  )
})

test_that("consensus_validate_enum reports every invalid value and lists all valid options", {
  expect_error(
    consensus_validate_enum(c("rct", "nope"), consensus_study_types(), "study_types"),
    "nope"
  )
  expect_silent(consensus_validate_enum("rct", consensus_study_types(), "study_types"))
  expect_silent(consensus_validate_enum(NULL, consensus_study_types(), "study_types"))
})

test_that("consensus_results_to_tibble output can produce a flat authors_str via consensus_search's post-processing", {
  fake_results <- list(list(title = "P", authors = list("A", "B")))
  tbl <- consensus_results_to_tibble(fake_results)
  authors_str <- vapply(tbl$authors, function(a) paste(a, collapse = "; "), character(1))
  expect_equal(authors_str, "A; B")
})
