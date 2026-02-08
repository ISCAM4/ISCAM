write_test_rd <- function(path) {
  writeLines(
    c(
      "\\name{iscamtprob}",
      "\\alias{iscamtprob}",
      "\\title{Tail Probability for t-distribution}",
      "\\description{Dummy help file for testing fallback paths.}",
      "\\usage{iscamtprob(xval, df, direction, xval2 = NULL, verbose = TRUE)}",
      "\\arguments{",
      "  \\item{xval}{observed value.}",
      "  \\item{df}{degrees of freedom.}",
      "}",
      "\\value{A numeric tail probability.}"
    ),
    path
  )
}

test_that(".iscam_show_help uses system.file(man, package=) path when available", {
  show_help <- get(".iscam_show_help", envir = asNamespace("ISCAM"))

  tmp <- tempfile("iscam-help-pkg-")
  dir.create(tmp)
  rd_path <- file.path(tmp, "iscamtprob.Rd")
  write_test_rd(rd_path)

  testthat::local_mocked_bindings(
    system.file = function(..., package = NULL) tmp,
    .package = "base"
  )
  testthat::local_mocked_bindings(
    help = function(...) character(0),
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    Rd_db = function(...) list(),
    .package = "tools"
  )

  help_lines <- capture_help_output(show_help("iscamtprob"))
  expect_true(any(grepl("^Arguments:", help_lines)))
})

test_that(".iscam_show_help falls back to working directory man files", {
  show_help <- get(".iscam_show_help", envir = asNamespace("ISCAM"))

  tmp <- tempfile("iscam-help-")
  dir.create(tmp)
  dir.create(file.path(tmp, "man"))
  write_test_rd(file.path(tmp, "man", "iscamtprob.Rd"))

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(tmp)

  testthat::local_mocked_bindings(
    system.file = function(..., package = NULL) "",
    loadedNamespaces = function() character(0),
    .package = "base"
  )
  testthat::local_mocked_bindings(
    help = function(...) character(0),
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    Rd_db = function(...) list(),
    .package = "tools"
  )

  help_lines <- capture_help_output(show_help("iscamtprob"))
  expect_true(any(grepl("^Arguments:", help_lines)))
})

test_that(".iscam_show_help handles missing Rd_db keys", {
  show_help <- get(".iscam_show_help", envir = asNamespace("ISCAM"))

  tmp <- tempfile("iscam-help-empty-")
  dir.create(tmp)
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(tmp)

  testthat::local_mocked_bindings(
    system.file = function(..., package = NULL) "",
    loadedNamespaces = function() character(0),
    .package = "base"
  )
  testthat::local_mocked_bindings(
    help = function(...) "has-help",
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    Rd_db = function(...) list(),
    .package = "tools"
  )

  help_lines <- capture_help_output(show_help("iscamtprob"))
  expect_true(any(grepl("No documentation available", help_lines, fixed = TRUE)))
})

test_that(".iscam_show_help reports missing documentation", {
  show_help <- get(".iscam_show_help", envir = asNamespace("ISCAM"))
  help_lines <- capture_help_output(show_help("not_a_real_topic_iscam"))

  expect_true(any(grepl("No documentation available", help_lines, fixed = TRUE)))
})

test_that(".iscam_maybe_help derives topic when omitted", {
  maybe_help <- get(".iscam_maybe_help", envir = asNamespace("ISCAM"))

  call_maybe_help <- function() {
    maybe_help("?", topic = NULL)
  }

  help_lines <- capture_help_output(call_maybe_help())
  expect_true(any(grepl("No documentation available", help_lines, fixed = TRUE)))
})
