generate_data <- function(n = 100, dist = c("norm", "exp", "lnorm", "t"), ...) {
  dist <- match.arg(dist)
  set.seed(0)
  switch(
    dist,
    norm = rnorm(n, ...),
    exp = rexp(n, ...),
    lnorm = rlnorm(n, ...),
    t = rt(n, ...)
  )
}

test_that("iscamaddexp creates an exponential plot", {
  vdiffr::expect_doppelganger("exponential", function() {
    iscamaddexp(generate_data(
      dist = "exp",
      rate = 2
    ))
  })
  res <- capture_plot_result(iscamaddexp(generate_data(
    dist = "exp",
    rate = 2
  )))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscamaddlnorm creates a log-normal plot", {
  vdiffr::expect_doppelganger("log-normal", function() {
    iscamaddlnorm(generate_data(dist = "lnorm"))
  })
  res <- capture_plot_result(iscamaddlnorm(generate_data(dist = "lnorm")))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscamaddnorm creates a normal plot", {
  vdiffr::expect_doppelganger("Normal plot", function() {
    iscamaddnorm(generate_data(dist = "norm"))
  })
  res <- capture_plot_result(iscamaddnorm(generate_data(dist = "norm")))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscamaddt creates a t-distribution plot", {
  vdiffr::expect_doppelganger("t-distribution", function() {
    iscamaddt(
      generate_data(dist = "t", df = 15),
      df = 15
    )
  })
  res <- capture_plot_result(iscamaddt(
    generate_data(dist = "t", df = 15),
    df = 15
  ))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscamaddtnorm creates a t and normal plot", {
  vdiffr::expect_doppelganger("t and normal", function() {
    iscamaddtnorm(
      generate_data(dist = "t", df = 1),
      df = 1
    )
  })
  res <- capture_plot_result(iscamaddtnorm(
    generate_data(dist = "t", df = 1),
    df = 1
  ))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("Plotting with custom parameters works", {
  vdiffr::expect_doppelganger("Custom params", function() {
    iscamaddnorm(
      generate_data(dist = "norm"),
      main = "Custom Title",
      xlab = "My Data",
      bins = 20
    )
  })
  res <- capture_plot_result(iscamaddnorm(
    generate_data(dist = "norm"),
    main = "Custom Title",
    xlab = "My Data",
    bins = 20
  ))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscamaddexp prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamaddexp("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamaddlnorm prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamaddlnorm("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamaddnorm prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamaddnorm("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamaddt prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamaddt("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamaddtnorm prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamaddtnorm("?"))
  expect_snapshot(collapse_output(help_lines))
})

