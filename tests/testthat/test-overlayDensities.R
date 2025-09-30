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
})

test_that("iscamaddlnorm creates a log-normal plot", {
  vdiffr::expect_doppelganger("log-normal", function() {
    iscamaddlnorm(generate_data(dist = "lnorm"))
  })
})

test_that("iscamaddnorm creates a normal plot", {
  vdiffr::expect_doppelganger("Normal plot", function() {
    iscamaddnorm(generate_data(dist = "norm"))
  })
})

test_that("iscamaddt creates a t-distribution plot", {
  vdiffr::expect_doppelganger("t-distribution", function() {
    iscamaddt(
      generate_data(dist = "t", df = 15),
      df = 15
    )
  })
})

test_that("iscamaddtnorm creates a t and normal plot", {
  vdiffr::expect_doppelganger("t and normal", function() {
    iscamaddtnorm(
      generate_data(dist = "t", df = 1),
      df = 1
    )
  })
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
})
