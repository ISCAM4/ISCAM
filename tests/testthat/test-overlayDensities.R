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
  x <- generate_data(dist = "exp", rate = 2)
  vdiffr::expect_doppelganger("iscamaddexp plot", function() iscamaddexp(x))
})

test_that("iscamaddlnorm creates a log-normal plot", {
  x <- generate_data(dist = "lnorm")
  vdiffr::expect_doppelganger("iscamaddlnorm plot", function() iscamaddlnorm(x))
})

test_that("iscamaddnorm creates a normal plot", {
  x <- generate_data(dist = "norm")
  vdiffr::expect_doppelganger("iscamaddnorm plot", function() iscamaddnorm(x))
})

test_that("iscamaddt creates a t-distribution plot", {
  x <- generate_data(dist = "t", df = 15)
  vdiffr::expect_doppelganger("iscamaddt plot", function() {
    iscamaddt(x, df = 10)
  })
})

test_that("iscamaddtnorm creates a t and normal plot", {
  x <- generate_data(dist = "t", df = 5)
  vdiffr::expect_doppelganger("iscamaddtnorm plot", function() {
    iscamaddtnorm(x, df = 5)
  })
})

test_that("Plotting with custom parameters works", {
  x <- generate_data(dist = "norm")
  vdiffr::expect_doppelganger(
    "Custom parameters plot",
    function() {
      iscamaddnorm(x, main = "Custom Title", xlab = "My Data", bins = 20)
    }
  )
})
