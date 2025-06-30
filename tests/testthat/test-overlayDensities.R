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

test_that("addexp creates an exponential plot", {
  x <- generate_data(dist = "exp", rate = 2)
  vdiffr::expect_doppelganger("addexp plot", function() addexp(x))
})

test_that("addlnorm creates a log-normal plot", {
  x <- generate_data(dist = "lnorm")
  vdiffr::expect_doppelganger("addlnorm plot", function() addlnorm(x))
})

test_that("addnorm creates a normal plot", {
  x <- generate_data(dist = "norm")
  vdiffr::expect_doppelganger("addnorm plot", function() addnorm(x))
})

test_that("addt creates a t-distribution plot", {
  x <- generate_data(dist = "t", df = 15)
  vdiffr::expect_doppelganger("addt plot", function() addt(x, df = 10))
})

test_that("addtnorm creates a t and normal plot", {
  x <- generate_data(dist = "t", df = 5)
  vdiffr::expect_doppelganger("addtnorm plot", function() addtnorm(x, df = 5))
})

test_that("Plotting with custom parameters works", {
  x <- generate_data(dist = "norm")
  vdiffr::expect_doppelganger(
    "Custom parameters plot",
    function() addnorm(x, main = "Custom Title", xlab = "My Data", bins = 20)
  )
})
