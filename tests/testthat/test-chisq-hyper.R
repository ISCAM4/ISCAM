test_that("iscamchisqprob returns formatted upper-tail probability", {
  res <- capture_plot_result(iscamchisqprob(5, 3))

  expect_equal(
    as.numeric(res$value),
    pchisq(5, 3, lower.tail = FALSE),
    tolerance = 1e-4
  )

  expect_snapshot(res$output)
})

test_that("iscamhyperprob matches hypergeometric tails", {
  res_lower <- capture_plot_result(suppressWarnings(iscamhyperprob(
    k = 2,
    total = 20,
    succ = 5,
    n = 8,
    lower.tail = TRUE
  )))
  res_upper <- capture_plot_result(suppressWarnings(iscamhyperprob(
    k = 3,
    total = 20,
    succ = 5,
    n = 8,
    lower.tail = FALSE
  )))

  fail <- 20 - 5

  expect_equal(res_lower$value, phyper(2, 5, fail, 8), tolerance = 1e-6)
  expect_equal(res_upper$value, 1 - phyper(2, 5, fail, 8), tolerance = 1e-6)

  expect_snapshot(res_lower$output)
  expect_snapshot(res_upper$output)
})

test_that("iscamhypernorm reports tail probabilities and normal approximations", {
  res_lower <- capture_plot_result(suppressWarnings(iscamhypernorm(
    k = 2,
    total = 20,
    succ = 5,
    n = 8,
    lower.tail = TRUE
  )))
  res_upper <- capture_plot_result(suppressWarnings(iscamhypernorm(
    k = 3,
    total = 20,
    succ = 5,
    n = 8,
    lower.tail = FALSE
  )))

  expect_null(res_lower$value)
  expect_null(res_upper$value)

  expect_snapshot(res_lower$output)
  expect_snapshot(res_upper$output)
})
