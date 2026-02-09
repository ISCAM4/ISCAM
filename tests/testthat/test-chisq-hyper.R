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

test_that("iscamchisqprob prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamchisqprob("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamhypernorm prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamhypernorm("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamhyperprob prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamhyperprob("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamhypernorm converts fractional k inputs", {
  total <- 30
  succ <- 12
  n <- 10
  k_prop <- 0.4

  res <- capture_plot_result(suppressWarnings(iscamhypernorm(
    k = k_prop,
    total = total,
    succ = succ,
    n = n,
    lower.tail = TRUE
  )))

  converted_k <- round((k_prop * n * (total - n) + n * succ) / total)
  expected_prob <- phyper(converted_k, succ, total - succ, n)

  expect_null(res$value)
  expect_snapshot(res$output)
  expect_gt(converted_k, 0)
  expect_gt(expected_prob, 0)
})

test_that("iscamhyperprob matches fractional inputs for both tails", {
  total <- 30
  succ <- 12
  n <- 10
  k_prop <- 0.35

  res_lower <- capture_plot_result(suppressWarnings(iscamhyperprob(
    k = k_prop,
    total = total,
    succ = succ,
    n = n,
    lower.tail = TRUE
  )))
  res_upper <- capture_plot_result(suppressWarnings(iscamhyperprob(
    k = k_prop,
    total = total,
    succ = succ,
    n = n,
    lower.tail = FALSE
  )))

  converted_k <- round((k_prop * (total - n) * n + succ * n) / total)
  lower_prob <- phyper(converted_k, succ, total - succ, n)
  upper_prob <- 1 - phyper(converted_k - 1, succ, total - succ, n)

  expect_equal(res_lower$value, lower_prob, tolerance = 1e-6)
  expect_equal(res_upper$value, upper_prob, tolerance = 1e-6)

  expect_snapshot(res_lower$output)
  expect_snapshot(res_upper$output)
})
