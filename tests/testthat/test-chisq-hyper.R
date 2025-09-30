test_that("iscamchisqprob returns formatted upper-tail probability", {
  res <- capture_plot_result(iscamchisqprob(5, 3))

  expect_equal(as.numeric(res$value), pchisq(5, 3, lower.tail = FALSE), tolerance = 1e-4)

  output_lines <- trimws(res$output)
  expect_true(any(grepl("probability: 0.1718", output_lines, fixed = TRUE)))
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

  lower_lines <- trimws(res_lower$output)
  expect_true(any(grepl("Probability 2 and below =", lower_lines, fixed = TRUE)))
  expect_true(any(grepl(sprintf("= %.7f", res_lower$value), lower_lines, fixed = TRUE)))

  upper_lines <- trimws(res_upper$output)
  expect_true(any(grepl("Probability 3 and above =", upper_lines, fixed = TRUE)))
  expect_true(any(grepl(sprintf("= %.7f", res_upper$value), upper_lines, fixed = TRUE)))
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

  lower_lines <- trimws(res_lower$output)
  expect_true(any(grepl("hypergeometric: 0.7038", lower_lines, fixed = TRUE)))
  expect_true(any(grepl("normal approx: 0.5", lower_lines, fixed = TRUE)))
  expect_true(any(grepl("normal approx with continuity: 0.6963", lower_lines, fixed = TRUE)))

  upper_lines <- trimws(res_upper$output)
  expect_true(any(grepl("hypergeometric: 0.2962", upper_lines, fixed = TRUE)))
  expect_true(any(grepl("normal approx: 0.1521", upper_lines, fixed = TRUE)))
  expect_true(any(grepl("normal approx with continuity: 0.3037", upper_lines, fixed = TRUE)))
})
