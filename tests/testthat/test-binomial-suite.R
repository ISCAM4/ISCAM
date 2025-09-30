test_that("iscambinomprob returns expected tail probabilities", {
  res_lower <- capture_plot_result(iscambinomprob(
    k = 3,
    n = 10,
    prob = 0.4,
    lower.tail = TRUE
  ))
  res_upper <- capture_plot_result(iscambinomprob(
    k = 7,
    n = 10,
    prob = 0.4,
    lower.tail = FALSE
  ))

  expect_equal(res_lower$value, pbinom(3, 10, 0.4), tolerance = 1e-6)
  expect_equal(res_upper$value, 1 - pbinom(6, 10, 0.4), tolerance = 1e-6)

  lower_lines <- trimws(res_lower$output)
  expect_true(any(grepl(
    "Probability 3 and below =",
    lower_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf("= %.7f", res_lower$value),
    lower_lines,
    fixed = TRUE
  )))

  upper_lines <- trimws(res_upper$output)
  expect_true(any(grepl(
    "Probability 7 and above =",
    upper_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf("= %.8f", res_upper$value),
    upper_lines,
    fixed = TRUE
  )))
})

test_that("iscaminvbinom solves the correct quantile", {
  res_lower <- capture_plot_result(iscaminvbinom(
    alpha = 0.1,
    n = 20,
    prob = 0.4,
    lower.tail = TRUE
  ))
  res_upper <- capture_plot_result(iscaminvbinom(
    alpha = 0.1,
    n = 20,
    prob = 0.4,
    lower.tail = FALSE
  ))

  expect_equal(res_lower$value, qbinom(0.1, 20, 0.4, lower.tail = TRUE) - 1)
  expect_equal(res_upper$value, qbinom(0.1, 20, 0.4, lower.tail = FALSE) + 1)

  lower_lines <- trimws(res_lower$output)
  expect_true(any(grepl(
    "The observation with at most 0.1 probability at or below",
    lower_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf("is %d", res_lower$value),
    lower_lines,
    fixed = TRUE
  )))

  upper_lines <- trimws(res_upper$output)
  expect_true(any(grepl(
    "The observation with at most 0.1 probability at or above",
    upper_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf("is %d", res_upper$value),
    upper_lines,
    fixed = TRUE
  )))
})

test_that("iscambinomnorm executes for each direction", {
  below_lines <- trimws(
    capture_plot_result(iscambinomnorm(10, 20, 0.5, "below"))$output
  )
  expect_true(any(grepl("binomial: 0.5881", below_lines, fixed = TRUE)))
  expect_true(any(grepl("normal approx: 0.5", below_lines, fixed = TRUE)))
  expect_true(any(grepl(
    "normal approx with continuity: 0.5885",
    below_lines,
    fixed = TRUE
  )))

  above_lines <- trimws(
    capture_plot_result(iscambinomnorm(10, 20, 0.5, "above"))$output
  )
  expect_true(any(grepl("binomial: 0.5881", above_lines, fixed = TRUE)))
  expect_true(any(grepl("normal approx: 0.5", above_lines, fixed = TRUE)))
  expect_true(any(grepl(
    "normal approx with continuity: 0.5885",
    above_lines,
    fixed = TRUE
  )))

  two_sided_lines <- trimws(
    capture_plot_result(iscambinomnorm(10, 20, 0.5, "two.sided"))$output
  )
  expect_true(any(grepl("binomial: 1.176", two_sided_lines, fixed = TRUE)))
  expect_true(any(grepl("normal approx: 1", two_sided_lines, fixed = TRUE)))
  expect_true(any(grepl(
    "normal approx with continuity: 1.177",
    two_sided_lines,
    fixed = TRUE
  )))
})

test_that("iscambinompower reports rejection probabilities", {
  los <- 0.05
  n <- 20
  prob1 <- 0.5
  prob2 <- 0.6

  res <- capture_plot_result(suppressWarnings(iscambinompower(
    LOS = los,
    n = n,
    prob1 = prob1,
    alternative = "greater",
    prob2 = prob2
  )))
  rr <- qbinom(los, n, prob1, lower.tail = FALSE) + 1
  null_prob <- 1 - pbinom(rr - 1, n, prob1)
  alt_prob <- 1 - pbinom(rr - 1, n, prob2)

  expect_equal(rr, 15)
  expect_equal(null_prob, 0.02069473, tolerance = 1e-6)
  expect_equal(alt_prob, 0.125599, tolerance = 1e-6)
  expect_null(res$value)

  output_lines <- trimws(res$output)
  expect_true(any(grepl(
    sprintf("Null: Probability %d and above = %.8f", rr, null_prob),
    output_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf("Alternative: Probability %d and above = %.6f", rr, alt_prob),
    output_lines,
    fixed = TRUE
  )))
})

test_that("iscambinomtest matches binom.test results", {
  res <- capture_plot_result(iscambinomtest(
    observed = 18,
    n = 30,
    hypothesized = 0.5,
    alternative = "two.sided",
    conf.level = 0.95
  ))

  bt <- binom.test(18, 30, p = 0.5, alternative = "two.sided")

  expect_equal(res$value$pvalue, signif(bt$p.value, 5), tolerance = 1e-6)
  expect_equal(res$value$lower, bt$conf.int[1], tolerance = 1e-4)
  expect_equal(res$value$upper, bt$conf.int[2], tolerance = 1e-4)

  output_lines <- trimws(res$output)
  expect_true("Exact Binomial Test" %in% output_lines)
  expect_true(any(grepl(
    "Data: observed successes = 18, sample size = 30, sample proportion = 0.6",
    output_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf("Null hypothesis       : pi = %.1f", 0.5),
    output_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Alternative hypothesis: pi <> 0.5",
    output_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf("p-value: %.5f", res$value$pvalue),
    output_lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    sprintf(
      "95 %% Confidence interval for pi: ( %.5f , %.5f )",
      res$value$lower,
      res$value$upper
    ),
    output_lines,
    fixed = TRUE
  )))
})
