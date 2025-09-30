test_that("iscamnormprob returns formatted probabilities", {
  res_above <- capture_plot_result(suppressWarnings(iscamnormprob(
    1.96,
    direction = "above"
  )))
  res_between <- capture_plot_result(suppressWarnings(iscamnormprob(
    -1,
    xval2 = 1,
    direction = "between"
  )))

  expect_equal(as.numeric(res_above$value), round(pnorm(1.96, lower.tail = FALSE), 3), tolerance = 1e-6)
  expect_equal(as.numeric(res_between$value), round(pnorm(1) - pnorm(-1), 4), tolerance = 1e-6)

  above_lines <- trimws(res_above$output)
  expect_true(any(grepl("probability: 0.025", above_lines, fixed = TRUE)))

  between_lines <- trimws(res_between$output)
  expect_true(any(grepl("probability: 0.6827", between_lines, fixed = TRUE)))
})

test_that("iscamnormpower reports null and alternative rejection rates", {
  los <- 0.05
  n <- 80
  prob1 <- 0.5
  prob2 <- 0.55

  res <- capture_plot_result(suppressWarnings(iscamnormpower(
    LOS = los,
    n = n,
    prob1 = prob1,
    alternative = "greater",
    prob2 = prob2
  )))

  mean1 <- prob1
  sd1 <- sqrt(prob1 * (1 - prob1) / n)
  rr <- qnorm(los, mean1, sd1, lower.tail = FALSE)
  null_prob <- 1 - pnorm(rr, mean1, sd1)

  mean2 <- prob2
  sd2 <- sqrt(prob2 * (1 - prob2) / n)
  alt_prob <- 1 - pnorm(rr, mean2, sd2)

  expect_equal(rr, 0.5919501, tolerance = 1e-6)
  expect_equal(null_prob, 0.05, tolerance = 1e-7)
  expect_equal(alt_prob, 0.2253625, tolerance = 1e-7)
  expect_null(res$value)

  output_lines <- trimws(res$output)
  expect_true(any(grepl("Null: Probability 0.592 and above = 0.05", output_lines, fixed = TRUE)))
  expect_true(any(grepl("Alt: Probability 0.592 and above = 0.2253625", output_lines, fixed = TRUE)))
})

test_that("iscaminvnorm reports requested quantiles", {
  res_below <- capture_plot_result(suppressWarnings(iscaminvnorm(
    0.05,
    direction = "below"
  )))
  res_outside <- capture_plot_result(suppressWarnings(iscaminvnorm(
    0.1,
    direction = "outside"
  )))

  expect_null(res_below$value)
  expect_null(res_outside$value)

  below_lines <- trimws(res_below$output)
  expect_true(any(grepl("The observation with 0.05 probability below is -1.645", below_lines, fixed = TRUE)))

  outside_lines <- trimws(res_outside$output)
  expect_true(any(grepl("There is 0.1 probability outside -1.645 and 1.645", outside_lines, fixed = TRUE)))
})
