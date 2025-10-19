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

  expect_equal(
    as.numeric(res_above$value),
    pnorm(1.96, lower.tail = FALSE),
    tolerance = 1e-3
  )
  expect_equal(
    as.numeric(res_between$value),
    pnorm(1) - pnorm(-1),
    tolerance = 5e-4
  )

  expect_snapshot(res_above$output)
  expect_snapshot(res_between$output)
})

test_that("iscamnormprob handles below and outside directions", {
  res_below <- capture_plot_result(suppressWarnings(iscamnormprob(
    xval = -0.5,
    mean = 0,
    sd = 1,
    direction = "below"
  )))
  res_outside <- capture_plot_result(suppressWarnings(iscamnormprob(
    xval = 2.3,
    xval2 = -1.2,
    mean = 1,
    sd = 2,
    direction = "outside"
  )))

  expect_equal(
    as.numeric(res_below$value),
    pnorm(-0.5, lower.tail = TRUE),
    tolerance = 5e-4
  )
  lower <- min(2.3, -1.2)
  upper <- max(2.3, -1.2)
  outside_prob <- 1 - (pnorm(upper, 1, 2) - pnorm(lower, 1, 2))
  expect_equal(
    as.numeric(res_outside$value),
    outside_prob,
    tolerance = 1e-4
  )

  expect_snapshot(res_below$output)
  expect_snapshot(res_outside$output)
})

test_that("iscamnormprob validates direction inputs", {
  expect_error(
    suppressWarnings(iscamnormprob(0, direction = "sideways")),
    "Use \"above\", \"below\", \"between\", or \"outside\" as the direction."
  )
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

  expect_equal(rr, qnorm(los, mean1, sd1, lower.tail = FALSE))
  expect_equal(null_prob, pnorm(rr, mean1, sd1, lower.tail = FALSE))
  expect_equal(alt_prob, 1 - pnorm(rr, mean2, sd2))
  expect_null(res$value)

  expect_snapshot(res$output)
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

  expect_snapshot(res_below$output)
  expect_snapshot(res_outside$output)
})

test_that("iscamnormpower handles less and two-sided alternatives", {
  los_less <- 0.1
  n_less <- 60
  prob1_less <- 0.45
  prob2_less <- 0.4
  los_two <- 0.05
  n_two <- 90
  prob1_two <- 0.55
  prob2_two <- 0.6

  res_less <- capture_plot_result(suppressWarnings(iscamnormpower(
    LOS = los_less,
    n = n_less,
    prob1 = prob1_less,
    alternative = "less",
    prob2 = prob2_less
  )))

  mean_less <- prob1_less
  sd_less <- sqrt(prob1_less * (1 - prob1_less) / n_less)
  rr_less <- qnorm(los_less, mean_less, sd_less)
  null_prob_less <- pnorm(rr_less, mean_less, sd_less)
  alt_prob_less <- pnorm(rr_less, prob2_less, sqrt(prob2_less * (1 - prob2_less) / n_less))

  res_two <- capture_plot_result(suppressWarnings(iscamnormpower(
    LOS = los_two,
    n = n_two,
    prob1 = prob1_two,
    alternative = "two.sided",
    prob2 = prob2_two
  )))

  mean_two <- prob1_two
  sd_two <- sqrt(prob1_two * (1 - prob1_two) / n_two)
  lower_rr <- qnorm(los_two / 2, mean_two, sd_two)
  upper_rr <- qnorm(los_two / 2, mean_two, sd_two, lower.tail = FALSE)
  null_two <- pnorm(lower_rr, mean_two, sd_two) +
    pnorm(upper_rr, mean_two, sd_two, lower.tail = FALSE)
  alt_sd_two <- sqrt(prob2_two * (1 - prob2_two) / n_two)
  alt_two <- pnorm(lower_rr, prob2_two, alt_sd_two) +
    pnorm(upper_rr, prob2_two, alt_sd_two, lower.tail = FALSE)

  expect_equal(rr_less, qnorm(los_less, mean_less, sd_less))
  expect_equal(null_prob_less, pnorm(rr_less, mean_less, sd_less))
  expect_equal(
    alt_prob_less,
    pnorm(rr_less, prob2_less, sqrt(prob2_less * (1 - prob2_less) / n_less))
  )
  expect_null(res_less$value)

  expect_equal(lower_rr, qnorm(los_two / 2, mean_two, sd_two))
  expect_equal(upper_rr, qnorm(los_two / 2, mean_two, sd_two, lower.tail = FALSE))
  expect_equal(
    null_two,
    pnorm(lower_rr, mean_two, sd_two) +
      pnorm(upper_rr, mean_two, sd_two, lower.tail = FALSE)
  )
  expect_equal(
    alt_two,
    pnorm(lower_rr, prob2_two, alt_sd_two) +
      pnorm(upper_rr, prob2_two, alt_sd_two, lower.tail = FALSE)
  )
  expect_null(res_two$value)

  expect_snapshot(res_less$output)
  expect_snapshot(res_two$output)
})

test_that("iscamnormpower validates alternative input", {
  expect_error(
    iscamnormpower(
      LOS = 0.05,
      n = 50,
      prob1 = 0.5,
      alternative = "invalid",
      prob2 = 0.6
    ),
    "Check input for alternative"
  )
})

test_that("iscaminvnorm supports legacy Sd values", {
  res_above <- capture_plot_result(suppressWarnings(iscaminvnorm(
    prob1 = 0.2,
    mean = 2,
    Sd = 1.5,
    direction = "above"
  )))
  res_between <- capture_plot_result(suppressWarnings(iscaminvnorm(
    prob1 = 0.9,
    mean = 1,
    sd = 0.8,
    direction = "between"
  )))

  expect_null(res_above$value)
  expect_null(res_between$value)

  expect_snapshot(res_above$output)
  expect_snapshot(res_between$output)
})

test_that("iscaminvnorm validates Sd and sd consistency", {
  expect_error(
    iscaminvnorm(
      prob1 = 0.1,
      mean = 0,
      sd = 2,
      Sd = 3,
      direction = "below"
    ),
    "`Sd` is deprecated; use `sd`."
  )
})
