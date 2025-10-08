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
    round(pnorm(1.96, lower.tail = FALSE), 3),
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(res_between$value),
    round(pnorm(1) - pnorm(-1), 4),
    tolerance = 1e-6
  )

  expect_snapshot(res_above$output)
  expect_snapshot(res_between$output)
  
  # Test "below" direction
  res_below <- capture_plot_result(suppressWarnings(iscamnormprob(
    -1.5,
    direction = "below"
  )))
  expect_equal(
    as.numeric(res_below$value),
    round(pnorm(-1.5), 5),
    tolerance = 1e-4
  )
  expect_snapshot(res_below$output)
  
  # Test "outside" direction
  res_outside <- capture_plot_result(suppressWarnings(iscamnormprob(
    -1.5,
    xval2 = 1.5,
    direction = "outside"
  )))
  expect_equal(
    as.numeric(res_outside$value),
    round(1 - (pnorm(1.5) - pnorm(-1.5)), 4),
    tolerance = 1e-6
  )
  expect_snapshot(res_outside$output)
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

  expect_snapshot(res$output)
  
  # Test "less" alternative
  res_less <- capture_plot_result(suppressWarnings(iscamnormpower(
    LOS = los,
    n = n,
    prob1 = prob1,
    alternative = "less",
    prob2 = 0.45
  )))
  expect_null(res_less$value)
  expect_snapshot(res_less$output)
  
  # Test "two.sided" alternative
  res_two <- capture_plot_result(suppressWarnings(iscamnormpower(
    LOS = los,
    n = n,
    prob1 = prob1,
    alternative = "two.sided",
    prob2 = prob2
  )))
  expect_null(res_two$value)
  expect_snapshot(res_two$output)
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
  
  # Test "above" direction
  res_above <- capture_plot_result(suppressWarnings(iscaminvnorm(
    0.05,
    direction = "above"
  )))
  expect_null(res_above$value)
  expect_snapshot(res_above$output)
  
  # Test "between" direction
  res_between <- capture_plot_result(suppressWarnings(iscaminvnorm(
    0.95,
    direction = "between"
  )))
  expect_null(res_between$value)
  expect_snapshot(res_between$output)
})
