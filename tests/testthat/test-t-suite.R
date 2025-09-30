test_that("iscaminvt reports requested t quantiles", {
  res_below <- capture_plot_result(iscaminvt(
    0.05,
    df = 15,
    direction = "below"
  ))
  res_outside <- capture_plot_result(iscaminvt(
    0.1,
    df = 10,
    direction = "outside"
  ))

  expect_equal(
    res_below$value$answer,
    round(qt(0.05, 15, lower.tail = TRUE), 3),
    tolerance = 1e-6
  )
  expect_equal(
    res_outside$value$answer1,
    round(qt(0.05, 10, lower.tail = TRUE), 3),
    tolerance = 1e-6
  )
  expect_equal(
    res_outside$value$answer2,
    round(qt(0.95, 10, lower.tail = TRUE), 3),
    tolerance = 1e-6
  )
})

test_that("iscamtprob matches t tail probabilities", {
  res_below <- capture_plot_result(iscamtprob(
    xval = -2.05,
    df = 10,
    direction = "below"
  ))
  res_between <- capture_plot_result(iscamtprob(
    xval = -2,
    xval2 = 2,
    df = 12,
    direction = "between"
  ))

  expect_null(res_below$value)
  expect_null(res_between$value)

  expect_snapshot(res_below$output)
  expect_snapshot(res_between$output)
})

test_that("iscamonesamplet returns Welch statistics", {
  res <- capture_plot_result(suppressWarnings(iscamonesamplet(
    xbar = 2.5,
    sd = 1.2,
    n = 30,
    alternative = "greater",
    hypothesized = 2,
    conf.level = 0.95
  )))

  se <- 1.2 / sqrt(30)
  t_expected <- (2.5 - 2) / se
  p_expected <- pt(t_expected, df = 29, lower.tail = FALSE)
  critical <- qt((1 - 0.95) / 2, df = 29)
  lower <- 2.5 + critical * se
  upper <- 2.5 - critical * se

  expect_null(res$value)

  expect_snapshot(res$output)
})

test_that("iscamtwosamplet returns Welch two-sample results", {
  x1 <- 5
  sd1 <- 2
  n1 <- 30
  x2 <- 4
  sd2 <- 1.5
  n2 <- 28
  conf <- 0.95

  res <- capture_plot_result(suppressWarnings(iscamtwosamplet(
    x1 = x1,
    sd1 = sd1,
    n1 = n1,
    x2 = x2,
    sd2 = sd2,
    n2 = n2,
    hypothesized = 0,
    alternative = "two.sided",
    conf.level = conf
  )))

  se <- sqrt(sd1^2 / n1 + sd2^2 / n2)
  df_calc <- (sd1^2 / n1 + sd2^2 / n2)^2 /
    ((sd1^2 / n1)^2 / (n1 - 1) + (sd2^2 / n2)^2 / (n2 - 1))
  df_expected <- signif(df_calc, 4)
  t_expected <- (x1 - x2) / se
  p_expected <- 2 * pt(-abs(t_expected), df_expected)
  critical <- qt((1 - conf) / 2, df_expected)
  lower <- (x1 - x2) + critical * se
  upper <- (x1 - x2) - critical * se

  expect_null(res$value)

  expect_snapshot(res$output)
})
