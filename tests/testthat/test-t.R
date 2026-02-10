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
    qt(0.05, 15, lower.tail = TRUE),
    tolerance = 1e-3
  )
  expect_equal(
    res_outside$value$answer1,
    qt(0.05, 10, lower.tail = TRUE),
    tolerance = 1e-3
  )
  expect_equal(
    res_outside$value$answer2,
    qt(0.95, 10, lower.tail = TRUE),
    tolerance = 1e-3
  )
})

test_that("iscaminvt handles above and between directions", {
  res_above <- capture_plot_result(iscaminvt(
    0.1,
    df = 12,
    direction = "above"
  ))
  res_between <- capture_plot_result(iscaminvt(
    0.9,
    df = 8,
    direction = "between"
  ))

  expect_equal(
    res_above$value$answer,
    qt(0.1, 12, lower.tail = FALSE),
    tolerance = 1e-3
  )
  lower_between <- qt((1 - 0.9) / 2, 8, lower.tail = TRUE)
  upper_between <- -lower_between
  expect_equal(
    res_between$value$answer1,
    lower_between,
    tolerance = 1e-3
  )
  expect_equal(
    res_between$value$answer2,
    upper_between,
    tolerance = 1e-3
  )

  expect_snapshot(res_above$output)
  expect_snapshot(res_between$output)
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

  expected_below <- as.numeric(format(pt(-2.05, 10), digits = 4))
  expect_equal(as.numeric(res_below$value), expected_below)
  expect_equal(
    as.numeric(res_between$value),
    as.numeric(format(pt(2, 12) - pt(-2, 12), digits = 4))
  )
  expect_false(res_below$visible)
  expect_false(res_between$visible)

  expect_snapshot(res_below$output)
  expect_snapshot(res_between$output)
})

test_that("iscamtprob handles all branches and validates parameters", {
  res_above <- capture_plot_result(iscamtprob(
    xval = 1.65,
    df = 14,
    direction = "above"
  ))
  res_outside <- capture_plot_result(iscamtprob(
    xval = -1.2,
    xval2 = 2.1,
    df = 12,
    direction = "outside"
  ))

  expect_equal(
    as.numeric(res_above$value),
    as.numeric(format(pt(1.65, 14, lower.tail = FALSE), digits = 4))
  )
  expect_equal(
    as.numeric(res_outside$value),
    as.numeric(format(1 - (pt(2.1, 12) - pt(-1.2, 12)), digits = 4))
  )
  expect_false(res_above$visible)
  expect_false(res_outside$visible)

  expect_snapshot(res_above$output)
  expect_snapshot(res_outside$output)

  expect_error(
    iscamtprob(xval = 0, df = 10, direction = "between"),
    "specify a second observation value"
  )
  expect_error(
    iscamtprob(xval = 0, df = 10, direction = "outside"),
    "specify a second observation value"
  )
  expect_error(
    iscamtprob(xval = 0, df = 10, direction = "sideways"),
    "Use \"above\", \"below\", \"between\", or \"outside\" as the direction."
  )
})

test_that("iscamtprob reorders bounds for between and outside directions", {
  res_between <- capture_plot_result(iscamtprob(
    xval = 2.2,
    xval2 = -1.4,
    df = 12,
    direction = "between",
    verbose = FALSE
  ))
  res_outside <- capture_plot_result(iscamtprob(
    xval = 2.2,
    xval2 = -1.4,
    df = 12,
    direction = "outside",
    verbose = FALSE
  ))

  lower <- min(2.2, -1.4)
  upper <- max(2.2, -1.4)
  expect_equal(
    as.numeric(res_between$value),
    as.numeric(format(pt(upper, 12) - pt(lower, 12), digits = 4))
  )
  expect_equal(
    as.numeric(res_outside$value),
    as.numeric(format(1 - (pt(upper, 12) - pt(lower, 12)), digits = 4))
  )
  expect_false(res_between$visible)
  expect_false(res_outside$visible)
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

test_that("iscamonesamplet handles one-sided and two-sided hypotheses", {
  res_less <- capture_plot_result(suppressWarnings(iscamonesamplet(
    xbar = 4.8,
    sd = 1.1,
    n = 22,
    hypothesized = 5.2,
    alternative = "less"
  )))
  res_two <- capture_plot_result(suppressWarnings(iscamonesamplet(
    xbar = 7.1,
    sd = 1.5,
    n = 18,
    hypothesized = 7,
    alternative = "not.equal"
  )))

  se_less <- 1.1 / sqrt(22)
  t_less <- (4.8 - 5.2) / se_less
  p_less <- pt(t_less, df = 21)

  se_two <- 1.5 / sqrt(18)
  t_two <- (7.1 - 7) / se_two
  p_two <- 2 * pt(-abs(t_two), df = 17)

  output_less <- collapse_output(res_less$output)
  output_two <- collapse_output(res_two$output)

  expect_true(grepl("Alternative hypothesis: mu <", output_less, fixed = TRUE))
  expect_true(grepl("p-value:", output_less, fixed = TRUE))
  expect_true(grepl("Alternative hypothesis: mu <>", output_two, fixed = TRUE))
  expect_true(grepl("p-value:", output_two, fixed = TRUE))
  expect_null(res_less$value)
  expect_null(res_two$value)

  expect_snapshot(res_less$output)
  expect_snapshot(res_two$output)
})

test_that("iscamonesamplet prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamonesamplet("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamonesamplet returns confidence intervals without hypothesis", {
  res_conf <- capture_plot_result(suppressWarnings(iscamonesamplet(
    xbar = 0.62,
    sd = 0.2,
    n = 15,
    conf.level = c(95, 99)
  )))

  expect_null(res_conf$value)
  output_conf <- collapse_output(res_conf$output)
  expect_true(grepl(
    "95 % Confidence interval for mu:",
    output_conf,
    fixed = TRUE
  ))
  expect_true(grepl(
    "99 % Confidence interval for mu:",
    output_conf,
    fixed = TRUE
  ))

  expect_snapshot(res_conf$output)
})

test_that("iscamonesamplet reports single confidence interval without hypothesis", {
  res <- capture_plot_result(suppressWarnings(iscamonesamplet(
    xbar = 1.8,
    sd = 0.4,
    n = 20,
    conf.level = 0.95
  )))

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
  df_expected <- df_calc
  t_expected <- (x1 - x2) / se
  p_expected <- 2 * pt(-abs(t_expected), df_expected)
  critical <- qt((1 - conf) / 2, df_expected)
  lower <- (x1 - x2) + critical * se
  upper <- (x1 - x2) - critical * se

  expect_null(res$value)

  expect_snapshot(res$output)
})

test_that("iscamtwosamplet handles less alternative", {
  res <- capture_plot_result(suppressWarnings(iscamtwosamplet(
    x1 = 4.5,
    sd1 = 1.2,
    n1 = 18,
    x2 = 5.1,
    sd2 = 1.4,
    n2 = 20,
    hypothesized = 0,
    alternative = "less"
  )))

  se <- sqrt(1.2^2 / 18 + 1.4^2 / 20)
  df_calc <- (1.2^2 / 18 + 1.4^2 / 20)^2 /
    ((1.2^2 / 18)^2 / (18 - 1) + (1.4^2 / 20)^2 / (20 - 1))
  t_expected <- (4.5 - 5.1) / se
  p_expected <- pt(t_expected, df = df_calc)

  expect_null(res$value)
  output <- collapse_output(res$output)
  get_value <- function(label) {
    match <- regexpr(paste0(label, ": [-0-9.]+"), output)
    matched <- regmatches(output, match)
    as.numeric(sub(paste0(label, ": "), "", matched))
  }
  expect_equal(get_value("t-statistic"), t_expected, tolerance = 1e-3)
  expect_equal(get_value("df"), df_calc, tolerance = 1e-3)
  expect_equal(get_value("p-value"), p_expected, tolerance = 1e-4)

  expect_snapshot(res$output)
})

test_that("iscamtwosamplet handles greater alternative", {
  res <- capture_plot_result(suppressWarnings(iscamtwosamplet(
    x1 = 5.8,
    sd1 = 1.1,
    n1 = 22,
    x2 = 5.1,
    sd2 = 1.3,
    n2 = 20,
    hypothesized = 0,
    alternative = "greater",
    verbose = FALSE
  )))

  expect_null(res$value)
})

test_that("iscamtwosamplet provides intervals without hypothesis test", {
  res <- capture_plot_result(suppressWarnings(iscamtwosamplet(
    x1 = 6.2,
    sd1 = 1.1,
    n1 = 25,
    x2 = 5.8,
    sd2 = 1.3,
    n2 = 24,
    conf.level = 95
  )))

  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscaminvt prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscaminvt("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamtwosamplet prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamtwosamplet("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamtprob prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamtprob("?"))
  expect_snapshot(collapse_output(help_lines))
})
