test_that("iscamonepropztest agrees with prop.test", {
  res <- capture_plot_result(suppressWarnings(iscamonepropztest(
    observed = 35,
    n = 50,
    hypothesized = 0.5,
    alternative = "greater",
    conf.level = 0.95
  )))

  statistic <- 35 / 50
  z_expected <- (statistic - 0.5) / sqrt(0.5 * (1 - 0.5) / 50)
  p_expected <- prop.test(
    35,
    50,
    p = 0.5,
    alternative = "greater",
    correct = FALSE
  )$p.value
  critical <- qnorm((1 - 0.95) / 2)
  se_stat <- sqrt(statistic * (1 - statistic) / 50)
  lower <- statistic + critical * se_stat
  upper <- statistic - critical * se_stat

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 5e-4)
  expect_equal(res$value$lower, lower, tolerance = 1e-6)
  expect_equal(res$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res$output)
})

test_that("iscamonepropztest handles less alternatives with proportion input", {
  res <- capture_plot_result(suppressWarnings(iscamonepropztest(
    observed = 0.32,
    n = 50,
    hypothesized = 0.35,
    alternative = "less",
    conf.level = 0.90
  )))

  successes <- round(0.32 * 50)
  statistic <- successes / 50
  z_expected <- (statistic - 0.35) / sqrt(0.35 * 0.65 / 50)
  p_expected <- pnorm(z_expected)
  critical <- qnorm((1 - 0.90) / 2)
  se_stat <- sqrt(statistic * (1 - statistic) / 50)
  lower <- statistic + critical * se_stat
  upper <- statistic - critical * se_stat

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 5e-4)
  expect_equal(res$value$lower, lower, tolerance = 1e-6)
  expect_equal(res$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res$output)
})

test_that("iscamonepropztest covers two-sided comparisons when sample is below null", {
  res <- capture_plot_result(suppressWarnings(iscamonepropztest(
    observed = 18,
    n = 60,
    hypothesized = 0.4,
    alternative = "two.sided"
  )))

  statistic <- 18 / 60
  z_expected <- (statistic - 0.4) / sqrt(0.4 * 0.6 / 60)
  p_expected <- 2 * pnorm(-abs(z_expected))

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 5e-4)

  expect_snapshot(res$output)
})

test_that("iscamonepropztest reports intervals when hypothesis is omitted", {
  res <- capture_plot_result(suppressWarnings(iscamonepropztest(
    observed = 28,
    n = 100,
    conf.level = 0.90
  )))

  statistic <- 28 / 100
  critical <- qnorm((1 - 0.90) / 2)
  se_stat <- sqrt(statistic * (1 - statistic) / 100)
  lower <- statistic + critical * se_stat
  upper <- statistic - critical * se_stat

  expect_null(res$value$zvalue)
  expect_null(res$value$pvalue)
  expect_equal(res$value$lower, lower, tolerance = 1e-6)
  expect_equal(res$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res$output)
})

test_that("iscamonepropztest covers two-sided branch above null and percentage levels", {
  res <- capture_plot_result(suppressWarnings(iscamonepropztest(
    observed = 38,
    n = 60,
    hypothesized = 0.4,
    alternative = "two.sided",
    conf.level = c(90, 95),
    verbose = FALSE
  )))

  expect_equal(length(res$value$lower), 2)
  expect_equal(length(res$value$upper), 2)
  expect_true(all(res$value$lower < res$value$upper))
})

test_that("iscamtwopropztest matches two-sample z test calculations", {
  res <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    observed1 = 35,
    n1 = 50,
    observed2 = 28,
    n2 = 45,
    hypothesized = 0,
    alternative = "greater",
    conf.level = 0.95
  )))

  p1 <- 35 / 50
  p2 <- 28 / 45
  diff_est <- p1 - p2
  pooled <- (35 + 28) / (50 + 45)
  z_expected <- (diff_est - 0) / sqrt(pooled * (1 - pooled) * (1 / 50 + 1 / 45))
  p_expected <- 1 - pnorm(z_expected)

  se_diff <- sqrt(p1 * (1 - p1) / 50 + p2 * (1 - p2) / 45)
  critical <- qnorm((1 - 0.95) / 2)
  lower <- diff_est + critical * se_diff
  upper <- diff_est - critical * se_diff

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 5e-4)
  expect_equal(res$value$lower, lower, tolerance = 1e-6)
  expect_equal(res$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res$output)
})

test_that("iscamtwopropztest handles less alternative with proportion input", {
  res <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    observed1 = 0.42,
    n1 = 60,
    observed2 = 0.5,
    n2 = 55,
    hypothesized = 0,
    alternative = "less",
    conf.level = 0.90
  )))

  successes1 <- round(0.42 * 60)
  successes2 <- round(0.5 * 55)
  p1 <- successes1 / 60
  p2 <- successes2 / 55
  diff_est <- p1 - p2
  pooled <- (successes1 + successes2) / (60 + 55)
  z_expected <- (diff_est - 0) / sqrt(pooled * (1 - pooled) * (1 / 60 + 1 / 55))
  p_expected <- pnorm(z_expected)
  se_diff <- sqrt(p1 * (1 - p1) / 60 + p2 * (1 - p2) / 55)
  critical <- qnorm((1 - 0.90) / 2)
  lower <- diff_est + critical * se_diff
  upper <- diff_est - critical * se_diff

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 5e-4)
  expect_equal(res$value$lower, lower, tolerance = 1e-6)
  expect_equal(res$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res$output)
})

test_that("iscamtwopropztest covers two-sided alternatives below the null", {
  res <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    observed1 = 20,
    n1 = 60,
    observed2 = 28,
    n2 = 60,
    hypothesized = 0,
    alternative = "two.sided"
  )))

  p1 <- 20 / 60
  p2 <- 28 / 60
  diff_est <- p1 - p2
  pooled <- (20 + 28) / (60 + 60)
  z_expected <- (diff_est - 0) / sqrt(pooled * (1 - pooled) * (1 / 60 + 1 / 60))
  p_expected <- 2 * pnorm(-abs(z_expected))

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 5e-4)

  expect_snapshot(res$output)
})

test_that("iscamtwopropztest accepts tabular input and confidence-only intervals", {
  res_alt <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    datatable = c(30, 20, 25, 25),
    hypothesized = 0,
    alternative = "greater"
  )))
  res_conf <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    observed1 = 0.62,
    n1 = 100,
    observed2 = 0.55,
    n2 = 90,
    conf.level = 0.90
  )))

  successes1 <- round(0.62 * 100)
  successes2 <- round(0.55 * 90)
  p1 <- successes1 / 100
  p2 <- successes2 / 90
  diff_est <- p1 - p2
  se_diff <- sqrt(p1 * (1 - p1) / 100 + p2 * (1 - p2) / 90)
  critical <- qnorm((1 - 0.90) / 2)
  lower <- diff_est + critical * se_diff
  upper <- diff_est - critical * se_diff

  expect_null(res_conf$value$zvalue)
  expect_null(res_conf$value$pvalue)
  expect_equal(res_conf$value$lower, lower, tolerance = 1e-6)
  expect_equal(res_conf$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res_alt$output)
  expect_snapshot(res_conf$output)
})

test_that("iscamtwopropztest covers two-sided branch above null and percentage levels", {
  res <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    observed1 = 45,
    n1 = 60,
    observed2 = 30,
    n2 = 60,
    hypothesized = 0,
    alternative = "two.sided",
    conf.level = c(90, 95),
    verbose = FALSE
  )))

  expect_equal(length(res$value$lower), 2)
  expect_equal(length(res$value$upper), 2)
  expect_true(all(res$value$lower < res$value$upper))
})

test_that("iscamonepropztest prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamonepropztest("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscamtwopropztest prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamtwopropztest("?"))
  expect_snapshot(collapse_output(help_lines))
})
