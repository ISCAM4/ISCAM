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
  p_expected <- signif(
    prop.test(
      35,
      50,
      p = 0.5,
      alternative = "greater",
      correct = FALSE
    )$p.value,
    4
  )
  critical <- qnorm((1 - 0.95) / 2)
  se_stat <- sqrt(statistic * (1 - statistic) / 50)
  lower <- statistic + critical * se_stat
  upper <- statistic - critical * se_stat

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 1e-6)
  expect_equal(res$value$lower, lower, tolerance = 1e-6)
  expect_equal(res$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res$output)
  
  # Test "less" alternative
  res_less <- capture_plot_result(suppressWarnings(iscamonepropztest(
    observed = 20,
    n = 50,
    hypothesized = 0.5,
    alternative = "less",
    conf.level = 0.95
  )))
  expect_snapshot(res_less$output)
  
  # Test "two.sided" alternative
  res_two <- capture_plot_result(suppressWarnings(iscamonepropztest(
    observed = 35,
    n = 50,
    hypothesized = 0.5,
    alternative = "two.sided",
    conf.level = 0.95
  )))
  expect_snapshot(res_two$output)
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
  p_expected <- signif(1 - pnorm(z_expected), 4)

  se_diff <- sqrt(p1 * (1 - p1) / 50 + p2 * (1 - p2) / 45)
  critical <- qnorm((1 - 0.95) / 2)
  lower <- diff_est + critical * se_diff
  upper <- diff_est - critical * se_diff

  expect_equal(res$value$zvalue, z_expected, tolerance = 1e-6)
  expect_equal(res$value$pvalue, p_expected, tolerance = 1e-6)
  expect_equal(res$value$lower, lower, tolerance = 1e-6)
  expect_equal(res$value$upper, upper, tolerance = 1e-6)

  expect_snapshot(res$output)
  
  # Test "less" alternative
  res_less <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    observed1 = 20,
    n1 = 50,
    observed2 = 28,
    n2 = 45,
    hypothesized = 0,
    alternative = "less",
    conf.level = 0.95
  )))
  expect_snapshot(res_less$output)
  
  # Test "two.sided" alternative
  res_two <- capture_plot_result(suppressWarnings(iscamtwopropztest(
    observed1 = 35,
    n1 = 50,
    observed2 = 28,
    n2 = 45,
    hypothesized = 0,
    alternative = "two.sided",
    conf.level = 0.95
  )))
  expect_snapshot(res_two$output)
})
