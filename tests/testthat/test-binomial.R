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

  expect_snapshot(res_lower$output)
  expect_snapshot(res_upper$output)
  
  # Test error condition for invalid prob
  expect_error(
    iscambinomprob(k = 3, n = 10, prob = 1.5, lower.tail = TRUE),
    "prob.*must be.*between 0 and 1"
  )
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

  expect_snapshot(res_lower$output)
  expect_snapshot(res_upper$output)
})

test_that("iscambinomnorm executes for each direction", {
  expect_snapshot(
    capture_plot_result(iscambinomnorm(10, 20, 0.5, "below"))$output
  )
  expect_snapshot(
    capture_plot_result(iscambinomnorm(10, 20, 0.5, "above"))$output
  )
  # Test two.sided with k < normmean
  expect_snapshot(
    capture_plot_result(iscambinomnorm(4, 20, 0.5, "two.sided"))$output
  )
  # Test two.sided with k > normmean
  expect_snapshot(
    capture_plot_result(iscambinomnorm(16, 20, 0.5, "two.sided"))$output
  )
  # Test two.sided with k == normmean
  expect_snapshot(
    capture_plot_result(iscambinomnorm(10, 20, 0.5, "two.sided"))$output
  )
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

  expect_snapshot(res$output)
  
  # Test "less" alternative
  res_less <- capture_plot_result(suppressWarnings(iscambinompower(
    LOS = los,
    n = n,
    prob1 = prob1,
    alternative = "less",
    prob2 = 0.3
  )))
  expect_null(res_less$value)
  expect_snapshot(res_less$output)
  
  # Test "two.sided" alternative
  res_two <- capture_plot_result(suppressWarnings(iscambinompower(
    LOS = los,
    n = n,
    prob1 = prob1,
    alternative = "two.sided",
    prob2 = prob2
  )))
  expect_null(res_two$value)
  expect_snapshot(res_two$output)
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

  expect_snapshot(res$output)
  
  # Test "less" alternative
  res_less <- capture_plot_result(iscambinomtest(
    observed = 8,
    n = 30,
    hypothesized = 0.5,
    alternative = "less",
    conf.level = 0.95
  ))
  bt_less <- binom.test(8, 30, p = 0.5, alternative = "less")
  expect_equal(res_less$value$pvalue, signif(bt_less$p.value, 5), tolerance = 1e-6)
  expect_snapshot(res_less$output)
  
  # Test "greater" alternative
  res_greater <- capture_plot_result(iscambinomtest(
    observed = 22,
    n = 30,
    hypothesized = 0.5,
    alternative = "greater",
    conf.level = 0.95
  ))
  bt_greater <- binom.test(22, 30, p = 0.5, alternative = "greater")
  expect_equal(res_greater$value$pvalue, signif(bt_greater$p.value, 5), tolerance = 1e-6)
  expect_snapshot(res_greater$output)
  
  # Test with only conf.level (no alternative or hypothesized)
  res_conf_only <- capture_plot_result(iscambinomtest(
    observed = 18,
    n = 30,
    conf.level = 0.95
  ))
  expect_snapshot(res_conf_only$output)
  
  # Test with proportion instead of count (line 543)
  res_prop <- capture_plot_result(iscambinomtest(
    observed = 0.6,  # proportion
    n = 30,
    hypothesized = 0.5,
    alternative = "greater"
  ))
  expect_snapshot(res_prop$output)
  
  # Test with multiple conf.level values (lines 705, 750)
  res_multi_conf <- capture_plot_result(iscambinomtest(
    observed = 18,
    n = 30,
    conf.level = c(0.90, 0.95, 0.99)
  ))
  expect_snapshot(res_multi_conf$output)
})
