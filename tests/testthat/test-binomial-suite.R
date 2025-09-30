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
})
