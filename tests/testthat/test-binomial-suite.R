library(stats)

test_that("iscambinomprob returns expected tail probabilities", {
  res_lower <- capture_plot_result(iscambinomprob(k = 3, n = 10, prob = 0.4, lower.tail = TRUE))
  res_upper <- capture_plot_result(iscambinomprob(k = 7, n = 10, prob = 0.4, lower.tail = FALSE))

  expect_snapshot({
    cat("lower_value:\n")
    print(res_lower$value)
    cat("lower_expected:", pbinom(3, 10, 0.4), "\n")
    cat("lower_output:\n")
    cat(collapse_output(res_lower$output), "\n")
    cat("---\n")
    cat("upper_value:\n")
    print(res_upper$value)
    cat("upper_expected:", 1 - pbinom(6, 10, 0.4), "\n")
    cat("upper_output:\n")
    cat(collapse_output(res_upper$output), "\n")
  })
})

test_that("iscaminvbinom solves the correct quantile", {
  res_lower <- capture_plot_result(iscaminvbinom(alpha = 0.1, n = 20, prob = 0.4, lower.tail = TRUE))
  res_upper <- capture_plot_result(iscaminvbinom(alpha = 0.1, n = 20, prob = 0.4, lower.tail = FALSE))

  expect_snapshot({
    cat("lower_value:\n")
    print(res_lower$value)
    cat("lower_expected:", qbinom(0.1, 20, 0.4, lower.tail = TRUE) - 1, "\n")
    cat("lower_output:\n")
    cat(collapse_output(res_lower$output), "\n")
    cat("---\n")
    cat("upper_value:\n")
    print(res_upper$value)
    cat("upper_expected:", qbinom(0.1, 20, 0.4, lower.tail = FALSE) + 1, "\n")
    cat("upper_output:\n")
    cat(collapse_output(res_upper$output), "\n")
  })
})

test_that("iscambinomnorm executes for each direction", {
  expect_snapshot({
    cat("below:\n")
    cat(collapse_output(capture_plot_result(iscambinomnorm(10, 20, 0.5, "below"))$output), "\n")
    cat("---\n")
    cat("above:\n")
    cat(collapse_output(capture_plot_result(iscambinomnorm(10, 20, 0.5, "above"))$output), "\n")
    cat("---\n")
    cat("two-sided:\n")
    cat(collapse_output(capture_plot_result(iscambinomnorm(10, 20, 0.5, "two.sided"))$output), "\n")
  })
})

test_that("iscambinompower reports rejection probabilities", {
  los <- 0.05
  n <- 20
  prob1 <- 0.5
  prob2 <- 0.6

  res <- capture_plot_result(suppressWarnings(iscambinompower(LOS = los, n = n, prob1 = prob1, alternative = "greater", prob2 = prob2)))
  rr <- qbinom(los, n, prob1, lower.tail = FALSE) + 1
  null_prob <- 1 - pbinom(rr - 1, n, prob1)
  alt_prob <- 1 - pbinom(rr - 1, n, prob2)

  expect_snapshot({
    cat("critical_rr:", rr, "\n")
    cat("null_prob:", null_prob, "\n")
    cat("alt_prob:", alt_prob, "\n")
    cat("value:\n")
    print(res$value)
    cat("output:\n")
    cat(collapse_output(res$output), "\n")
  })
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

  expect_snapshot({
    cat("value:\n")
    print(res$value)
    cat("expected_p: ", signif(bt$p.value, 5), "\n", sep = "")
    cat("expected_ci:", bt$conf.int[1], bt$conf.int[2], "\n")
    cat("output:\n")
    cat(collapse_output(res$output), "\n")
  })
})
