test_that("binomial-family functions match v1 plot snapshots", {
  expect_plot_vdiffr(
    "iscambinomprob-lower-tail",
    iscambinomprob(k = 3, n = 10, prob = 0.4, lower.tail = TRUE)
  )
  expect_plot_vdiffr(
    "iscaminvbinom-lower-tail",
    iscaminvbinom(alpha = 0.1, n = 20, prob = 0.4, lower.tail = TRUE)
  )
  expect_plot_vdiffr(
    "iscambinomnorm-below",
    iscambinomnorm(k = 10, n = 20, prob = 0.5, direction = "below")
  )
  expect_plot_vdiffr(
    "iscambinompower-greater",
    suppressWarnings(iscambinompower(
      LOS = 0.05,
      n = 20,
      prob1 = 0.5,
      alternative = "greater",
      prob2 = 0.6
    ))
  )
  expect_plot_vdiffr(
    "iscambinomtest-two-sided",
    iscambinomtest(
      observed = 18,
      n = 30,
      hypothesized = 0.5,
      alternative = "two.sided",
      conf.level = 0.95
    )
  )
})

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
  expect_false(res_lower$visible)
  expect_false(res_upper$visible)

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
  expect_false(res_lower$visible)
  expect_false(res_upper$visible)

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

test_that("iscambinomnorm handles two-sided alternatives", {
  res_lower <- capture_plot_result(iscambinomnorm(
    k = 6,
    n = 20,
    prob = 0.5,
    direction = "two.sided"
  ))
  res_upper <- capture_plot_result(iscambinomnorm(
    k = 14,
    n = 20,
    prob = 0.5,
    direction = "two.sided"
  ))

  expect_null(res_lower$value)
  expect_null(res_upper$value)

  expect_snapshot(res_lower$output)
  expect_snapshot(res_upper$output)
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
  expect_null(res$value)

  cutoff <- qbinom(los, n, prob1, lower.tail = FALSE) + 1
  cutoff_in_output <- as.integer(sub(
    ".*Probability ([0-9]+).*",
    "\\1",
    res$output[1]
  ))
  expect_equal(cutoff_in_output, cutoff)
  parse_prob <- function(line) {
    as.numeric(trimws(sub(".*= ", "", line)))
  }
  expect_equal(
    parse_prob(res$output[1]),
    pbinom(cutoff - 1, n, prob1, lower.tail = FALSE),
    tolerance = 1e-6
  )
  expect_equal(
    parse_prob(res$output[2]),
    pbinom(cutoff - 1, n, prob2, lower.tail = FALSE),
    tolerance = 1e-6
  )

  expect_snapshot(res$output)
})

test_that("iscambinompower handles less and two-sided cases", {
  los_less <- 0.1
  n_less <- 25
  prob1_less <- 0.35
  prob2_less <- 0.25
  los_two <- 0.05
  n_two <- 18
  prob1_two <- 0.55
  prob2_two <- 0.7

  res_less <- capture_plot_result(iscambinompower(
    LOS = los_less,
    n = n_less,
    prob1 = prob1_less,
    alternative = "less",
    prob2 = prob2_less
  ))
  res_two <- capture_plot_result(iscambinompower(
    LOS = los_two,
    n = n_two,
    prob1 = prob1_two,
    alternative = "two.sided",
    prob2 = prob2_two
  ))
  expect_null(res_less$value)
  expect_null(res_two$value)

  cutoff_less <- qbinom(los_less, n_less, prob1_less) - 1
  less_line <- res_less$output[1]
  expect_equal(
    as.integer(sub(".*Probability ([0-9]+).*", "\\1", less_line)),
    cutoff_less
  )
  parse_tail <- function(line) {
    as.numeric(trimws(sub(".*= ", "", line)))
  }
  expect_equal(
    parse_tail(res_less$output[1]),
    pbinom(cutoff_less, n_less, prob1_less),
    tolerance = 1e-4
  )
  expect_equal(
    parse_tail(res_less$output[2]),
    pbinom(cutoff_less, n_less, prob2_less),
    tolerance = 1e-4
  )

  lower_rr <- qbinom(los_two / 2, n_two, prob1_two) - 1
  upper_rr <- qbinom(los_two / 2, n_two, prob1_two, lower.tail = FALSE) + 1
  parse_region <- function(line) {
    as.numeric(trimws(sub(".*region ", "", line)))
  }
  expect_equal(
    parse_region(res_two$output[1]),
    pbinom(lower_rr, n_two, prob1_two) +
      pbinom(upper_rr - 1, n_two, prob1_two, lower.tail = FALSE),
    tolerance = 1e-4
  )
  expect_equal(
    parse_region(res_two$output[2]),
    pbinom(lower_rr, n_two, prob2_two) +
      pbinom(upper_rr - 1, n_two, prob2_two, lower.tail = FALSE),
    tolerance = 1e-4
  )

  expect_snapshot(res_less$output)
  expect_snapshot(res_two$output)
})

test_that("iscambinompower validates alternative input", {
  expect_error(
    iscambinompower(
      LOS = 0.05,
      n = 20,
      prob1 = 0.5,
      alternative = "invalid",
      prob2 = 0.6
    ),
    "Check input for alternative"
  )
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

  expect_equal(res$value$pvalue, bt$p.value, tolerance = 5e-5)
  expect_equal(res$value$lower, bt$conf.int[1], tolerance = 5e-5)
  expect_equal(res$value$upper, bt$conf.int[2], tolerance = 5e-5)

  expect_snapshot(res$output)
})

test_that("iscambinomtest handles one-sided alternatives", {
  res_less <- capture_plot_result(iscambinomtest(
    observed = 6,
    n = 15,
    hypothesized = 0.4,
    alternative = "less"
  ))
  res_greater <- capture_plot_result(iscambinomtest(
    observed = 11,
    n = 18,
    hypothesized = 0.45,
    alternative = "greater"
  ))

  expect_equal(
    res_less$value$pvalue,
    pbinom(6, 15, 0.4, lower.tail = TRUE),
    tolerance = 5e-5
  )
  expect_equal(
    res_greater$value$pvalue,
    pbinom(10, 18, 0.45, lower.tail = FALSE),
    tolerance = 5e-5
  )

  expect_snapshot(res_less$output)
  expect_snapshot(res_greater$output)
})

test_that("iscambinomtest converts proportion inputs and multiple confidence levels", {
  res <- capture_plot_result(iscambinomtest(
    observed = 0.14,
    n = 100,
    alternative = NULL,
    conf.level = c(90, 95)
  ))

  successes <- round(0.14 * 100)
  levels <- c(0.90, 0.95)
  alphas <- (1 - levels) / 2
  expected_lower <- sapply(
    alphas,
    function(alpha) qbeta(alpha, successes, 100 - successes + 1)
  )
  expected_upper <- sapply(
    alphas,
    function(alpha) qbeta(1 - alpha, successes + 1, 100 - successes)
  )

  expect_equal(res$value$lower, expected_lower, tolerance = 5e-5)
  expect_equal(res$value$upper, expected_upper, tolerance = 5e-5)

  expect_snapshot(res$output)
})

test_that("iscambinomtest treats not.equal as two-sided", {
  res <- capture_plot_result(iscambinomtest(
    observed = 13,
    n = 24,
    hypothesized = 0.5,
    alternative = "not.equal"
  ))

  bt <- binom.test(13, 24, p = 0.5, alternative = "two.sided")

  expect_equal(res$value$pvalue, bt$p.value, tolerance = 5e-5)
  expect_snapshot(res$output)
})

test_that("iscambinomtest returns single confidence intervals without hypothesis", {
  res <- capture_plot_result(iscambinomtest(
    observed = 18,
    n = 40,
    conf.level = 0.90
  ))

  successes <- 18
  n <- 40
  alpha <- (1 - 0.90) / 2
  lower <- qbeta(alpha, successes, n - successes + 1)
  upper <- qbeta(1 - alpha, successes + 1, n - successes)

  expect_equal(res$value$lower, lower, tolerance = 5e-5)
  expect_equal(res$value$upper, upper, tolerance = 5e-5)
  expect_snapshot(res$output)
})

test_that("iscambinomnorm exercises two-sided adjustment edge cases", {
  res_plus <- capture_plot_result(iscambinomnorm(
    k = 0,
    n = 5,
    prob = 0.25,
    direction = "two.sided"
  ))
  res_minus <- capture_plot_result(iscambinomnorm(
    k = 2,
    n = 5,
    prob = 0.3,
    direction = "two.sided"
  ))

  expect_null(res_plus$value)
  expect_null(res_minus$value)
})

test_that("iscambinomtest handles extreme confidence interval boundaries", {
  res_zero <- capture_plot_result(iscambinomtest(
    observed = 0,
    n = 10,
    conf.level = 0.95,
    verbose = FALSE
  ))
  res_full <- capture_plot_result(iscambinomtest(
    observed = 10,
    n = 10,
    conf.level = 0.95,
    verbose = FALSE
  ))

  expect_equal(res_zero$value$lower, 0)
  expect_equal(res_full$value$upper, 1)
})

test_that("iscambinomprob validates probability range", {
  expect_error(
    iscambinomprob(k = 3, n = 10, prob = 1.2, lower.tail = TRUE),
    "must be a numeric value between 0 and 1"
  )
})

test_that("iscambinomnorm prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscambinomnorm("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscambinompower prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscambinompower("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscambinomprob prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscambinomprob("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscambinomtest prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscambinomtest("?"))
  expect_snapshot(collapse_output(help_lines))
})

test_that("iscaminvbinom prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscaminvbinom("?"))
  expect_snapshot(collapse_output(help_lines))
})
