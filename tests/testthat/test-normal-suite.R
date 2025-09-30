library(stats)

test_that("iscamnormprob returns formatted probabilities", {
  res_above <- capture_plot_result(suppressWarnings(iscamnormprob(1.96, direction = "above")))
  res_between <- capture_plot_result(suppressWarnings(iscamnormprob(-1, xval2 = 1, direction = "between")))

  expect_snapshot({
    cat("above_value:\n")
    print(res_above$value)
    cat("above_expected:", pnorm(1.96, lower.tail = FALSE), "\n")
    cat("above_output:\n")
    cat(collapse_output(res_above$output), "\n")
    cat("---\n")
    cat("between_value:\n")
    print(res_between$value)
    cat("between_expected:", pnorm(1) - pnorm(-1), "\n")
    cat("between_output:\n")
    cat(collapse_output(res_between$output), "\n")
  })
})

test_that("iscamnormpower reports null and alternative rejection rates", {
  los <- 0.05
  n <- 80
  prob1 <- 0.5
  prob2 <- 0.55

  res <- capture_plot_result(suppressWarnings(iscamnormpower(LOS = los, n = n, prob1 = prob1, alternative = "greater", prob2 = prob2)))

  mean1 <- prob1
  sd1 <- sqrt(prob1 * (1 - prob1) / n)
  rr <- qnorm(los, mean1, sd1, lower.tail = FALSE)
  null_prob <- 1 - pnorm(rr, mean1, sd1)

  mean2 <- prob2
  sd2 <- sqrt(prob2 * (1 - prob2) / n)
  alt_prob <- 1 - pnorm(rr, mean2, sd2)

  expect_snapshot({
    cat("rr:", rr, "\n")
    cat("null_prob:", null_prob, "\n")
    cat("alt_prob:", alt_prob, "\n")
    cat("value:\n")
    print(res$value)
    cat("output:\n")
    cat(collapse_output(res$output), "\n")
  })
})

test_that("iscaminvnorm reports requested quantiles", {
  res_below <- capture_plot_result(suppressWarnings(iscaminvnorm(0.05, direction = "below")))
  res_outside <- capture_plot_result(suppressWarnings(iscaminvnorm(0.1, direction = "outside")))

  expect_snapshot({
    cat("below_value:\n")
    print(res_below$value)
    cat("below_output:\n")
    cat(collapse_output(res_below$output), "\n")
    cat("---\n")
    cat("outside_value:\n")
    print(res_outside$value)
    cat("outside_output:\n")
    cat(collapse_output(res_outside$output), "\n")
  })
})
