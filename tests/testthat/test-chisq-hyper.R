library(stats)

test_that("iscamchisqprob returns formatted upper-tail probability", {
  res <- capture_plot_result(iscamchisqprob(5, 3))

  expect_snapshot({
    cat("value:\n")
    print(res$value)
    cat("expected:", pchisq(5, 3, lower.tail = FALSE), "\n")
    cat("output:\n")
    cat(collapse_output(res$output), "\n")
  })
})

test_that("iscamhyperprob matches hypergeometric tails", {
  res_lower <- capture_plot_result(suppressWarnings(iscamhyperprob(k = 2, total = 20, succ = 5, n = 8, lower.tail = TRUE)))
  res_upper <- capture_plot_result(suppressWarnings(iscamhyperprob(k = 3, total = 20, succ = 5, n = 8, lower.tail = FALSE)))

  fail <- 20 - 5

  expect_snapshot({
    cat("lower_value:\n")
    print(res_lower$value)
    cat("lower_expected:", phyper(2, 5, fail, 8), "\n")
    cat("lower_output:\n")
    cat(collapse_output(res_lower$output), "\n")
    cat("---\n")
    cat("upper_value:\n")
    print(res_upper$value)
    cat("upper_expected:", 1 - phyper(2, 5, fail, 8), "\n")
    cat("upper_output:\n")
    cat(collapse_output(res_upper$output), "\n")
  })
})

test_that("iscamhypernorm reports tail probabilities and normal approximations", {
  res_lower <- capture_plot_result(suppressWarnings(iscamhypernorm(k = 2, total = 20, succ = 5, n = 8, lower.tail = TRUE)))
  res_upper <- capture_plot_result(suppressWarnings(iscamhypernorm(k = 3, total = 20, succ = 5, n = 8, lower.tail = FALSE)))

  expect_snapshot({
    cat("lower_value:\n")
    print(res_lower$value)
    cat("lower_output:\n")
    cat(collapse_output(res_lower$output), "\n")
    cat("---\n")
    cat("upper_value:\n")
    print(res_upper$value)
    cat("upper_output:\n")
    cat(collapse_output(res_upper$output), "\n")
  })
})
