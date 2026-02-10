test_that("internal utility null branches are covered", {
  expect_equal(
    # jarl-ignore internal_function: testing internal function
    ISCAM:::.iscam_resolve_interval_bounds(
      xval = 5,
      xval2 = NULL,
      direction = "above"
    ),
    c(5, 5)
  )

  # jarl-ignore internal_function: testing internal function
  expect_null(ISCAM:::.iscam_normalize_conf_levels(NULL))
})

test_that("two-sided helper sequences are symmetric around center", {
  # jarl-ignore internal_function: testing internal function
  out <- ISCAM:::.iscam_two_sided_draw_sequences(
    min_x = -10,
    max_x = 10,
    center = 2,
    statistic = 5,
    step = 1
  )

  expect_equal(out$left, seq(-10, -1, 1))
  expect_equal(out$right, seq(5, 10, 1))
})

test_that("discrete tail region helper works for both tails", {
  # jarl-ignore internal_function: testing internal function
  lower <- ISCAM:::.iscam_discrete_tail_regions(
    k = 3,
    lower_bound = 0,
    upper_bound = 10,
    lower.tail = TRUE,
    correction = 0.5,
    step = 1
  )
  # jarl-ignore internal_function: testing internal function
  upper <- ISCAM:::.iscam_discrete_tail_regions(
    k = 3,
    lower_bound = 0,
    upper_bound = 10,
    lower.tail = FALSE,
    correction = 0.5,
    step = 1
  )

  expect_equal(lower$tail_seq, 0:3)
  expect_equal(lower$corrected_seq, seq(0, 3.5, 1))
  expect_equal(lower$corrected_cutoff, 3.5)

  expect_equal(upper$tail_seq, 3:10)
  expect_equal(upper$corrected_seq, seq(2.5, 10, 1))
  expect_equal(upper$corrected_cutoff, 2.5)
})

test_that("normal approximation helper returns exact and corrected tails", {
  # jarl-ignore internal_function: testing internal function
  lower <- ISCAM:::.iscam_normal_tail_probs_with_cc(
    k = 4,
    mean = 5,
    sd = 2,
    lower.tail = TRUE,
    digits = 4
  )
  # jarl-ignore internal_function: testing internal function
  upper <- ISCAM:::.iscam_normal_tail_probs_with_cc(
    k = 4,
    mean = 5,
    sd = 2,
    lower.tail = FALSE,
    digits = 4
  )

  expect_equal(lower$prob, pnorm(4, 5, 2))
  expect_equal(lower$corrected_prob, pnorm(4.5, 5, 2))
  expect_equal(lower$showprob, format(lower$prob, digits = 4))
  expect_equal(
    lower$showprob_corrected,
    format(lower$corrected_prob, digits = 4)
  )

  expect_equal(upper$prob, pnorm(4, 5, 2, lower.tail = FALSE))
  expect_equal(upper$corrected_prob, pnorm(3.5, 5, 2, lower.tail = FALSE))
  expect_equal(upper$showprob, format(upper$prob, digits = 4))
  expect_equal(
    upper$showprob_corrected,
    format(upper$corrected_prob, digits = 4)
  )
})
