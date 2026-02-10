test_that("internal utility null branches are covered", {
  expect_equal(
    ISCAM:::.iscam_resolve_interval_bounds(
      xval = 5,
      xval2 = NULL,
      direction = "above"
    ),
    c(5, 5)
  )

  expect_null(ISCAM:::.iscam_normalize_conf_levels(NULL))
})
