test_that("iscamboxplot works correctly with one variable", {
  vdiffr::expect_doppelganger("iscamboxplot-one-variable", function() {
    iscamboxplot(
      mtcars$mpg,
      main = "mtcars Cylinders iscamdotplot",
      xlab = "Number of Cylinders"
    )
  })
  response <- mtcars$mpg
  res <- capture_plot_result(iscamboxplot(
    response,
    main = "mtcars Cylinders iscamdotplot",
    xlab = "Number of Cylinders"
  ))
  expect_equal(res$value, response)
  expect_snapshot(res$output)
})

test_that("iscamboxplot works correctly with two variables", {
  vdiffr::expect_doppelganger("iscamboxplot-two-variables", function() {
    iscamboxplot(
      mtcars$mpg,
      mtcars$am,
      main = "Automatic Cars Have Better Mileage on Average",
      xlab = "Mileage (miles per gallon)",
      ylab = "Automatic (yes coded as 1)"
    )
  })
  response <- mtcars$mpg
  res <- capture_plot_result(iscamboxplot(
    response,
    mtcars$am,
    main = "Automatic Cars Have Better Mileage on Average",
    xlab = "Mileage (miles per gallon)",
    ylab = "Automatic (yes coded as 1)"
  ))
  expect_equal(res$value, response)
  expect_snapshot(res$output)
})

test_that("iscamdotplot works correctly with one variable", {
  vdiffr::expect_doppelganger("iscamdotplot-one-variable", function() {
    iscamdotplot(
      mtcars$cyl,
      main = "mtcars Cylinders iscamdotplot",
      xlab = "Number of Cylinders"
    )
  })
  res <- capture_plot_result(iscamdotplot(
    mtcars$cyl,
    main = "mtcars Cylinders iscamdotplot",
    xlab = "Number of Cylinders"
  ))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscamdotplot works correctly with two variables", {
  vdiffr::expect_doppelganger("iscamdotplot-two-variables", function() {
    iscamdotplot(
      mtcars$mpg,
      mtcars$am,
      main = "Automatic Cars Have Better Mileage on Average",
      xlab = "Mileage (miles per gallon)",
      ylab = "Automatic (yes coded as 1)"
    )
  })
  res <- capture_plot_result(iscamdotplot(
    mtcars$mpg,
    mtcars$am,
    main = "Automatic Cars Have Better Mileage on Average",
    xlab = "Mileage (miles per gallon)",
    ylab = "Automatic (yes coded as 1)"
  ))
  expect_null(res$value)
  expect_snapshot(res$output)
})

test_that("iscamboxplot prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamboxplot("?"))
  expect_snapshot(collapse_output(head(help_lines, 12)))
  expect_snapshot(collapse_output(extract_help_section(help_lines, "Arguments")))
})

test_that("iscamdotplot prints help for question mark", {
  help_lines <- capture_help_output(ISCAM::iscamdotplot("?"))
  expect_snapshot(collapse_output(head(help_lines, 12)))
  expect_snapshot(collapse_output(extract_help_section(help_lines, "Arguments")))
})
