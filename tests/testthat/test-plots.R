test_that("iscamboxplot works correctly with one variable", {
  vdiffr::expect_doppelganger("iscamboxplot-one-var", function() {
    iscamboxplot(
      mtcars$mpg,
      main = "mtcars Cylinders iscamdotplot",
      xlab = "Number of Cylinders"
    )
  })
})

test_that("iscamboxplot works correctly with two variables", {
  vdiffr::expect_doppelganger("iscamboxplot-two-vars", function() {
    iscamboxplot(
      mtcars$mpg,
      mtcars$am,
      main = "Automatic Cars Have Better Mileage on Average",
      xlab = "Mileage (miles per gallon)",
      ylab = "Automatic (yes coded as 1)"
    )
  })
})

test_that("iscamdotplot works correctly with one variable", {
  vdiffr::expect_doppelganger("iscamdotplot-one-var", function() {
    iscamdotplot(
      mtcars$cyl,
      main = "mtcars Cylinders iscamdotplot",
      xlab = "Number of Cylinders"
    )
  })
})

test_that("iscamdotplot works correctly with two variables", {
  vdiffr::expect_doppelganger("iscamdotplot-two-vars", function() {
    iscamdotplot(
      mtcars$mpg,
      mtcars$am,
      main = "Automatic Cars Have Better Mileage on Average",
      xlab = "Mileage (miles per gallon)",
      ylab = "Automatic (yes coded as 1)"
    )
  })
})
