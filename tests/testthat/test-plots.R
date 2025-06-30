test_that("boxplot works correctly with one variable", {
  vdiffr::expect_doppelganger("boxplot-one-var", function() {
    boxplot(
      mtcars$mpg,
      main = "mtcars Cylinders Dotplot",
      xlab = "Number of Cylinders"
    )
  })
})

test_that("boxplot works correctly with two variables", {
  vdiffr::expect_doppelganger("boxplot-two-vars", function() {
    boxplot(
      mtcars$mpg,
      mtcars$am,
      main = "Automatic Cars Have Better Mileage on Average",
      xlab = "Mileage (miles per gallon)",
      ylab = "Automatic (yes coded as 1)"
    )
  })
})

test_that("dotplot works correctly with one variable", {
  vdiffr::expect_doppelganger("dotplot-one-var", function() {
    dotplot(
      mtcars$cyl,
      main = "mtcars Cylinders Dotplot",
      xlab = "Number of Cylinders"
    )
  })
})

test_that("dotplot works correctly with two variables", {
  vdiffr::expect_doppelganger("dotplot-two-vars", function() {
    dotplot(
      mtcars$mpg,
      mtcars$am,
      main = "Automatic Cars Have Better Mileage on Average",
      xlab = "Mileage (miles per gallon)",
      ylab = "Automatic (yes coded as 1)"
    )
  })
})
