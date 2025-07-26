#' Template documentation for plotting parameters
#'
#' @param response Vector of numeric values to plot.
#' @param explanatory Optional second categorical variable to group by.
#' @param main Optional title for the plot
#' @param xlab Optional x-axis label for the plot
#' @param ylab Optional y-axis label for the plot. Only displayed when `explanatory` is provided.
#'
#' @keywords internal
#' @name .plot_funcs_common_params
NULL

#' A box plot
#'
#' `boxplot` plots the given data in a box plot. If a second categorical variable
#'  is given, the data is grouped by this variable.
#'
#' @inheritParams .plot_funcs_common_params
#'
#' @return A box plot.
#'
#' @export
#'
#' @examples
#' iscamboxplot(
#'   mtcars$mpg,
#'   main = "mtcars Cylinders Dotplot",
#'   xlab = "Number of Cylinders"
#' )
#' iscamboxplot(
#'   mtcars$mpg,
#'   mtcars$am,
#'   main = "Automatic Cars Have Better Mileage on Average",
#'   xlab = "Mileage (miles per gallon)",
#'   ylab = "Automatic (yes coded as 1)"
#' )
iscamboxplot <- function(
  response,
  explanatory = NULL,
  main = "",
  xlab = "",
  ylab = substitute(explanatory)
) {
  withr::local_par(mar = c(4, 4, 2, 2))

  if (is.null(explanatory)) {
    withr::local_par(mar = c(4, 3, 2, 2))
    qq <- quantile(response, na.rm = TRUE)
    bp <- graphics::boxplot(response, plot = FALSE)
    bp$stats[2, 1] <- qq[2L]
    bp$stats[4, 1] <- qq[4L]
    bxp(bp, horizontal = TRUE, boxfill = "lightgrey")
  } else {
    mylabels <- names(table(explanatory))
    grouparray <- matrix(nrow = length(mylabels), ncol = 5)
    middle <- tapply(response, explanatory, quantile)
    proportions <- table(explanatory) / length(explanatory)
    bp <- graphics::boxplot(response ~ explanatory, plot = FALSE)
    for (i in 1:length(mylabels)) {
      names(middle)[i] <- "group1"
      grouparray[i, ] <- (middle[i]$group1)
      bp$stats[2, i] <- grouparray[i, 2]
      bp$stats[4, i] <- grouparray[i, 4]
    }
    bxp(bp, horizontal = TRUE, boxfill = "lightgrey", width = proportions)
  }
  title(main = main, xlab = xlab, ylab = ylab)
  invisible(response)
}

#' A dot plot
#'
#' `dotplot` creates a horizontal dot plot. If a second categorical variable is
#'    given, the data is grouped by this variable. Use `names` & `mytitle` to
#'    specify the labels and title.
#'
#' @inheritParams .plot_funcs_common_params
#'
#' @return A dot plot.
#'
#' @export
#'
#' @examples
#' iscamdotplot(
#'   mtcars$cyl,
#'   main = "mtcars Cylinders Dotplot",
#'   xlab = "Number of Cylinders"
#' )
#' iscamdotplot(
#'   mtcars$mpg,
#'   mtcars$am,
#'   main = "Automatic Cars Have Better Mileage on Average",
#'   xlab = "Mileage (miles per gallon)",
#'   ylab = "Automatic (yes coded as 1)"
#' )
iscamdotplot <- function(
  response,
  explanatory = NULL,
  main = "",
  xlab = substitute(response),
  ylab = substitute(explanatory)
) {
  withr::local_par(mar = c(5, 1, 5, 1))

  if (is.null(explanatory)) {
    stripchart(
      response,
      method = "stack",
      ylim = c(0, 1000),
      pch = 16,
      xlab = ""
    )
    abline(h = 0)
  } else {
    numCategories <- length(table(explanatory))
    ymin <- .5
    ymax <- numCategories + .5
    withr::local_par(mar = c(5, 5, 5, 1))
    stripchart(
      response ~ explanatory,
      vertical = FALSE,
      method = "stack",
      ylim = c(ymin, ymax),
      pch = 16,
      xlab = ""
    )
    for (i in 1:numCategories) {
      abline(h = i)
    }
  }
  title(main = main, xlab = xlab, ylab = ylab)
  invisible()
}

# TODO: look at lattice::histogram
iscamhistogram <- function(...) lattice::histogram(...)
