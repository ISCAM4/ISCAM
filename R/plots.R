#' A box plot
#'
#' `boxplot` plots the given data in a box plot. If a second categorical variable
#'  is given, the data is grouped by this variable.
#'
#' @param x Vector of numeric values to plot.
#' @param explanatory Optional second categorical variable to group by.
#' @param main Optional title for the plot
#' @param xlab Optional x-axis label for the plot
#' @param ylab Optional y-axis for the plot
#'
#' @return A box plot.
#'
#' @export
#'
#'
#' @examples
boxplot <- function(
  x,
  explanatory = NULL,
  main = "",
  xlab = "",
  ylab = substitute(explanatory)
) {
  # TODO Masks graphics's boxplot?
  Description <- "iscamboxplot (x, explanatory, names) \n This function displays horizontal boxplot(s) utilizing quartiles instead of hinges to match the summary statistics. \n Optional: A second, categorical variable can also be specified \n and values will be calculated separately for each group. \n  Use the names input to specify the horizontal and vertical axis labels respectively."

  if (as.character(x[1]) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(4, 4, 2, 2))

  if (is.null(explanatory)) {
    withr::local_par(mar = c(4, 3, 2, 2))
    qq <- quantile(x, na.rm = TRUE)
    bp <- graphics::boxplot(x, plot = FALSE)
    bp$stats[2, 1] <- qq[2L]
    bp$stats[4, 1] <- qq[4L]
    bxp(bp, horizontal = TRUE, boxfill = "lightgrey")
  } else {
    mylabels <- names(table(explanatory))
    grouparray <- matrix(nrow = length(mylabels), ncol = 5)
    middle <- tapply(x, explanatory, quantile)
    proportions <- table(explanatory) / length(explanatory)
    bp <- graphics::boxplot(x ~ explanatory, plot = FALSE)
    for (i in 1:length(mylabels)) {
      names(middle)[i] <- "group1"
      grouparray[i, ] <- (middle[i]$group1)
      bp$stats[2, i] <- grouparray[i, 2]
      bp$stats[4, i] <- grouparray[i, 4]
    }

    bxp(bp, horizontal = TRUE, boxfill = "lightgrey", width = proportions)
    mtext(side = 2, line = 2, ylab)
  }
  mtext(side = 1, line = 2, xlab)
  invisible()
}

#' A dot plot
#'
#' `dotplot` creates a horizontal dot plot. If a second categorical variable is
#'    given, the data is grouped by this variable. Use `names` & `mytitle` to
#'    specify the labels and title.
#'
#' @param response Vector of numeric values to plot.
#' @param explanatory Optional second categorical variable to group by.
#' @param main Optional title for the plot
#' @param xlab Optional x-axis label for the plot
#' @param ylab Optional y-axis for the plot
#'
#' @return A dot plot.
#'
#' @export
#'
#' @examples
#' dotplot(mtcars$cyl, main = "mtcars Cylinders Dotplot", xlab = "Cylinders", ylab = "Counts")
dotplot <- function(
  response,
  explanatory = NULL,
  main = "",
  xlab = substitute(response),
  ylab = substitute(explanatory)
) {
  Description <- "iscamdotplot(response, explanatory, names, mytitle) \n This function creates horizontal dotplots. \n If no explanatory variable is specified, just one plot. \n If an explanatory variable is specified, parallel dotplots will be created. \n  Use the names input to specify the horizontal and vertical axis labels respectively and mytitle to specify main. Vertical axis cuts off at 1000."

  if (as.character(response[1]) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(5, 1, 5, 1))

  if (is.null(explanatory)) {
    stripchart(
      response,
      method = "stack",
      ylim = c(0, 1000),
      pch = 16,
      main = main,
      xlab = xlab,
      ylab = ylab
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
      main = main,
      xlab = xlab,
      ylab = ylab
    )
    for (i in 1:numCategories) {
      abline(h = i)
    }
  }
  invisible()
}
