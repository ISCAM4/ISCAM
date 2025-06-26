#' Some Summary Statistics
#'
#' `summary` calculates the five number summary, mean, and standard
#'  deviation of the quantitative variable `x`. An optional second, categorical
#'  variable can be specified and values will be calculated separately for
#'  each group. The number of digits in output can also be specified.
#'
#' @param x data to summarize.
#' @param explanatory optional explanatory variable to group by.
#' @param digits number of digits to round to.
#'
#' @return A table with some summary statistics of `x`.
#'
#' @importFrom withr local_options
#'
#' @export
#'
#' @examples
#' summary(rnorm(30)) # simulating some data
summary <- function(x, explanatory = NULL, digits = 3) {
  # TODO Deal with function name
  Description <- "iscamsummary(x, explanatory, digits) \n This function calculates the five number summary, mean, and standard deviation \n of the quantitative variable x \n Optional: A second, categorical variable can also be specified \n and values will be calculated separately for each group. \n Optional: Specify the number of digits in output to be different from 3."
  summary_colnames <- c(
    "Missing",
    "n",
    "Min",
    "Q1",
    "Median",
    "Q3",
    "Max",
    "Mean",
    "SD"
  )
  if (as.character(x[1]) == "?") {
    stop(Description)
  }
  withr::local_options(list("scipen" = 100)) # TODO See if this works
  if (is.null(explanatory)) {
    qq <- quantile(x, na.rm = TRUE)
    qq <- c(
      sum(is.na(x)),
      length(x) - sum(is.na(x)),
      c(qq[1L:5L], mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    )
    names(qq) <- summary_colnames
    print(
      gsub(
        substr(".000000000000000", 1, digits + 1),
        "",
        formatC(qq, digits = digits, format = "f"),
        fixed = T
      ),
      quote = F
    )
  } else {
    mylabels <- names(table(explanatory))
    lengths <- tapply(x, explanatory, length)
    middle <- tapply(x, explanatory, quantile, na.rm = TRUE)
    means <- tapply(x, explanatory, mean, na.rm = TRUE)
    sds <- tapply(x, explanatory, sd, na.rm = TRUE)
    missing <- 0

    grouparray <- matrix(
      nrow = length(mylabels),
      ncol = 9,
      dimnames = list(
        mylabels,
        summary_colnames
      )
    )

    for (i in 1:length(mylabels)) {
      names(middle)[i] <- "group1"
      missing[i] <- sum(is.na(x[explanatory == mylabels[i]]))
      grouparray[i, ] <- c(
        missing[i],
        lengths[i] - missing[i],
        formatC(
          c(middle[i]$group1, means[i], sds[i]),
          digits = digits,
          format = "f"
        )
      )
    }

    data.frame(rbind(grouparray))
  }
}
