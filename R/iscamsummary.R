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
#' @export
#'
#' @examples
#' set.seed(0)
#' fake_data <- rnorm(30) # simulating some data
#' groups <- sample(c("group1","group2"), 30, TRUE)
#' summary(fake_data)
#' summary(fake_data, explanatory = groups, digits = 2) # with groups
summary <- function(x, explanatory = NULL, digits = 3) {
  if (is.null(explanatory)) {
    output <- .getSummaryStats(x)
  } else {
    output <- as.data.frame(do.call(
      rbind,
      tapply(x, explanatory, .getSummaryStats)
    ))
  }
  round(output, digits)
}

.getSummaryStats <- function(x) {
  curried_quantile <- function(prob) {
    unname(stats::quantile(x, probs = prob, na.rm = TRUE))
  }
  data.frame(
    Missing = sum(is.na(x)),
    n = length(x) - sum(is.na(x)),
    Min = min(x, na.rm = TRUE),
    Q1 = curried_quantile(0.25),
    Median = median(x, na.rm = TRUE),
    Q3 = curried_quantile(0.75),
    Max = max(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
  )
}
