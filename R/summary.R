#' Some Summary Statistics
#'
#' `summary` calculates the five number summary, mean, and standard
#'  deviation of the quantitative variable `x`. An optional second, categorical
#'  variable can be specified and values will be calculated separately for
#'  each group. The number of digits in output can also be specified. Skewness is
#'  sample skewness: \eqn{g_1 := \frac{m_3}{m_2^{3/2}}}, where
#'  \eqn{m_2 := \frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})^2}
#'  and \eqn{m_3 := \frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})^3} are the second
#'  and third central sample moments.
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
#' iscamsummary(fake_data)
#' iscamsummary(fake_data, explanatory = groups, digits = 2) # with groups
iscamsummary <- function(x, explanatory = NULL, digits = 3) {
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
    unname(quantile(x, probs = prob, na.rm = TRUE))
  }
  sample_skewness <- function(x) {
    x <- x[!is.na(x)]
    d <- x - mean(x)
    sum(d^3) / sum(d^2)^1.5 * length(x)^0.5
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
    SD = sd(x, na.rm = TRUE),
    Skewness = sample_skewness(x)
  )
}
