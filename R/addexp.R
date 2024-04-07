#' Overlay a Exponential Density Function on Histogram
#'
#' `add_exp` creates a histogram of `x` and overlays an exponential density function with \eqn{\lambda = \frac{1}{mean}}.
#'
#' @param x A numeric vector.
#'
#' @return A histogram of X overlayed with an exponential density function.
#' @importFrom stats dexp
#' @importFrom graphics abline grid hist lines mtext par
#' @export
#'
#' @examples
addexp <- function(x) {
  Description <- "iscamaddexp(x) \n This function creates a histogram of the inputted variable \n and overlays an exponetial density function with lambda = 1/mean."

  if (as.character(x[1]) == "?") stop(Description)
  par(mar = c(4, 3, 1, 1))

  min <- 0
  max <- max(x)
  myseq <- seq(min, max, .001)
  ymax <- max(dexp(myseq, 1 / mean(x)))

  hist(x, freq = FALSE, xlim = c(min, max), ylim = c(0, ymax * 1.1), yaxs = "i", col = "grey", add = F, main = "", xlab = "")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(x, freq = FALSE, xlab = deparse(substitute(x)), col = "grey", add = T)

  lines(myseq, dexp(myseq, 1 / mean(x)), col = "red")
  abline(h = 0, col = "black")
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}
