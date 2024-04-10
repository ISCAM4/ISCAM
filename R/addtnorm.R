#' Overlay a t Density Function & a Normal Density Function on Histogram
#'
#' @param x A numeric vector representing the data to be plotted.
#' @param df A numeric value representing the degrees of freedom of `x`.
#'
#' @returns A histogram of x overlayed with an t density function and a normal density function.
#'
#' @export
#'
#' @examples
addtnorm <- function(x, df) {
  Description <- "iscamaddt(x, df) \n This function creates a histogram of the inputted variable \n and overlays a t density function with df degrees of freedom."

  if (as.character(x[1]) == "?") stop(Description)

  par(mar = c(4, 3, 1, 1))

  min <- min(x, mean(x) - 3 * sd(x))
  max <- max(x, mean(x) + 3 * sd(x))
  myseq <- seq(min, max, .001)
  myhist <- hist(x, freq = FALSE, xlim = c(min, max))
  ymax <- max(dt(myseq, df), myhist$density)
  hist(x, freq = FALSE, xlim = c(min, max), ylim = c(0, ymax * 1.05), main = "", xlab = "", ylab = "", yaxs = "i", col = "grey", add = F)
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(x, freq = FALSE, xlim = c(min, max), ylim = c(0, ymax * 1.05), main = "", xlab = "", ylab = "", yaxs = "i", col = "grey", add = T)
  lines(myseq, dnorm(myseq, 0, 1), col = 3)
  lines(myseq, dt(myseq, df), col = 2)
  legend("topleft", c("t", "normal"), text.col = c(2, 3))
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}
