#' Overlay a t Density Function on Histogram
#'
#' @param x A numeric vector.
#' @param df A numeric value.
#'
#' @returns A histogram of x overlayed with an t density function.
#'
#' @export
#'
#' @examples
addt <- function(x, df) {
  Description <- "iscamaddt(x, df) \n This function creates a histogram of the inputted variable \n and overlays a t density function with df degrees of freedom."

  if (as.character(x[1]) == "?") stop(Description)
  par(mar = c(4, 3, 1, 1))

  min <- min(x, mean(x) - 3 * sd(x))
  max <- max(x, mean(x) + 3 * sd(x))
  myseq <- seq(min, max, .001)
  myhist <- hist(x, freq = FALSE, xlim = c(min, max))
  ymax <- max(dt(myseq, df), myhist$density)
  hist(x, freq = FALSE, xlim = c(min, max), ylim = c(0, ymax * 1.05), main = "", xlab = "", ylab = "", yaxs = "i", col = "grey", add = F)
  # abline(h=seq(0,ymax, ymax/4), lty=3, col="light grey")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(x, freq = FALSE, xlim = c(min, max), ylim = c(0, ymax * 1.05), main = "", xlab = "", ylab = "", yaxs = "i", col = "grey", add = T)
  lines(myseq, dt(myseq, df), col = "red")
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}
