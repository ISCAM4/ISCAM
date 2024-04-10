#' Overlay a Log Normal Density Function on Histogram
#'
#' `addlnorm` creates a histogram of `x` and overlays a log normal density function.
#'
#' @param x A numeric vector representing the data to be plotted.
#'
#' @returns A histogram of x overlayed with an log normal density function.
#'
#' @importFrom stats dlnorm sd
#' @export
#'
#' @examples
addlnorm <- function(x) {
  # TODO Check this definition?
  Description <- "iscamaddlnorm(x) \n This function creates a histogram of the inputted variable \n and overlays an exponetial density function."

  if (as.character(x[1]) == "?") stop(Description)
  par(mar = c(4, 3, 1, 1))
  min <- 0
  max <- max(x)
  myseq <- seq(min, max, .001)
  ymax <- max(dlnorm(myseq, mean(log(x)), sd(log(x))))

  hist(x, freq = FALSE, xlab = "", col = "grey", xlim = c(min, max), ylim = c(0, ymax * 1.05), yaxs = "i", add = F, main = "")
  # hist(x, freq=FALSE, xlab="", col="grey", add=F)
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(x, freq = FALSE, xlab = "", col = "grey", xlim = c(min, max), ylim = c(0, ymax * 1.05), yaxs = "i", add = T)

  abline(h = 0, col = "black")
  lines(myseq, dlnorm(myseq, mean(log(x)), sd(log(x))), col = "red")
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}
