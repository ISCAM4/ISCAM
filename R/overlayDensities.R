#' Overlay an Exponential Density Function on Histogram
#'
#' `addexp` creates a histogram of `x` and overlays an exponential density
#'    function with \eqn{\lambda = \frac{1}{mean}}.
#'
#' @param x A numeric vector representing the data to be plotted.
#'
#' @returns A histogram of x overlayed with an exponential density function.
#'
#' @export
#'
#' @examples
addexp <- function(
  x,
  main = "Histogram with exponential curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  .internal_add_density(
    x,
    fun1 = function(z) dexp(z, 1 / mean(x, na.rm = TRUE)),
    label1 = "exponential",
    min_val = 0,
    xlab = xlab,
    main = main,
    bins = bins
  )
}


#' Overlay a Log Normal Density Function on Histogram
#'
#' `addlnorm` creates a histogram of `x` and overlays a log normal density function.
#'
#' @param x A numeric vector representing the data to be plotted.
#'
#' @returns A histogram of x overlayed with an log normal density function.
#'
#' @export
#'
#' @examples
addlnorm <- function(
  x,
  main = "Histogram with normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  mu <- mean(log(x), na.rm = TRUE)
  sdx <- sd(log(x), na.rm = TRUE)

  .internal_add_density(
    x,
    fun1 = function(z) dlnorm(z, mu, sdx),
    label1 = "log-normal",
    min_val = 0,
    bins = bins,
    xlab = xlab,
    main = main
  )
}

#' Overlay a Normal Density Function on Histogram
#'
#' `addnorm` creates a histogram of `x` and overlays a normal density function.

#' @param x A numeric vector representing the data to be plotted.
#' @param xlab What to label the x-axis.
#' @param title What to title the graph.
#' @param bins Number of bins.
#'
#' @returns A histogram of x overlayed with an normal density function.
#'
#' @export
#'
#' @examples
addnorm <- function(
  x,
  main = "Histogram with normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  mu <- mean(x, na.rm = TRUE)
  sdx <- sd(x, na.rm = TRUE)

  .internal_add_density(
    x,
    fun1 = function(z) dnorm(z, mu, sdx),
    label1 = "normal",
    min_val = mu - 3 * sdx,
    max_val = mu + 3 * sdx,
    bins = bins,
    xlab = xlab,
    main = main
  )
}

#' Overlay a t Density Function on Histogram
#'
#' @param x A numeric vector representing the data to be plotted.
#' @param df A numeric value representing the degrees of freedom of `x`.
#'
#' @returns A histogram of x overlayed with an t density function.
#'
#' @export
#'
#' @examples
addt <- function(
  x,
  df,
  main = "Histogram with t curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  mu <- mean(x, na.rm = TRUE)
  sdx <- sd(x, na.rm = TRUE)

  .internal_add_density(
    x,
    fun1 = function(z) dt((z - mu) / sdx, df) / sdx,
    label1 = paste0("t, df=", df),
    min_val = mu - 3 * sdx,
    max_val = mu + 3 * sdx,
    xlab = xlab,
    main = main,
    bins = bins
  )
}

#' Overlay a t Density Function and a Normal Density Function on Histogram
#'
#' @param x A numeric vector representing the data to be plotted.
#' @param df A numeric value representing the degrees of freedom of `x`.
#'
#' @returns A histogram of x overlayed with an t density function and a normal density function.
#'
#' @export
#'
#' @examples
addtnorm <- function(
  x,
  df,
  main = "Histogram with t and normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  mu <- mean(x, na.rm = TRUE)
  sdx <- sd(x, na.rm = TRUE)

  .internal_add_density(
    x,
    fun1 = function(z) dnorm((z - mu) / sdx) / sdx,
    col1 = 3,
    lty1 = 2,
    label1 = "normal",
    fun2 = function(z) dt((z - mu) / sdx, df) / sdx,
    col2 = 2,
    lty2 = 1,
    label2 = paste0("t, df=", df),
    main = main,
    min_val = mu - 3 * sdx,
    max_val = mu + 3 * sdx,
    bins = bins,
    xlab = xlab
  )
}

.internal_add_density <- function(
  x,
  fun1,
  col1 = 2,
  lty1 = 1,
  label1 = "curve 1",
  fun2 = NULL,
  col2 = 3,
  lty2 = 2,
  label2 = "curve 2",
  main,
  min_val = min(x, na.rm = TRUE),
  max_val = max(x, na.rm = TRUE),
  bins = NULL,
  xlab = "",
  ylab = "density",
  legend_pos = "topright"
) {
  withr::local_par(mar = c(4, 3, 1, 1))
  ylim_expand <- 1.05
  bins <- if (is.null(bins)) "Sturges" else bins

  seqx <- seq(min_val, max_val, length.out = 1001)
  ymax <- max(
    hist(x, plot = FALSE, breaks = bins)$density,
    fun1(seqx),
    if (!is.null(fun2)) fun2(seqx) else 0
  )

  hist(
    x,
    freq = FALSE,
    xlim = c(min_val, max_val),
    ylim = c(0, ymax * ylim_expand),
    col = "grey",
    main = "",
    xlab = "",
    ylab = "",
    breaks = bins
  )

  abline(h = 0)

  lines(seqx, fun1(seqx), col = col1, lty = lty1, lwd = 1.5)

  mtext(side = 1, line = 2, xlab)
  mtext(side = 2, line = 2, ylab)
  if (nzchar(main)) {
    title(main)
  }

  if (!is.null(fun2)) {
    lines(seqx, fun2(seqx), col = col2, lty = lty2, lwd = 1.5)
    legend(
      legend_pos,
      legend = c(label1, label2),
      col = c(col1, col2),
      lty = c(lty1, lty2),
      bty = "n",
      cex = 0.9
    )
  }

  invisible()
}
