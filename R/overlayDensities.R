#' Template documentation for plotting arguments
#'
#' @param x A numeric vector representing the data to be plotted.
#' @param main (optional) title for the plot.
#' @param xlab (optional) x-axis label for the plot.
#' @param bins (optional) number of bins for the histogram.
#'
#' @keywords internal
#' @name .add_density_common_params
NULL

#' Overlay an Exponential Density Function on Histogram
#'
#' `addexp` creates a histogram of `x` and overlays an exponential density
#'    function with \eqn{\lambda = \frac{1}{mean}}.
#'
#' @inheritParams .add_density_common_params
#' @returns A histogram of x overlayed with an exponential density function.
#'
#' @export
#'
#' @examples
#' set.seed(0)
#' x <- rexp(100, rate = 0.5)
#' iscamaddexp(x)
#' iscamaddexp(x, main = "Your Active Title", xlab = "Exponential Data", bins = 20)
iscamaddexp <- function(
  x,
  main = "Histogram with exponential curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  if (.iscam_maybe_help(x, "iscamaddexp")) {
    return(invisible())
  }

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
#' @inheritParams .add_density_common_params
#'
#' @returns A histogram of x overlayed with an log normal density function.
#'
#' @export
#'
#' @examples
#' set.seed(0)
#' x <- rlnorm(100)
#' iscamaddlnorm(x)
#' iscamaddlnorm(x, main = "Your Active Title", xlab = "Log Normal Data", bins = 20)
iscamaddlnorm <- function(
  x,
  main = "Histogram with log-normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  if (.iscam_maybe_help(x, "iscamaddlnorm")) {
    return(invisible())
  }

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
#'
#' @inheritParams .add_density_common_params
#'
#' @returns A histogram of x overlayed with an normal density function.
#'
#' @export
#'
#' @examples
#' set.seed(0)
#' x <- rnorm(100)
#' iscamaddnorm(x)
#' iscamaddnorm(x, main = "Your Active Title", xlab = "Normal Data", bins = 20)
iscamaddnorm <- function(
  x,
  main = "Histogram with normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  if (.iscam_maybe_help(x, "iscamaddnorm")) {
    return(invisible())
  }

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
#' @inheritParams .add_density_common_params
#' @param df A numeric value representing the degrees of freedom of `x`.
#'
#' @returns A histogram of x overlayed with an t density function.
#'
#' @export
#'
#' @examples
#' set.seed(0)
#' x <- rt(100, 30)
#' iscamaddt(x, 30)
#' iscamaddt(x, 30, main = "Your Active Title", xlab = "t Data", bins = 20)
iscamaddt <- function(
  x,
  df,
  main = "Histogram with t curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  if (.iscam_maybe_help(x, "iscamaddt")) {
    return(invisible())
  }

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
#' @inheritParams .add_density_common_params
#' @param df A numeric value representing the degrees of freedom of `x`.
#'
#' @returns A histogram of x overlayed with an t density function and a normal density function.
#'
#' @export
#'
#' @examples
#' set.seed(0)
#' x <- rt(100, 5)
#' iscamaddtnorm(x, 5)
#' iscamaddtnorm(x, 5, main = "Your Active Title", xlab = "t Data", bins = 20)
iscamaddtnorm <- function(
  x,
  df,
  main = "Histogram with t and normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
) {
  if (.iscam_maybe_help(x, "iscamaddtnorm")) {
    return(invisible())
  }

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

#' Common processing for plotting histogram and overlaying arbitrary densities
#'
#' This internal helper function handles the core logic of creating a histogram
#' from data and overlaying one or two density curves. It is not intended for
#' direct use.
#'
#' @param x A numeric vector of data to be plotted in the histogram.
#' @param fun1 A function representing the first density curve to overlay.
#' @param col1 Color for the first density curve. Defaults to 2 (red).
#' @param lty1 Line type for the first density curve. Defaults to 1 (solid).
#' @param label1 Legend label for the first density curve.
#' @param fun2 (optional) function for a second density curve. Defaults to `NULL`.
#' @param col2 Color for the second density curve. Defaults to 3 (green).
#' @param lty2 Line type for the second density curve. Defaults to 2 (dashed).
#' @param label2 Legend label for the second density curve.
#' @param min_val Minimum value for the x-axis range. Defaults to the minimum of `x`.
#' @param max_val Maximum value for the x-axis range. Defaults to the maximum of `x`.
#' @param bins Number of bins. Defaults to "Sturges".
#' @param main Main title for the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis. Defaults to "density".
#' @param legend_pos Position of the legend (e.g., "topright"). See `?legend` for details.
#'
#' @noRd
#' @keywords internal
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
  min_val = min(x, na.rm = TRUE),
  max_val = max(x, na.rm = TRUE),
  bins = NULL,
  main,
  xlab = "",
  ylab = "density",
  legend_pos = "topright"
) {
  old <- par(mar = c(4, 3, 1, 1))
  on.exit(par(old), add = TRUE)
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
