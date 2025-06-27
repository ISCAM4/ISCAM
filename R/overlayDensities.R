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
  xlab = NULL,
  main = "Histogram with exponential curve",
  bins = NULL
) {
  .internal_add_density(
    x,
    curves = list(list(
      fn = function(z) dexp(z, 1 / mean(x, na.rm = TRUE)),
      col = 2,
      lty = 1,
      label = "exponential"
    )),
    min = 0,
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
  xlab = NULL,
  bins = NULL
) {
  mu <- mean(log(x), na.rm = TRUE)
  sig <- sd(log(x), na.rm = TRUE)

  .internal_add_density(
    x,
    curves = list(list(
      fn = function(z) dlnorm(z, mu, sig),
      col = 2,
      lty = 1,
      label = "log‑normal"
    )),
    min = 0,
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
  xlab = NULL,
  bins = NULL
) {
  mu <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)

  .internal_add_density(
    x,
    curves = list(list(
      fn = function(z) dnorm(z, mu, sig),
      col = 2,
      lty = 1,
      label = "normal"
    )),
    min = mu - 3 * sig,
    max = mu + 3 * sig,
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
  xlab = NULL,
  bins = NULL
) {
  mu <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)

  .internal_add_density(
    x,
    curves = list(list(
      fn = function(z) dt((z - mu) / sig, df) / sig,
      col = 2,
      lty = 1,
      label = "t"
    )),
    min = mu - 3 * sig,
    max = mu + 3 * sig,
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
  xlab = NULL,
  bins = NULL
) {
  mu <- mean(x, na.rm = TRUE)
  sig <- sd(x, na.rm = TRUE)

  .internal_add_density(
    x,
    curves = list(
      list(
        fn = function(z) dnorm((z - mu) / sig) / sig,
        col = 3,
        lty = 2,
        label = "normal"
      ),
      list(
        fn = function(z) dt((z - mu) / sig, df) / sig,
        col = 2,
        lty = 1,
        label = paste0("t, df=", df)
      )
    ),
    main = main,
    min = mu - 3 * sig,
    max = mu + 3 * sig,
    bins = bins,
    xlab = xlab
  )
}


# -------------------------------------------------------------------------
#  Histogram + density overlay generator
# -------------------------------------------------------------------------
#  The goal is:
#    1. prepare a histogram
#    2. (optionally) drop a background grid
#    3. overlay one or more density curves
#    4. keep axis / title / legend logic consistent
# -------------------------------------------------------------------------

.internal_add_density <- function(
  x,
  curves, # list(list(fn,col,lty))
  main,
  min = min(x, na.rm = TRUE),
  max = max(x, na.rm = TRUE),
  bins = "Sturges",
  xlab = deparse(substitute(x)),
  ylab = "density",
  legend_pos = "topright"
) {
  stopifnot(is.list(curves), length(curves) >= 1)
  withr::local_par(mar = c(4, 3, 1, 1))
  ylim_expand <- 1.05

  seqx <- seq(min, max, length.out = 1001)

  ymax_curves <- vapply(curves, function(crv) max(crv$fn(seqx)), numeric(1))
  ymax <- max(c(ymax_curves, hist(x, plot = FALSE, breaks = bins)$density))

  hist(
    x,
    freq = FALSE,
    xlim = c(min, max),
    ylim = c(0, ymax * ylim_expand),
    col = "grey",
    main = "",
    xlab = "",
    ylab = "",
    breaks = bins
  )

  abline(h = 0)

  for (crv in curves) {
    lines(seqx, crv$fn(seqx), col = crv$col, lty = crv$lty, lwd = 1.5)
  }

  mtext(side = 1, line = 2, xlab)
  mtext(side = 2, line = 2, ylab)
  if (nzchar(main)) {
    title(main)
  }

  if (length(curves) > 1) {
    legend(
      legend_pos,
      legend = vapply(curves, `[[`, "", "label"),
      col = vapply(curves, function(z) z$col, numeric(1)),
      lty = vapply(curves, function(z) z$lty, numeric(1)),
      bty = "n",
      cex = 0.9
    )
  }
  invisible(h)
}
