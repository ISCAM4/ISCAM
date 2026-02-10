#' Order two numeric values
#'
#' @param x First value.
#' @param y Second value.
#'
#' @return Length-2 numeric vector in ascending order.
#'
#' @keywords internal
#' @noRd
.iscam_order_pair <- function(x, y) {
  if (y < x) {
    c(y, x)
  } else {
    c(x, y)
  }
}

#' Convert proportion-like input to a count
#'
#' Treats values below 1 as a proportion of `n`, rounded to nearest count.
#'
#' @param observed Observed count or proportion.
#' @param n Sample size.
#'
#' @return Observed count.
#'
#' @keywords internal
#' @noRd
.iscam_as_count <- function(observed, n) {
  if (observed < 1) {
    round(n * observed)
  } else {
    observed
  }
}

#' Resolve and validate two-value interval bounds
#'
#' @param xval First value.
#' @param xval2 Optional second value.
#' @param direction Direction string.
#' @param default_xval2 Optional fallback function when `xval2` is `NULL`.
#'
#' @return Length-2 numeric vector of ordered bounds.
#'
#' @keywords internal
#' @noRd
.iscam_resolve_interval_bounds <- function(
  xval,
  xval2,
  direction,
  default_xval2 = NULL
) {
  if (direction %in% c("between", "outside") && is.null(xval2)) {
    stop("You need to specify a second observation value.")
  }

  if (is.null(xval2) && !is.null(default_xval2)) {
    xval2 <- default_xval2(xval)
  }

  if (is.null(xval2)) {
    return(c(xval, xval))
  }

  .iscam_order_pair(xval, xval2)
}

#' Normalize confidence levels to proportions
#'
#' Converts values above 1 to percentages divided by 100.
#'
#' @param conf.level Numeric confidence level(s) in `[0, 1]` or `[0, 100]`.
#'
#' @return Normalized confidence level(s) in `[0, 1]`.
#'
#' @keywords internal
#' @noRd
.iscam_normalize_conf_levels <- function(conf.level) {
  if (is.null(conf.level)) {
    return(NULL)
  }
  normalized <- conf.level
  normalized[normalized > 1] <- normalized[normalized > 1] / 100
  normalized
}

#' Print a standardized probability line
#'
#' @param verbose Logical controlling output.
#' @param showprob Probability string to print.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_print_probability <- function(verbose, showprob) {
  if (verbose) {
    cat(c("probability:", showprob), "\n")
  }
}

#' Map alternative string to symbolic comparator
#'
#' @param alternative Alternative hypothesis name.
#' @param include_not_equal Logical; if `TRUE`, map `"not.equal"` too.
#'
#' @return Single comparator symbol.
#'
#' @keywords internal
#' @noRd
.iscam_alt_symbol <- function(alternative, include_not_equal = TRUE) {
  if (include_not_equal) {
    switch(
      alternative,
      less = "<",
      greater = ">",
      two.sided = "<>",
      not.equal = "<>"
    )
  } else {
    switch(
      alternative,
      less = "<",
      greater = ">",
      two.sided = "<>"
    )
  }
}

#' Print standardized null and alternative hypotheses
#'
#' @param verbose Logical controlling output.
#' @param null_name Null-side parameter label.
#' @param alt_name Alternative-side parameter label.
#' @param hypothesized Null value.
#' @param alternative Alternative hypothesis name.
#' @param include_not_equal Logical passed to `.iscam_alt_symbol()`.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_print_hypotheses <- function(
  verbose,
  null_name,
  alt_name,
  hypothesized,
  alternative,
  include_not_equal = TRUE
) {
  if (!verbose) {
    return(invisible(NULL))
  }

  cat(
    paste("Null hypothesis       :", null_name, "=", hypothesized, sep = " "),
    "\n"
  )
  altname <- .iscam_alt_symbol(
    alternative,
    include_not_equal = include_not_equal
  )
  cat(
    paste(
      "Alternative hypothesis:",
      alt_name,
      altname,
      hypothesized,
      sep = " "
    ),
    "\n"
  )

  invisible(NULL)
}

#' Add baseline and axis labels to an existing base plot
#'
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param baseline_col Baseline color; `NULL` suppresses baseline.
#' @param x_line X-axis line offset.
#' @param y_line Y-axis line offset.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_add_plot_guides <- function(
  x_label,
  y_label,
  baseline_col = "gray",
  x_line = 2,
  y_line = 2
) {
  if (!is.null(baseline_col)) {
    abline(h = 0, col = baseline_col)
  }
  mtext(side = 1, line = x_line, x_label)
  mtext(side = 2, line = y_line, y_label)
}

#' Add standard-score tick labels on an axis
#'
#' Draws ticks for -3 through 3 standard units around a center.
#'
#' @param side Axis side passed to `axis()`.
#' @param center Center value (e.g., null mean).
#' @param scale One standard-unit distance in data units.
#' @param prefix Prefix label for ticks (e.g., `"z"` or `"t"`).
#' @param padj Label adjustment passed to `axis()`.
#' @param col.axis Axis label color.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_standard_score_axis <- function(
  side = 1,
  center,
  scale,
  prefix,
  padj = 1.2,
  col.axis = "blue"
) {
  ticks <- center + scale * (-3:3)
  labels <- paste0(prefix, "=", -3:3)
  axis(
    side = side,
    at = ticks,
    labels = labels,
    padj = padj,
    tick = FALSE,
    col.axis = col.axis
  )
}

#' Plot a continuous density-style curve with shared styling
#'
#' @param x X-values.
#' @param density_y Y-values for density/curve.
#' @param xlim Optional x-axis limits.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param baseline_col Baseline color; `NULL` suppresses baseline.
#' @param panel_grid Logical; add panel grid if `TRUE`.
#' @param line_type Plot line type.
#' @param lwd Line width.
#' @param x_line X-axis label line offset.
#' @param y_line Y-axis label line offset.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_plot_continuous_distribution <- function(
  x,
  density_y,
  xlim = NULL,
  x_label,
  y_label,
  baseline_col = "gray",
  panel_grid = TRUE,
  line_type = "l",
  lwd = 1,
  x_line = 2,
  y_line = 2
) {
  if (is.null(xlim)) {
    xlim <- range(x)
  }
  plot(
    x,
    density_y,
    xlim = xlim,
    type = line_type,
    xlab = "",
    ylab = "",
    lwd = lwd,
    panel.first = if (panel_grid) grid() else NULL
  )
  .iscam_add_plot_guides(
    x_label = x_label,
    y_label = y_label,
    baseline_col = baseline_col,
    x_line = x_line,
    y_line = y_line
  )
}

#' Plot a discrete mass-style chart with shared styling
#'
#' @param x X-values.
#' @param prob_y Probability heights.
#' @param xlim Optional x-axis limits.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param baseline_col Baseline color; `NULL` suppresses baseline.
#' @param panel_grid Logical; add panel grid if `TRUE`.
#' @param lwd Line width for spikes.
#' @param x_line X-axis label line offset.
#' @param y_line Y-axis label line offset.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_plot_discrete_distribution <- function(
  x,
  prob_y,
  xlim = NULL,
  x_label,
  y_label,
  baseline_col = "gray",
  panel_grid = TRUE,
  lwd = 2,
  x_line = 2,
  y_line = 2
) {
  if (is.null(xlim)) {
    xlim <- range(x)
  }
  plot(
    x,
    prob_y,
    xlab = " ",
    ylab = " ",
    type = "h",
    xlim = xlim,
    panel.first = if (panel_grid) grid() else NULL,
    lwd = lwd
  )
  .iscam_add_plot_guides(
    x_label = x_label,
    y_label = y_label,
    baseline_col = baseline_col,
    x_line = x_line,
    y_line = y_line
  )
}

#' Plot a one-dimensional confidence-interval strip
#'
#' Draws the compact CI strip used across tests/interval functions.
#'
#' @param min_x Left anchor value for the strip.
#' @param statistic Point estimate value.
#' @param max_x Right anchor value for the strip.
#' @param lower Lower confidence bound.
#' @param upper Upper confidence bound.
#' @param x_label X-axis label.
#' @param ci_label Text for the confidence-level label.
#' @param ci_label_x X-position where `ci_label` is drawn.
#' @param baseline_col Color for the vertical line at `statistic`.
#' @param digits Digits used when printing numeric labels.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_plot_ci_strip <- function(
  min_x,
  statistic,
  max_x,
  lower,
  upper,
  x_label,
  ci_label,
  ci_label_x = min_x,
  baseline_col = "gray",
  digits = 4
) {
  plot(
    c(min_x, statistic, max_x),
    c(1, 1, 1),
    pch = c(".", "^", "."),
    ylab = " ",
    xlab = x_label,
    ylim = c(1, 1)
  )
  abline(v = statistic, col = baseline_col)
  text(ci_label_x, 1, labels = ci_label)
  text(statistic, 0.9, labels = signif(statistic, digits))
  text(lower, 1, labels = signif(lower, digits), pos = 3)
  text(upper, 1, labels = signif(upper, digits), pos = 3)
  points(c(lower, upper), c(1, 1), pch = c("[", "]"))
  lines(c(lower, upper), c(1, 1))
  invisible(NULL)
}

#' Compute plotting limits for binomial distributions
#'
#' @param n Number of trials.
#' @param prob Success probability.
#' @param include_upper Optional value to force as minimum upper bound.
#'
#' @return Length-2 numeric vector of min/max x-limits.
#'
#' @keywords internal
#' @noRd
.iscam_binom_limits <- function(n, prob, include_upper = NULL) {
  spread <- 4 * sqrt(prob * (1 - prob) * n)
  limits <- c(max(0, n * prob - spread), min(n, n * prob + spread))
  if (!is.null(include_upper)) {
    limits[2] <- max(limits[2], include_upper)
  }
  limits
}

#' Build standard binomial plot title expression
#'
#' @param n Number of trials.
#' @param prob Success probability.
#'
#' @return Language object suitable for `title()`.
#'
#' @keywords internal
#' @noRd
.iscam_binom_title <- function(n, prob) {
  substitute(
    paste("Binomial (", n == x1, ", ", pi == x2, ")", ),
    list(x1 = n, x2 = prob)
  )
}

#' Compute exact binomial confidence interval bounds
#'
#' @param observed Observed successes.
#' @param n Number of trials.
#' @param conf.level Confidence level in `[0, 1]`.
#'
#' @return Named numeric vector with `lower` and `upper`.
#'
#' @keywords internal
#' @noRd
.iscam_binom_ci <- function(observed, n, conf.level) {
  alpha <- (1 - conf.level) / 2
  lower <- if (observed == 0) {
    0
  } else {
    qbeta(alpha, observed, n - observed + 1)
  }
  upper <- if (observed == n) {
    1
  } else {
    qbeta(1 - alpha, observed + 1, n - observed)
  }
  c(lower = lower, upper = upper)
}

#' Stop with standardized direction validation error
#'
#' @return No return value; always errors.
#'
#' @keywords internal
#' @noRd
.iscam_stop_invalid_direction <- function() {
  stop(
    "Use \"above\", \"below\", \"between\", or \"outside\" as the direction."
  )
}
