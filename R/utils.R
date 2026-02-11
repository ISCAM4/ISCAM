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

#' Choose one of two values based on tail direction
#'
#' @param lower.tail Logical tail selector.
#' @param lower_value Value to use when `lower.tail` is `TRUE`.
#' @param upper_value Value to use when `lower.tail` is `FALSE`.
#'
#' @return The selected value.
#'
#' @keywords internal
#' @noRd
.iscam_tail_choice <- function(lower.tail, lower_value, upper_value) {
  if (lower.tail) lower_value else upper_value
}

#' Reflect a value about a center point
#'
#' @param center Center point.
#' @param x Value to reflect.
#'
#' @return Reflected value `2 * center - x`.
#'
#' @keywords internal
#' @noRd
.iscam_reflect_about <- function(center, x) {
  2 * center - x
}

#' Compute symmetric two-sided cutpoints around a center
#'
#' @param center Center point.
#' @param x Observed value.
#'
#' @return Length-2 numeric vector `c(lower, upper)` of symmetric cutpoints.
#'
#' @keywords internal
#' @noRd
.iscam_two_sided_cutpoints <- function(center, x) {
  reflected <- .iscam_reflect_about(center, x)
  c(min(x, reflected), max(x, reflected))
}

#' Build x-sequences for symmetric two-tailed shading
#'
#' @param min_x Left plotting bound.
#' @param max_x Right plotting bound.
#' @param center Symmetry center.
#' @param statistic Observed statistic.
#' @param step Step size for sequences.
#'
#' @return Named list with `left` and `right` numeric sequences.
#'
#' @keywords internal
#' @noRd
.iscam_two_sided_draw_sequences <- function(
  min_x,
  max_x,
  center,
  statistic,
  step = 0.001
) {
  cutpoints <- .iscam_two_sided_cutpoints(center, statistic)
  left_step <- max(step, abs(cutpoints[1] - min_x) / 2000)
  right_step <- max(step, abs(max_x - cutpoints[2]) / 2000)
  list(
    left = seq(
      min_x,
      cutpoints[1],
      by = if (cutpoints[1] >= min_x) left_step else -left_step
    ),
    right = seq(
      cutpoints[2],
      max_x,
      by = if (max_x >= cutpoints[2]) right_step else -right_step
    )
  )
}

#' Check whether an alternative denotes a two-sided test
#'
#' @param alternative Alternative hypothesis label.
#'
#' @return `TRUE` for `"two.sided"` or `"not.equal"`.
#'
#' @keywords internal
#' @noRd
.iscam_is_two_sided_alt <- function(alternative) {
  alternative %in% c("two.sided", "not.equal")
}

#' Compute lower/upper tail probability for discrete distributions
#'
#' @param k Observed count.
#' @param lower.tail Logical; if `TRUE`, compute `P(X <= k)`, else `P(X >= k)`.
#' @param cdf_lower Function returning lower-tail cumulative probability at `k`.
#' @param cdf_upper Function returning upper-tail cumulative probability at `k`.
#' @param digits Digits used to format probability text.
#'
#' @return Named list with numeric `prob` and character `showprob`.
#'
#' @keywords internal
#' @noRd
.iscam_discrete_tail_probability <- function(
  k,
  lower.tail,
  cdf_lower,
  cdf_upper,
  digits = 4
) {
  prob <- .iscam_tail_choice(lower.tail, cdf_lower(k), cdf_upper(k))
  list(prob = prob, showprob = format(prob, digits = digits))
}

#' Build raw and continuity-corrected sequences for a discrete tail
#'
#' @param k Observed count cutoff.
#' @param lower_bound Left bound for plotting.
#' @param upper_bound Right bound for plotting.
#' @param lower.tail Logical tail selector.
#' @param correction Continuity correction applied to the cutoff.
#' @param step Sequence step size.
#'
#' @return Named list with `tail_seq`, `corrected_seq`, and `corrected_cutoff`.
#'
#' @keywords internal
#' @noRd
.iscam_discrete_tail_regions <- function(
  k,
  lower_bound,
  upper_bound,
  lower.tail,
  correction = 0.5,
  step = 0.001
) {
  corrected_cutoff <- .iscam_tail_choice(
    lower.tail,
    k + correction,
    k - correction
  )
  lower_step <- max(step, abs(k - lower_bound) / 2000)
  upper_step <- max(step, abs(upper_bound - k) / 2000)
  corrected_lower_step <- max(step, abs(corrected_cutoff - lower_bound) / 2000)
  corrected_upper_step <- max(step, abs(upper_bound - corrected_cutoff) / 2000)
  list(
    tail_seq = .iscam_tail_choice(
      lower.tail,
      seq(
        lower_bound,
        k,
        by = if (k >= lower_bound) lower_step else -lower_step
      ),
      seq(
        k,
        upper_bound,
        by = if (upper_bound >= k) upper_step else -upper_step
      )
    ),
    corrected_seq = .iscam_tail_choice(
      lower.tail,
      seq(
        lower_bound,
        corrected_cutoff,
        by = if (corrected_cutoff >= lower_bound) {
          corrected_lower_step
        } else {
          -corrected_lower_step
        }
      ),
      seq(
        corrected_cutoff,
        upper_bound,
        by = if (upper_bound >= corrected_cutoff) {
          corrected_upper_step
        } else {
          -corrected_upper_step
        }
      )
    ),
    corrected_cutoff = corrected_cutoff
  )
}

#' Compute normal-tail probabilities with continuity correction
#'
#' @param k Discrete cutoff value.
#' @param mean Mean of the normal approximation.
#' @param sd Standard deviation of the normal approximation.
#' @param lower.tail Logical tail selector.
#' @param correction Continuity correction amount.
#' @param digits Digits used to format probability text.
#'
#' @return Named list with exact and corrected probabilities (numeric + formatted).
#'
#' @keywords internal
#' @noRd
.iscam_normal_tail_probs_with_cc <- function(
  k,
  mean,
  sd,
  lower.tail,
  correction = 0.5,
  digits = 4
) {
  prob <- .iscam_tail_choice(
    lower.tail,
    pnorm(k, mean, sd, lower.tail = TRUE),
    pnorm(k, mean, sd, lower.tail = FALSE)
  )
  corrected_prob <- .iscam_tail_choice(
    lower.tail,
    pnorm(k + correction, mean, sd, lower.tail = TRUE),
    pnorm(k - correction, mean, sd, lower.tail = FALSE)
  )
  list(
    prob = prob,
    corrected_prob = corrected_prob,
    showprob = format(prob, digits = digits),
    showprob_corrected = format(corrected_prob, digits = digits)
  )
}

#' Build plotmath label for a discrete tail probability
#'
#' @param k Observed count.
#' @param showprob Formatted probability string.
#' @param lower.tail Logical; controls `<=` vs `>=` comparator.
#'
#' @return Plotmath expression.
#'
#' @keywords internal
#' @noRd
.iscam_discrete_tail_label <- function(k, showprob, lower.tail) {
  .iscam_tail_choice(
    lower.tail,
    bquote(atop(P(X <= .(k)), "=" ~ .(showprob))),
    bquote(atop(P(X >= .(k)), "=" ~ .(showprob)))
  )
}

#' Draw highlighted spikes for a discrete lower/upper tail
#'
#' @param k Observed count.
#' @param upper Maximum x value for right-tail highlighting.
#' @param lower.tail Logical; if `TRUE`, draw `0:k`, else draw `k:upper`.
#' @param pmf_fn Function returning mass values for integer x inputs.
#' @param col Line color.
#' @param lwd Line width.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_draw_discrete_tail_spikes <- function(
  k,
  upper,
  lower.tail,
  pmf_fn,
  col = "red",
  lwd = 2
) {
  x_vals <- .iscam_tail_choice(lower.tail, 0:k, k:upper)
  lines(x_vals, pmf_fn(x_vals), col = col, type = "h", lwd = lwd)
  invisible(NULL)
}

#' Print standardized discrete tail probability message
#'
#' @param verbose Logical controlling output.
#' @param k Observed count.
#' @param prob Numeric probability value.
#' @param lower.tail Logical; controls "below"/"above" wording.
#' @param prefix Prefix text shown at start of message.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_print_discrete_tail_probability <- function(
  verbose,
  k,
  prob,
  lower.tail,
  prefix = "Probability"
) {
  if (!verbose) {
    return(invisible(NULL))
  }
  direction_word <- .iscam_tail_choice(lower.tail, "below", "above")
  cat(prefix, k, "and", direction_word, "=", prob, "\n")
  invisible(NULL)
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

#' Plot base distribution scaffold with shared styling
#'
#' @param x X-values.
#' @param y Y-values.
#' @param xlim Optional x-axis limits.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param baseline_col Baseline color; `NULL` suppresses baseline.
#' @param panel_grid Logical; add panel grid if `TRUE`.
#' @param type Base plot type.
#' @param lwd Line width.
#' @param x_line X-axis label line offset.
#' @param y_line Y-axis label line offset.
#' @param xlab_raw Raw xlab argument passed to `plot()`.
#' @param ylab_raw Raw ylab argument passed to `plot()`.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_plot_distribution_base <- function(
  x,
  y,
  xlim = NULL,
  x_label,
  y_label,
  baseline_col,
  panel_grid,
  type,
  lwd,
  x_line,
  y_line,
  xlab_raw,
  ylab_raw
) {
  if (is.null(xlim)) {
    xlim <- range(x)
  }
  plot(
    x,
    y,
    xlim = xlim,
    type = type,
    xlab = xlab_raw,
    ylab = ylab_raw,
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
  invisible(NULL)
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
  .iscam_plot_distribution_base(
    x,
    density_y,
    xlim = xlim,
    x_label = x_label,
    y_label = y_label,
    baseline_col = baseline_col,
    panel_grid = panel_grid,
    type = line_type,
    lwd = lwd,
    x_line = x_line,
    y_line = y_line,
    xlab_raw = "",
    ylab_raw = ""
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
  .iscam_plot_distribution_base(
    x,
    prob_y,
    xlim = xlim,
    x_label = x_label,
    y_label = y_label,
    baseline_col = baseline_col,
    panel_grid = panel_grid,
    type = "h",
    lwd = lwd,
    x_line = x_line,
    y_line = y_line,
    xlab_raw = " ",
    ylab_raw = " "
  )
}

#' Shade a sequence under a curve with configurable polygon closure
#'
#' @param x_seq X-sequence to shade along.
#' @param density_fn Function returning y-values for `x_seq`.
#' @param col Fill color.
#' @param border Border color.
#' @param start_from_baseline Logical; if `TRUE`, polygon starts at baseline.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_shade_sequence <- function(
  x_seq,
  density_fn,
  col = "red",
  border = "red",
  start_from_baseline = TRUE
) {
  if (start_from_baseline) {
    poly_x <- c(x_seq[1], x_seq, x_seq[length(x_seq)])
    poly_y <- c(0, density_fn(x_seq), 0)
  } else {
    poly_x <- c(x_seq, x_seq[length(x_seq)], x_seq[1])
    poly_y <- c(density_fn(x_seq), 0, 0)
  }
  polygon(poly_x, poly_y, col = col, border = border)
  invisible(NULL)
}

#' Shade a left-tail region for a continuous curve
#'
#' @param min_x Leftmost x value.
#' @param x_end Tail cutoff on x-axis.
#' @param density_fn Function that returns y-values for a numeric x vector.
#' @param step Sequence step size.
#' @param col Fill color.
#' @param border Border color.
#' @param baseline_start Logical; if `TRUE`, start polygon at `(min_x, 0)`.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_shade_left_tail <- function(
  min_x,
  x_end,
  density_fn,
  step = 0.001,
  col = "red",
  border = "red",
  baseline_start = FALSE
) {
  tail_step <- max(step, abs(x_end - min_x) / 2000)
  .iscam_shade_sequence(
    x_seq = seq(
      min_x,
      x_end,
      by = if (x_end >= min_x) tail_step else -tail_step
    ),
    density_fn = density_fn,
    col = col,
    border = border,
    start_from_baseline = baseline_start
  )
}

#' Shade a right-tail region for a continuous curve
#'
#' @param x_start Tail cutoff on x-axis.
#' @param max_x Rightmost x value.
#' @param density_fn Function that returns y-values for a numeric x vector.
#' @param step Sequence step size.
#' @param col Fill color.
#' @param border Border color.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_shade_right_tail <- function(
  x_start,
  max_x,
  density_fn,
  step = 0.001,
  col = "red",
  border = "red"
) {
  tail_step <- max(step, abs(max_x - x_start) / 2000)
  .iscam_shade_sequence(
    x_seq = seq(
      x_start,
      max_x,
      by = if (max_x >= x_start) tail_step else -tail_step
    ),
    density_fn = density_fn,
    col = col,
    border = border,
    start_from_baseline = TRUE
  )
}

#' Shade a between-bounds region for a continuous curve
#'
#' @param x_start Left bound.
#' @param x_end Right bound.
#' @param density_fn Function that returns y-values for a numeric x vector.
#' @param step Sequence step size.
#' @param col Fill color.
#' @param border Border color.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
#' @noRd
.iscam_shade_between <- function(
  x_start,
  x_end,
  density_fn,
  step = 0.001,
  col = "red",
  border = "red"
) {
  between_step <- max(step, abs(x_end - x_start) / 2000)
  .iscam_shade_sequence(
    x_seq = seq(
      x_start,
      x_end,
      by = if (x_end >= x_start) between_step else -between_step
    ),
    density_fn = density_fn,
    col = col,
    border = border,
    start_from_baseline = TRUE
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
