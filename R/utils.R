.iscam_order_pair <- function(x, y) {
  if (y < x) {
    c(y, x)
  } else {
    c(x, y)
  }
}

.iscam_as_count <- function(observed, n) {
  if (observed < 1) {
    round(n * observed)
  } else {
    observed
  }
}

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

.iscam_normalize_conf_levels <- function(conf.level) {
  if (is.null(conf.level)) {
    return(NULL)
  }
  normalized <- conf.level
  normalized[normalized > 1] <- normalized[normalized > 1] / 100
  normalized
}

.iscam_print_probability <- function(verbose, showprob) {
  if (verbose) {
    cat(c("probability:", showprob), "\n")
  }
}

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

.iscam_binom_limits <- function(n, prob, include_upper = NULL) {
  spread <- 4 * sqrt(prob * (1 - prob) * n)
  limits <- c(max(0, n * prob - spread), min(n, n * prob + spread))
  if (!is.null(include_upper)) {
    limits[2] <- max(limits[2], include_upper)
  }
  limits
}

.iscam_binom_title <- function(n, prob) {
  substitute(
    paste("Binomial (", n == x1, ", ", pi == x2, ")", ),
    list(x1 = n, x2 = prob)
  )
}

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

.iscam_stop_invalid_direction <- function() {
  stop(
    "Use \"above\", \"below\", \"between\", or \"outside\" as the direction."
  )
}
