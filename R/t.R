#' Inverse T Calculation
#'
#' `invt` calculates the t quantile of a specified probability.
#'
#' @param prob Desired probability.
#' @param df Degrees of freedom
#' @param direction direction for probability calculation: "above", "below",
#'  "outside", "between".
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return The t value for the specified probability.
#' @export
#'
#' @examples
#' iscaminvt(0.05, df = 15, direction = "below")
#' iscaminvt(0.10, df = 25, direction = "above")
#' iscaminvt(0.95, df = 30, direction = "between")
#' iscaminvt(0.05, df = 20, direction = "outside")
iscaminvt <- function(prob, df, direction, verbose = TRUE) {
  if (.iscam_maybe_help(prob, "iscaminvt")) {
    return(invisible())
  }

  old <- par(mar = c(4, 3, 2, 2))
  on.exit(par(old), add = TRUE)

  min <- -4
  max <- 4
  ymax <- dt(0, df)
  thisx <- seq(min, max, by = max(0.001, abs(max - min) / 2000))
  .iscam_plot_continuous_distribution(
    x = thisx,
    density_y = dt(thisx, df),
    x_label = "t-values",
    y_label = "density",
    baseline_col = "black"
  )
  title(paste("t (df =", df, ")"))
  if (direction == "below") {
    answer <- signif(qt(prob, df, lower.tail = TRUE), 4)
    thisrange <- seq(min, answer, by = max(0.001, abs(answer - min) / 2000))
    # should we use min instead of zero?
    polygon(c(thisrange, answer, 0), c(dt(thisrange, df), 0, 0), col = "pink")
    text(
      (min + answer) / 2,
      dt(answer, df) / 2,
      labels = prob,
      pos = 2,
      col = "blue"
    )
    # jarl-ignore true_false_symbol: not using T as TRUE
    text(
      answer,
      min(dt(answer, df), ymax * 0.85),
      labels = bquote(T <= .(answer)),
      col = "red",
      pos = 3
    )
    if (verbose) {
      cat(
        "The observation with",
        prob,
        "probability below is",
        answer,
        "\n"
      )
    }
    invisible(list("answer" = answer))
  } else if (direction == "above") {
    answer <- signif(qt(prob, df, lower.tail = FALSE), 4)
    thisrange <- seq(answer, max, by = max(0.001, abs(max - answer) / 2000))
    polygon(c(answer, thisrange, max), c(0, dt(thisrange, df), 0), col = "pink")
    text(
      (answer + max) / 2,
      (dt(answer, df) / 2),
      labels = prob,
      pos = 4,
      col = "blue"
    )
    # jarl-ignore true_false_symbol: not using T as TRUE
    text(
      answer,
      min(dt(answer, df), ymax * 0.85),
      labels = bquote(T >= .(answer)),
      col = "red",
      pos = 3
    )
    if (verbose) {
      cat(
        "The observation with",
        prob,
        "probability above is",
        answer,
        "\n"
      )
    }
    invisible(list("answer" = answer))
  } else if (direction == "between") {
    answer1 <- signif(qt((1 - prob) / 2, df, lower.tail = TRUE), 4)
    answer2 <- .iscam_reflect_about(0, answer1)
    thisrange <- seq(answer1, answer2, by = max(0.001, abs(answer2 - answer1) / 2000))
    polygon(
      c(answer1, thisrange, answer2),
      c(0, dt(thisrange, df), 0),
      col = "pink"
    )
    text(0, (dt(0.5, df) / 2), labels = prob, col = "blue")
    # jarl-ignore true_false_symbol: not using T as TRUE
    text(
      answer1,
      min(dt(answer1, df), ymax * 0.85),
      labels = bquote(T <= .(answer1)),
      col = "red",
      pos = 3
    )
    # jarl-ignore true_false_symbol: not using T as TRUE
    text(
      answer2,
      min(dt(answer2, df), ymax * 0.85),
      labels = bquote(T <= .(answer2)),
      col = "red",
      pos = 3
    )
    if (verbose) {
      cat(
        "There is",
        prob,
        "probability between",
        answer1,
        "and",
        answer2,
        "\n"
      )
    }
    invisible(list("answer1" = answer1, "answer2" = answer2))
  } else if (direction == "outside") {
    answer1 <- signif(qt(prob / 2, df, lower.tail = TRUE), 4)
    answer2 <- .iscam_reflect_about(0, answer1)
    thisrange1 <- seq(min, answer1, by = max(0.001, abs(answer1 - min) / 2000))
    thisrange2 <- seq(answer2, max, by = max(0.001, abs(max - answer2) / 2000))
    polygon(
      c(min, thisrange1, answer1),
      c(0, dt(thisrange1, df), 0),
      col = "pink"
    )
    polygon(
      c(answer2, thisrange2, max),
      c(0, dt(thisrange2, df), 0),
      col = "pink"
    )
    text(
      answer1,
      dt(answer1, df) / 2,
      labels = prob / 2,
      col = "blue",
      pos = 2
    )
    text(
      answer2,
      dt(answer2, df) / 2,
      labels = prob / 2,
      col = "blue",
      pos = 4
    )
    # jarl-ignore true_false_symbol: not using T as TRUE
    text(
      answer1,
      min(dt(answer1, df), ymax * 0.85),
      labels = bquote(T <= .(answer1)),
      col = "red",
      pos = 3
    )
    # jarl-ignore true_false_symbol: not using T as TRUE
    text(
      answer2,
      min(dt(answer2, df), ymax * 0.85),
      labels = bquote(T >= .(answer2)),
      col = "red",
      pos = 3
    )
    if (verbose) {
      cat(
        "There is",
        prob,
        "probability outside",
        answer1,
        "and",
        answer2,
        "\n"
      )
    }
    invisible(list("answer1" = answer1, "answer2" = answer2))
  }
}

#' One Sample T-Test
#'
#' `onesamplet` calculates a one sample t-test and/or interval from summary statistics.
#' It defaults to a hypothesized population mean of 0. You can optionally set an
#' alternative hypothesis and confidence level for a two-sided confidence interval.
#'
#' @param xbar Observed mean.
#' @param sd Observed standard deviation.
#' @param n Sample size.
#' @param hypothesized Hypothesized population mean.
#' @param alternative "less", "greater", or "two.sided"
#' @param conf.level Confidence level.
#' @param verbose Logical; if `TRUE`, print textual descriptions of results.
#'
#' @return The t value, p value, and confidence interval.
#'
#' @export
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#' @examples
#' iscamonesamplet(
#'   xbar = 2.5,
#'   sd = 1.2,
#'   n = 30,
#'   alternative = "greater",
#'   hypothesized = 2
#' )
#' iscamonesamplet(
#'   xbar = 10.3,
#'   sd = 2,
#'   n = 50,
#'   alternative = "less",
#'   hypothesized = 11
#' )
#' iscamonesamplet(
#'   xbar = 98.2,
#'   sd = 2,
#'   n = 100,
#'   alternative = "two.sided",
#'   conf.level = 0.95
#' )
#' iscamonesamplet(xbar = 55, sd = 5, n = 40, conf.level = 0.99)
iscamonesamplet <- function(
  xbar,
  sd,
  n,
  hypothesized = 0,
  alternative = NULL,
  conf.level = NULL,
  verbose = TRUE
) {
  if (.iscam_maybe_help(xbar, "iscamonesamplet")) {
    return(invisible())
  }

  if (verbose) {
    cat("\nOne Sample t test\n\n", sep = "")
  }
  statistic <- xbar
  df <- n - 1
  se <- sd / sqrt(n)
  tvalue <- NULL
  pvalue <- NULL
  if (verbose) {
    cat(
      paste(
        "mean = ",
        xbar,
        ", sd = ",
        sd,
        ",  sample size = ",
        n,
        "\n",
        sep = ""
      )
    )
  }
  if (!is.null(alternative)) {
    .iscam_print_hypotheses(
      verbose = verbose,
      null_name = "mu",
      alt_name = "mu",
      hypothesized = hypothesized,
      alternative = alternative
    )

    tvalue <- (statistic - hypothesized) / se
    if (verbose) {
      cat("t-statistic:", signif(tvalue, 4), "\n")
    }
    min <- min(-4, tvalue - 0.001)
    diffmin <- min(
      hypothesized - 4 * se,
      hypothesized - abs(hypothesized - statistic) - 0.01
    )
    max <- max(4, tvalue + 0.001)
    diffmax <- max(
      hypothesized + 4 * se,
      hypothesized + abs(hypothesized - statistic) + 0.01
    )
    x <- seq(min, max, by = max(0.001, abs(max - min) / 2000))
    diffx <- x * se + hypothesized
    old <- par(mar = c(4, 3, 2, 2))
    on.exit(par(old), add = TRUE)
    plot(
      diffx,
      dt(x, df),
      xlab = bquote("<-" ~ "Sample Means" ~ "->"),
      ylab = " ",
      type = "l",
      ylim = c(0, dt(0, df)),
      panel.first = grid()
    )
    mtext(side = 2, line = 2, "density")
    .iscam_standard_score_axis(
      side = 1,
      center = hypothesized,
      scale = se,
      prefix = "t"
    )
    abline(h = 0, col = "black")
    title(paste("t (df=", df, ")"))
    if (alternative == "less") {
      pvalue <- pt(tvalue, df)
      drawseq <- seq(diffmin, statistic, by = max(0.001, abs(statistic - diffmin) / 2000))
      polygon(
        c(drawseq, statistic, diffmin),
        c(dt((drawseq - hypothesized) / se, df), 0, 0),
        col = "red"
      )
      text(
        diffmin,
        dt(0, df) * 0.9,
        labels = paste("t-statistic:", signif(tvalue, 3)),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * 0.8,
        labels = paste("p-value:", signif(pvalue, 4)),
        pos = 4,
        col = "red"
      )
    } else if (alternative == "greater") {
      pvalue <- 1 - pt(tvalue, df)
      drawseq <- seq(statistic, diffmax, by = max(0.001, abs(diffmax - statistic) / 2000))
      polygon(
        c(statistic, drawseq, diffmax),
        c(0, dt((drawseq - hypothesized) / se, df), 0),
        col = "red"
      )
      text(
        diffmax,
        dt(0, df) * 0.9,
        labels = paste("t-statistic:", signif(tvalue, 3)),
        pos = 2,
        col = "blue"
      )
      text(
        diffmax,
        dt(0, df) * 0.8,
        labels = paste("p-value:", signif(pvalue, 4)),
        pos = 2,
        col = "red"
      )
    } else if (.iscam_is_two_sided_alt(alternative)) {
      pvalue <- 2 * pt(-1 * abs(tvalue), df)
      drawseq <- .iscam_two_sided_draw_sequences(
        min_x = diffmin,
        max_x = diffmax,
        center = hypothesized,
        statistic = statistic
      )
      drawseq1 <- drawseq$left
      drawseq2 <- drawseq$right
      polygon(
        c(diffmin, drawseq1, drawseq1[length(drawseq1)]),
        c(0, dt((drawseq1 - hypothesized) / se, df), 0),
        col = "red"
      )
      polygon(
        c(drawseq2[1], drawseq2, diffmax),
        c(0, dt((drawseq2 - hypothesized) / se, df), 0),
        col = "red"
      )
      text(
        diffmin,
        dt(0, df) * 0.9,
        labels = paste("t-statistic:", signif(tvalue, 4)),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * 0.8,
        labels = paste("two-sided p-value:", signif(pvalue, 4)),
        pos = 4,
        col = "red"
      )
    }
  } # end test

  lower <- NULL
  upper <- NULL
  if (!is.null(conf.level)) {
    conf.level <- .iscam_normalize_conf_levels(conf.level)
    old <- par(mar = c(4, 0.5, 1.5, 0.5), mfrow = c(3, 1))
    on.exit(par(old), add = TRUE)
    if (length(conf.level) > 1) {
      old <- par(mar = c(4, 2, 1.5, 0.4), mfrow = c(length(conf.level), 1))
      on.exit(par(old), add = TRUE)
    }
    for (k in seq_along(conf.level)) {
      criticalvalue <- qt((1 - conf.level[k]) / 2, df)
      lower[k] <- statistic + criticalvalue * se
      upper[k] <- statistic - criticalvalue * se
      multconflevel <- 100 * conf.level[k]
      if (verbose) {
        cat(
          multconflevel,
          "% Confidence interval for mu: (",
          lower[k],
          ", ",
          upper[k],
          ") \n"
        )
      }
    }
    if (is.null(alternative)) {
      min <- statistic - 4 * se
      max <- statistic + 4 * se
      CIseq <- seq(min, max, by = max(0.001, abs(max - min) / 2000))
      if (length(conf.level) == 1) {
        old <- par(mar = c(4, 0.5, 1.5, 0.5), mfrow = c(3, 1))
        on.exit(par(old), add = TRUE)
        myxlab <- substitute(paste(mean == x1), list(x1 = signif(lower[1], 4)))
        plot(CIseq, dnorm(CIseq, lower[1], se), type = "l", xlab = " ")
        mtext("sample means", side = 1, line = 1.75, adj = 0.5, cex = 0.75)
        topseq <- seq(statistic, max, by = max(0.001, abs(max - statistic) / 2000))
        polygon(
          c(statistic, topseq, max),
          c(0, dnorm(topseq, lower[1], se), 0),
          col = "red"
        )
        myxlab <- substitute(
          paste("population mean", s == x1),
          list(x1 = signif(lower[1], 4))
        )
        title(myxlab)
        upper_curve <- seq(min, max, by = max(0.001, abs(max - min) / 2000))
        plot(
          upper_curve,
          dnorm(upper_curve, upper[1], se),
          type = "l",
          xlab = " "
        )
        mtext("sample means", side = 1, line = 1.75, adj = 0.5, cex = 0.75)
        bottomseq <- seq(min, statistic, by = max(0.001, abs(statistic - min) / 2000))
        polygon(
          c(min, bottomseq, statistic, statistic),
          c(
            0,
            dnorm(bottomseq, upper[1], se),
            dnorm(statistic, upper[1], se),
            0
          ),
          col = "red"
        )
        title(substitute(
          paste("population mean", s == x1),
          list(x1 = signif(upper[1], 4))
        ))
      } # just one interval
      for (k in seq_along(conf.level)) {
        .iscam_plot_ci_strip(
          min_x = min,
          statistic = statistic,
          max_x = max,
          lower = lower[k],
          upper = upper[k],
          x_label = "population mean",
          ci_label = paste(100 * conf.level[k], "% CI:"),
          ci_label_x = min * 1.01
        )
      }
    }
  }
  if (!is.null(alternative)) {
    if (verbose) {
      cat("p-value:", pvalue, "\n")
    }
  }
  old <- par(mfrow = c(1, 1))
  on.exit(par(old), add = TRUE)
  invisible()
}

#' Two Sample T-Test
#'
#' `twosamplet` calculates a two sample t-test and/or interval from summary data.
#' It defaults to a hypothesized population mean difference of 0. You can
#' optionally set an alternative hypothesis and confidence level for a two-sided
#' confidence interval.
#'
#' @param x1 Observed mean for group 1.
#' @param sd1 Observed standard deviation for group 1.
#' @param n1 Sample size for group 1.
#' @param x2 Observed mean for group 2.
#' @param sd2 Observed standard deviation for group 2.
#' @param n2 Sample size for group 2.
#' @param hypothesized Hypothesized difference in population means.
#' @param alternative "less", "greater", or "two.sided"
#' @param conf.level Confidence level.
#' @param verbose Logical; if `TRUE`, print textual descriptions of results.
#'
#' @return The t value, p value, and confidence interval.
#' @export
#'
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#' iscamtwosamplet(
#'   x1 = 25,
#'   sd1 = 5,
#'   n1 = 40,
#'   x2 = 22,
#'   sd2 = 6,
#'   n2 = 45,
#'   alternative = "greater"
#' )
#' iscamtwosamplet(
#'   x1 = 10,
#'   sd1 = 2,
#'   n1 = 50,
#'   x2 = 12,
#'   sd2 = 2.5,
#'   n2 = 50,
#'   alternative = "two.sided"
#' )
#' iscamtwosamplet(
#'   x1 = 8,
#'   sd1 = 1.5,
#'   n1 = 30,
#'   x2 = 5,
#'   sd2 = 1.8,
#'   n2 = 33,
#'   alternative = "greater",
#'   hypothesized = 2
#' )
#' iscamtwosamplet(
#'   x1 = 15,
#'   sd1 = 3,
#'   n1 = 25,
#'   x2 = 12,
#'   sd2 = 3.5,
#'   n2 = 28,
#'   conf.level = 0.95
#' )
iscamtwosamplet <- function(
  x1,
  sd1,
  n1,
  x2,
  sd2,
  n2,
  hypothesized = 0,
  alternative = NULL,
  conf.level = 0,
  verbose = TRUE
) {
  if (.iscam_maybe_help(x1, "iscamtwosamplet")) {
    return(invisible())
  }

  old <- par(mar = c(4, 3, 2, 2))
  on.exit(par(old), add = TRUE)
  if (verbose) {
    cat("\nTwo Sample t test\n\n", sep = "")
  }
  statistic1 <- x1
  statistic2 <- x2
  statistic <- statistic1 - statistic2
  df <- signif(
    (sd1 * sd1 / n1 + sd2 * sd2 / n2) *
      (sd1 * sd1 / n1 + sd2 * sd2 / n2) /
      ((sd1 * sd1 / n1)**2 / (n1 - 1) + (sd2**2 / n2)**2 / (n2 - 1)),
    4
  )
  unpooledsd <- sqrt(sd1 * sd1 / n1 + sd2 * sd2 / n2)

  if (verbose) {
    cat(
      paste(
        "Group1: mean = ",
        x1,
        ", sd = ",
        sd1,
        ",  sample size = ",
        n1,
        "\n",
        sep = ""
      )
    )
    cat(
      paste(
        "Group2: mean = ",
        x2,
        ", sd = ",
        sd2,
        ",  sample size = ",
        n2,
        "\n",
        sep = ""
      )
    )
    cat(paste("diff:", x1 - x2, "\n\n", sep = ""))
  }

  if (!is.null(alternative)) {
    .iscam_print_hypotheses(
      verbose = verbose,
      null_name = "mu1-mu2",
      alt_name = "mu1-mu2",
      hypothesized = hypothesized,
      alternative = alternative
    )

    tvalue <- (statistic1 - statistic2 - hypothesized) / unpooledsd
    if (verbose) {
      cat("t-statistic:", signif(tvalue, 4), "\n")
      cat("df:", signif(df, 4), "\n")
    }
    min <- min(-4, tvalue - 0.001)
    diffmin <- min(
      hypothesized - 4 * unpooledsd,
      min(
        hypothesized - 4 * unpooledsd,
        hypothesized - abs(hypothesized - statistic) - 0.001
      )
    )
    max <- max(4, tvalue + 0.001)
    diffmax <- max(
      hypothesized + 4 * unpooledsd,
      hypothesized + abs(hypothesized - statistic) + 0.001
    )

    x <- seq(min, max, by = max(0.001, abs(max - min) / 2000))
    diffx <- x * unpooledsd + hypothesized
    plot(
      diffx,
      dt(x, df),
      xlab = bquote("<-" ~ "Difference in Sample Means" ~ "->"),
      ylab = "",
      type = "l",
      ylim = c(0, dt(0, df)),
      panel.first = grid()
    )
    .iscam_standard_score_axis(
      side = 1,
      center = hypothesized,
      scale = unpooledsd,
      prefix = "t"
    )
    abline(h = 0, col = "black")
    mtext(side = 2, line = 2, "density")

    title(paste("t (df=", df, ")"))
    if (alternative == "less") {
      pvalue <- signif(pt(tvalue, df), 4)
      tvalue <- signif(tvalue, 4)
      drawseq <- seq(diffmin, statistic, by = max(0.001, abs(statistic - diffmin) / 2000))
      polygon(
        c(drawseq, statistic, diffmin),
        c(dt((drawseq - hypothesized) / unpooledsd, df), 0, 0),
        col = "red"
      )
      text(
        diffmin,
        dt(0, df) * 0.9,
        labels = paste("t-statistic:", tvalue),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * 0.8,
        labels = paste("p-value:", pvalue),
        pos = 4,
        col = "red"
      )
    } else if (alternative == "greater") {
      pvalue <- signif(1 - pt(tvalue, df), 4)
      tvalue <- signif(tvalue, 3)
      drawseq <- seq(statistic, diffmax, by = max(0.001, abs(diffmax - statistic) / 2000))
      polygon(
        c(statistic, drawseq, diffmax),
        c(0, dt((drawseq - hypothesized) / unpooledsd, df), 0),
        col = "red"
      )
      text(
        diffmax,
        dt(0, df) * 0.9,
        labels = paste("t-statistic:", tvalue),
        pos = 2,
        col = "blue"
      )
      text(
        diffmax,
        dt(0, df) * 0.8,
        labels = paste("p-value:", pvalue),
        pos = 2,
        col = "red"
      )
    } else if (.iscam_is_two_sided_alt(alternative)) {
      pvalue <- signif(2 * pt(-1 * abs(tvalue), df), 4)
      tvalue <- signif(tvalue, 4)
      drawseq <- .iscam_two_sided_draw_sequences(
        min_x = diffmin,
        max_x = diffmax,
        center = hypothesized,
        statistic = statistic
      )
      drawseq1 <- drawseq$left
      drawseq2 <- drawseq$right
      polygon(
        c(diffmin, drawseq1, drawseq1[length(drawseq1)]),
        c(0, dt((drawseq1 - hypothesized) / unpooledsd, df), 0),
        col = "red"
      )
      polygon(
        c(drawseq2[1], drawseq2, diffmax),
        c(0, dt((drawseq2 - hypothesized) / unpooledsd, df), 0),
        col = "red"
      )
      text(
        diffmin,
        dt(0, df) * 0.9,
        labels = paste("t-statistic:", tvalue),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * 0.8,
        labels = paste("two-sided p-value:", pvalue),
        pos = 4,
        col = "red"
      )
    }
  }
  lower <- NULL
  upper <- NULL

  if (conf.level != 0) {
    conf.level <- .iscam_normalize_conf_levels(conf.level)
    criticalvalue <- qt((1 - conf.level) / 2, df)
    lower <- statistic + criticalvalue * unpooledsd
    upper <- statistic - criticalvalue * unpooledsd
    multconflevel <- 100 * conf.level
    if (verbose) {
      cat(
        multconflevel,
        "% Confidence interval for mu1-mu2: (",
        lower,
        ", ",
        upper,
        ") \n"
      )
    }
    if (is.null(alternative)) {
      min <- statistic - 4 * unpooledsd
      max <- statistic + 4 * unpooledsd
      CIseq <- seq(min, max, by = max(0.001, abs(max - min) / 2000))
      old <- par(mar = c(4, 0.5, 1.5, 0.5), mfrow = c(3, 1))
      on.exit(par(old), add = TRUE)
      myxlab <- substitute(paste(mean == x1), list(x1 = signif(lower, 4)))
      plot(CIseq, dnorm(CIseq, lower, unpooledsd), type = "l", xlab = " ")
      mtext(
        "difference in sample means",
        side = 1,
        line = 1.75,
        adj = 0.5,
        cex = 0.75
      )
      topseq <- seq(statistic, max, by = max(0.001, abs(max - statistic) / 2000))
      polygon(
        c(statistic, topseq, max),
        c(0, dnorm(topseq, lower, unpooledsd), 0),
        col = "red"
      )
      myxlab <- substitute(
        paste("difference in population mean", s == x1),
        list(x1 = signif(lower, 4))
      )
      title(myxlab)
      upper_curve <- seq(min, max, by = max(0.001, abs(max - min) / 2000))
      plot(
        upper_curve,
        dnorm(upper_curve, upper, unpooledsd),
        type = "l",
        xlab = " "
      )
      mtext(
        "difference in sample means",
        side = 1,
        line = 1.75,
        adj = 0.5,
        cex = 0.75
      )
      bottomseq <- seq(min, statistic, by = max(0.001, abs(statistic - min) / 2000))
      polygon(
        c(min, bottomseq, statistic, statistic),
        c(
          0,
          dnorm(bottomseq, upper, unpooledsd),
          dnorm(statistic, upper, unpooledsd),
          0
        ),
        col = "red"
      )
      title(substitute(
        paste("difference in population mean", s == x1),
        list(x1 = signif(upper, 4))
      ))

      .iscam_plot_ci_strip(
        min_x = min,
        statistic = statistic,
        max_x = max,
        lower = lower,
        upper = upper,
        x_label = "difference in process means",
        ci_label = paste(multconflevel, "% CI:"),
        ci_label_x = min * 1.01
      )
    }
  }
  if (!is.null(alternative)) {
    if (verbose) {
      cat("p-value:", pvalue, "\n")
    }
    invisible(list(
      "tvalue" = tvalue,
      "df" = df,
      "pvalue" = pvalue,
      "lower" = lower,
      "upper" = upper
    ))
  }
  old <- par(mfrow = c(1, 1))
  on.exit(par(old), add = TRUE)
}

#' Tail Probability for t-distribution
#'
#' @param xval observed value.
#' @param df degrees of freedom.
#' @param direction direction for probability calculation, "above" or "below"; if
#'  "outside" or "between" are used, a second larger observation, `xval2` must be
#'  specified
#' @param xval2 second observation value.
#' @param verbose Logical; if `TRUE`, print textual descriptions of results.
#'
#' @return The tail probability in the specified direction using the given
#' arguments.
#' @export
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#' @examples
#' iscamtprob(xval = -2.05, df = 10, direction = "below")
#' iscamtprob(xval = 1.80, df = 20, direction = "above")
#' iscamtprob(xval = -2, xval2 = 2, df = 15, direction = "between")
#' iscamtprob(xval = -2.5, xval2 = 2.5, df = 25, direction = "outside")
iscamtprob <- function(xval, df, direction, xval2 = NULL, verbose = TRUE) {
  if (.iscam_maybe_help(xval, "iscamtprob")) {
    return(invisible())
  }

  old <- par(mar = c(4, 4, 2, 1))
  on.exit(par(old), add = TRUE)
  minx <- min(-5, -1 * abs(xval) - 0.5)
  maxx <- max(5, abs(xval) + 0.5)
  thisx <- seq(minx, maxx, by = max(0.001, abs(maxx - minx) / 2000))
  .iscam_plot_continuous_distribution(
    x = thisx,
    density_y = dt(thisx, df),
    xlim = c(minx, maxx),
    x_label = "t-values",
    y_label = "density"
  )

  if (direction == "below") {
    tprob <- pt(xval, df)
    showprob <- format(tprob, digits = 4)
    .iscam_shade_left_tail(
      min_x = minx,
      x_end = max(minx, xval),
      density_fn = function(v) dt(v, df)
    )
    text(
      minx,
      dt(0, df) / 2,
      labels = bquote(atop(P(X <= .(xval)), "=" ~ .(showprob))),
      col = "red",
      pos = 4
    )
  } else if (direction == "above") {
    tprob <- pt(xval, df, lower.tail = FALSE)
    showprob <- format(tprob, digits = 4)
    .iscam_shade_right_tail(
      x_start = min(maxx, xval),
      max_x = maxx,
      density_fn = function(v) dt(v, df)
    )
    text(
      maxx,
      dt(0, df) / 2,
      labels = bquote(atop(P(X >= .(xval)), "=" ~ .(showprob))),
      col = "red",
      pos = 2
    )
  } else if (direction == "between") {
    ordered_vals <- .iscam_resolve_interval_bounds(
      xval = xval,
      xval2 = xval2,
      direction = direction
    )
    xval <- ordered_vals[1]
    xval2 <- ordered_vals[2]
    tprob <- pt(xval2, df) - pt(xval, df)
    showprob <- format(tprob, digits = 4)
    .iscam_shade_between(
      x_start = xval,
      x_end = xval2,
      density_fn = function(v) dt(v, df)
    )
    text(
      minx,
      dt(0, df) / 2,
      # TODO: doubled X, but needed for plotmath syntax it looks like?
      labels = bquote(atop(
        P(.(xval) <= X, phantom(X) <= .(xval2)),
        "=" ~ .(showprob)
      )),
      col = "red",
      pos = 4
    )
  } else if (direction == "outside") {
    ordered_vals <- .iscam_resolve_interval_bounds(
      xval = xval,
      xval2 = xval2,
      direction = direction
    )
    xval <- ordered_vals[1]
    xval2 <- ordered_vals[2]
    maxx <- max(maxx, xval2)
    tprob <- 1 - (pt(xval2, df) - pt(xval, df))
    showprob <- format(tprob, digits = 4)
    .iscam_shade_left_tail(
      min_x = minx,
      x_end = xval,
      density_fn = function(v) dt(v, df),
      baseline_start = TRUE
    )
    .iscam_shade_right_tail(
      x_start = xval2,
      max_x = maxx,
      density_fn = function(v) dt(v, df)
    )
    text(
      -2,
      dt(0, df) / 2,
      labels = bquote(atop(
        P(X <= .(xval)) ~ "and" ~ P(X >= .(xval2)),
        "=" ~ .(showprob)
      ))
    )
  } else {
    .iscam_stop_invalid_direction()
  }

  title(substitute(paste("t(", df == x3, ")"), list(x3 = df)))
  .iscam_print_probability(verbose, showprob)
  invisible(showprob)
}
