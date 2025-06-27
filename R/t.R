#' Inverse T Calculation
#'
#' `invt` calculates the t quantile of a specified probability.
#'
#' @param prob Desired probability.
#' @param df Degrees of freedom
#' @param direction direction for probability calculation: "above", "below",
#'  "outside", "between".
#'
#' @return The t value for the specified probability.
#' @export
#'
#' @examples
invt <- function(prob, df, direction) {
  withr::local_par(mar = c(4, 3, 2, 2))

  min <- -4
  max <- 4
  ymax <- dt(0, df)
  thisx <- seq(min, max, .001)
  plot(
    thisx,
    dt(thisx, df),
    xlab = "",
    ylab = "density",
    type = "l",
    panel.first = grid()
  )
  title(paste("t (df =", df, ")"))
  mtext(side = 1, line = 2, "t-values")
  mtext(side = 2, line = 2, "density")

  abline(h = 0, col = "black")
  if (direction == "below") {
    answer <- signif(qt(prob, df, lower.tail = TRUE), 4)
    thisrange <- seq(min, answer, .001)
    # should we use min instead of zero?
    polygon(c(thisrange, answer, 0), c(dt(thisrange, df), 0, 0), col = "pink")
    text(
      (min + answer) / 2,
      dt(answer, df) / 2,
      labels = prob,
      pos = 2,
      col = "blue"
    )
    text(
      answer,
      min(dt(answer, df), ymax * .85),
      labels = paste("T\u2264 ", answer),
      col = "red",
      pos = 3
    )
    cat("The observation with", prob, "probability below is", answer, "\n")
    invisible(list("answer" = answer))
  } else if (direction == "above") {
    answer <- signif(qt(prob, df, lower.tail = FALSE), 4)
    thisrange <- seq(answer, max, .001)
    polygon(c(answer, thisrange, max), c(0, dt(thisrange, df), 0), col = "pink")
    text(
      (answer + max) / 2,
      (dt(answer, df) / 2),
      labels = prob,
      pos = 4,
      col = "blue"
    )
    text(
      answer,
      min(dt(answer, df), ymax * .85),
      labels = paste("T\u2265", answer),
      col = "red",
      pos = 3
    )
    cat("The observation with", prob, "probability above is", answer, "\n")
    invisible(list("answer" = answer))
  } else if (direction == "between") {
    answer1 <- signif(qt((1 - prob) / 2, df, lower.tail = TRUE), 4)
    answer2 <- 0 + (0 - answer1)
    thisrange <- seq(answer1, answer2, .001)
    polygon(
      c(answer1, thisrange, answer2),
      c(0, dt(thisrange, df), 0),
      col = "pink"
    )
    text(0, (dt(.5, df) / 2), labels = prob, col = "blue")
    text(
      answer1,
      min(dt(answer1, df), ymax * .85),
      labels = paste("T\u2264", answer1),
      col = "red",
      pos = 3
    )
    text(
      answer2,
      min(dt(answer2, df), ymax * .85),
      labels = paste("T\u2264", answer2),
      co = "red",
      pos = 3
    )
    cat("There is", prob, "probability between", answer1, "and", answer2, "\n")
    invisible(list("answer1" = answer1, "answer2" = answer2))
  } else if (direction == "outside") {
    answer1 <- signif(qt(prob / 2, df, lower.tail = TRUE), 4)
    answer2 <- 0 + (0 - answer1)
    thisrange1 <- seq(min, answer1, .001)
    thisrange2 <- seq(answer2, max, .001)
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
    text(
      answer1,
      min(dt(answer1, df), ymax * .85),
      labels = paste("T\u2264", answer1),
      col = "red",
      pos = 3
    )
    text(
      answer2,
      min(dt(answer2, df), ymax * .85),
      labels = paste("T\u225", answer2),
      col = "red",
      pos = 3
    )
    cat("There is", prob, "probability outside", answer1, "and", answer2, "\n")
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
#'
#' @return The t value, p value, and confidence interval.
#'
#' @export
#'
#' @examples
onesamplet <- function(
  xbar,
  sd,
  n,
  hypothesized = 0,
  alternative = NULL,
  conf.level = NULL
) {
  Description <- "iscamonesamplet(xbar, sd, n,  hypothesized=0, alternative = NULL, conf.level =0\n This function calculates a one sample t-test and/or interval from summary statistics. \n  Input the observed mean, standard deviation, and sample size \n Input  hypothesized population mean (default is zero)  \n Optional: Input the form of alternative (\"less\", \"greater\", or \"two.sided\") \n Optional: Input confidence level(s) for a two-sided confidence interval.\n   "

  if (as.character(xbar) == "?") {
    stop(Description)
  }

  cat("\n", "One Sample t test\n", sep = "", "\n")
  statistic <- xbar
  df <- n - 1
  se <- sd / sqrt(n)
  tvalue <- NULL
  pvalue <- NULL
  cat(paste(
    "mean = ",
    xbar,
    ", sd = ",
    sd,
    ",  sample size = ",
    n,
    "\n",
    sep = ""
  ))
  if (!is.null(alternative)) {
    cat(paste("Null hypothesis       : mu =", hypothesized, sep = " "), "\n")
    altname <- switch(
      alternative,
      less = "<",
      greater = ">",
      two.sided = "<>",
      not.equal = "<>"
    )
    cat(
      paste("Alternative hypothesis: mu", altname, hypothesized, sep = " "),
      "\n"
    )

    tvalue <- (statistic - hypothesized) / se
    cat("t-statistic:", signif(tvalue, 4), "\n")
    min <- min(-4, tvalue - .001)
    diffmin <- min(
      hypothesized - 4 * se,
      hypothesized - abs(hypothesized - statistic) - .01
    )
    max <- max(4, tvalue + .001)
    diffmax <- max(
      hypothesized + 4 * se,
      hypothesized + abs(hypothesized - statistic) + .01
    )
    x <- seq(min, max, .001)
    diffx <- x * se + hypothesized
    withr::local_par(mar = c(4, 3, 2, 2))
    plot(
      diffx,
      dt(x, df),
      xlab = "\u2190 Sample Means \u2192",
      ylab = " ",
      type = "l",
      ylim = c(0, dt(0, df)),
      panel.first = grid()
    )
    tseq <- c(
      hypothesized - 3 * se,
      hypothesized - 2 * se,
      hypothesized - se,
      hypothesized,
      hypothesized + se,
      hypothesized + 2 * se,
      hypothesized + 3 * se
    )
    mtext(side = 2, line = 2, "density")

    axis(
      side = 1,
      at = tseq,
      labels = c("t=-3", "t=-2", "t=-1", "t=0", "t=1", "t=2", "t=3"),
      padj = 1.2,
      tick = FALSE,
      col.axis = "blue"
    )
    abline(h = 0, col = "black")
    title(paste("t (df=", df, ")"))
    if (alternative == "less") {
      pvalue <- pt(tvalue, df)
      drawseq <- seq(diffmin, statistic, .001)
      polygon(
        c(drawseq, statistic, diffmin),
        c(dt((drawseq - hypothesized) / se, df), 0, 0),
        col = "red"
      )
      text(
        diffmin,
        dt(0, df) * .9,
        labels = paste("t-statistic:", signif(tvalue, 3)),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * .8,
        labels = paste("p-value:", signif(pvalue, 4)),
        pos = 4,
        col = "red"
      )
    } else if (alternative == "greater") {
      pvalue <- 1 - pt(tvalue, df)
      drawseq <- seq(statistic, diffmax, .001)
      polygon(
        c(statistic, drawseq, diffmax),
        c(0, dt((drawseq - hypothesized) / se, df), 0),
        col = "red"
      )
      text(
        diffmax,
        dt(0, df) * .9,
        labels = paste("t-statistic:", signif(tvalue, 3)),
        pos = 2,
        col = "blue"
      )
      text(
        diffmax,
        dt(0, df) * .8,
        labels = paste("p-value:", signif(pvalue, 4)),
        pos = 2,
        col = "red"
      )
    } else if (alternative == "two.sided" || alternative == "not.equal") {
      pvalue <- 2 * pt(-1 * abs(tvalue), df)
      drawseq1 <- seq(
        diffmin,
        hypothesized - abs(hypothesized - statistic),
        .001
      )
      drawseq2 <- seq(
        hypothesized + abs(hypothesized - statistic),
        diffmax,
        .001
      )
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
        dt(0, df) * .9,
        labels = paste("t-statistic:", signif(tvalue, 4)),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * .8,
        labels = paste("two-sided p-value:", signif(pvalue, 4)),
        pos = 4,
        col = "red"
      )
    }
  } # end test

  lower <- NULL
  upper <- NULL
  if (!is.null(conf.level)) {
    withr::local_par(mar = c(4, .5, 1.5, .5), mfrow = c(3, 1))
    if (length(conf.level) > 1) {
      withr::local_par(mar = c(4, 2, 1.5, .4), mfrow = c(length(conf.level), 1))
    }
    for (k in 1:length(conf.level)) {
      if (conf.level[k] > 1) {
        conf.level[k] <- conf.level[k] / 100
      }
      criticalvalue <- qt((1 - conf.level[k]) / 2, df)
      lower[k] <- statistic + criticalvalue * se
      upper[k] <- statistic - criticalvalue * se
      multconflevel <- 100 * conf.level[k]
      cat(
        multconflevel,
        "% Confidence interval for mu: (",
        lower[k],
        ", ",
        upper[k],
        ") \n"
      )
    }
    if (is.null(alternative)) {
      min <- statistic - 4 * se
      max <- statistic + 4 * se
      CIseq <- seq(min, max, .001)
      if (length(conf.level) == 1) {
        withr::local_par(mar = c(4, .5, 1.5, .5), mfrow = c(3, 1))
        myxlab <- substitute(paste(mean == x1), list(x1 = signif(lower[1], 4)))
        plot(CIseq, dnorm(CIseq, lower[1], se), type = "l", xlab = " ")
        mtext("sample means", side = 1, line = 1.75, adj = .5, cex = .75)
        topseq <- seq(statistic, max, .001)
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
        plot(
          seq(min, max, .001),
          dnorm(seq(min, max, .001), upper[1], se),
          type = "l",
          xlab = " "
        )
        mtext("sample means", side = 1, line = 1.75, adj = .5, cex = .75)
        bottomseq <- seq(min, statistic, .001)
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
      for (k in 1:length(conf.level)) {
        plot(
          c(min, statistic, max),
          c(1, 1, 1),
          pch = c(".", "^", "."),
          ylab = " ",
          xlab = "population mean",
          ylim = c(1, 1)
        )
        abline(v = statistic, col = "gray")
        text(min * 1.01, 1, labels = paste(100 * conf.level[k], "% CI:"))
        text(statistic, .9, labels = signif(statistic, 4))
        text(lower[k], 1, labels = signif(lower[k], 4), pos = 3)
        text(upper[k], 1, labels = signif(upper[k], 4), pos = 3)
        points(c(lower[k], upper[k]), c(1, 1), pch = c("[", "]"))
        lines(c(lower[k], upper[k]), c(1, 1))
      }
    }
  }
  if (!is.null(alternative)) {
    cat("p-value:", pvalue, "\n")
  }
  withr::local_par(mfrow = c(1, 1))
  invisible(list(
    "tvalue" = tvalue,
    "pvalue" = pvalue,
    "lower" = lower,
    "upper" = upper
  ))

  withr::local_par(mfrow = c(1, 1))
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
#'
#' @return The t value, p value, and confidence interval.
#' @export
#'
#' @examples
twosamplet <- function(
  x1,
  sd1,
  n1,
  x2,
  sd2,
  n2,
  hypothesized = 0,
  alternative = NULL,
  conf.level = 0
) {
  withr::local_par(mar = c(4, 3, 2, 2))

  cat("\n", "Two Sample t test\n", sep = "", "\n")
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

  cat(paste(
    "Group1: mean = ",
    x1,
    ", sd = ",
    sd1,
    ",  sample size = ",
    n1,
    "\n",
    sep = ""
  ))
  cat(paste(
    "Group2: mean = ",
    x2,
    ", sd = ",
    sd2,
    ",  sample size = ",
    n2,
    "\n",
    sep = ""
  ))
  cat(paste("diff:", x1 - x2, "\n\n", sep = ""))

  if (!is.null(alternative)) {
    cat(
      paste("Null hypothesis       : mu1-mu2 =", hypothesized, sep = " "),
      "\n"
    )
    altname <- switch(
      alternative,
      less = "<",
      greater = ">",
      two.sided = "<>",
      not.equal = "<>"
    )
    cat(
      paste(
        "Alternative hypothesis: mu1-mu2",
        altname,
        hypothesized,
        sep = " "
      ),
      "\n"
    )

    tvalue <- (statistic1 - statistic2 - hypothesized) / unpooledsd
    cat("t-statistic:", signif(tvalue, 4), "\n")
    cat("df:", signif(df, 4), "\n")
    min <- min(-4, tvalue - .001)
    diffmin <- min(
      hypothesized - 4 * unpooledsd,
      min(
        hypothesized - 4 * unpooledsd,
        hypothesized - abs(hypothesized - statistic) - .001
      )
    )
    max <- max(4, tvalue + .001)
    diffmax <- max(
      hypothesized + 4 * unpooledsd,
      hypothesized + abs(hypothesized - statistic) + .001
    )

    x <- seq(min, max, .001)
    diffx <- x * unpooledsd + hypothesized
    plot(
      diffx,
      dt(x, df),
      xlab = "\u2190 Difference in Sample Means \u2192",
      ylab = "",
      type = "l",
      ylim = c(0, dt(0, df)),
      panel.first = grid()
    )
    tseq <- c(
      hypothesized - 3 * unpooledsd,
      hypothesized - 2 * unpooledsd,
      hypothesized - unpooledsd,
      hypothesized,
      hypothesized + unpooledsd,
      hypothesized + 2 * unpooledsd,
      hypothesized + 3 * unpooledsd
    )

    axis(
      side = 1,
      at = tseq,
      labels = c("t=-3", "t=-2", "t=-1", "t=0", "t=1", "t=2", "t=3"),
      padj = 1.2,
      tick = FALSE,
      col.axis = "blue"
    )
    abline(h = 0, col = "black")
    mtext(side = 2, line = 2, "density")

    title(paste("t (df=", df, ")"))
    if (alternative == "less") {
      pvalue <- signif(pt(tvalue, df), 4)
      tvalue <- signif(tvalue, 4)
      drawseq <- seq(diffmin, statistic, .001)
      polygon(
        c(drawseq, statistic, diffmin),
        c(dt((drawseq - hypothesized) / unpooledsd, df), 0, 0),
        col = "red"
      )
      text(
        diffmin,
        dt(0, df) * .9,
        labels = paste("t-statistic:", tvalue),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * .8,
        labels = paste("p-value:", pvalue),
        pos = 4,
        col = "red"
      )
    } else if (alternative == "greater") {
      pvalue <- signif(1 - pt(tvalue, df), 4)
      tvalue <- signif(tvalue, 3)
      drawseq <- seq(statistic, diffmax, .001)
      polygon(
        c(statistic, drawseq, diffmax),
        c(0, dt((drawseq - hypothesized) / unpooledsd, df), 0),
        col = "red"
      )
      text(
        diffmax,
        dt(0, df) * .9,
        labels = paste("t-statistic:", tvalue),
        pos = 2,
        col = "blue"
      )
      text(
        diffmax,
        dt(0, df) * .8,
        labels = paste("p-value:", pvalue),
        pos = 2,
        col = "red"
      )
    } else if (alternative == "two.sided" || alternative == "not.equal") {
      pvalue <- signif(2 * pt(-1 * abs(tvalue), df), 4)
      tvalue <- signif(tvalue, 4)
      drawseq1 <- seq(
        diffmin,
        hypothesized - abs(hypothesized - statistic),
        .001
      )
      drawseq2 <- seq(
        hypothesized + abs(hypothesized - statistic),
        diffmax,
        .001
      )
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
        dt(0, df) * .9,
        labels = paste("t-statistic:", tvalue),
        pos = 4,
        col = "blue"
      )
      text(
        diffmin,
        dt(0, df) * .8,
        labels = paste("two-sided p-value:", pvalue),
        pos = 4,
        col = "red"
      )
    }
  }
  lower <- NULL
  upper <- NULL

  if (conf.level != 0) {
    if (conf.level > 1) {
      conf.level <- conf.level / 100
    }
    criticalvalue <- qt((1 - conf.level) / 2, df)
    lower <- statistic + criticalvalue * unpooledsd
    upper <- statistic - criticalvalue * unpooledsd
    multconflevel <- 100 * conf.level
    cat(
      multconflevel,
      "% Confidence interval for mu1-mu2: (",
      lower,
      ", ",
      upper,
      ") \n"
    )
    if (is.null(alternative)) {
      min <- statistic - 4 * unpooledsd
      max <- statistic + 4 * unpooledsd
      CIseq <- seq(min, max, .001)
      withr::local_par(mar = c(4, .5, 1.5, .5), mfrow = c(3, 1))
      myxlab <- substitute(paste(mean == x1), list(x1 = signif(lower, 4)))
      plot(CIseq, dnorm(CIseq, lower, unpooledsd), type = "l", xlab = " ")
      mtext(
        "difference in sample means",
        side = 1,
        line = 1.75,
        adj = .5,
        cex = .75
      )
      topseq <- seq(statistic, max, .001)
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
      plot(
        seq(min, max, .001),
        dnorm(seq(min, max, .001), upper, unpooledsd),
        type = "l",
        xlab = " "
      )
      mtext(
        "difference in sample means",
        side = 1,
        line = 1.75,
        adj = .5,
        cex = .75
      )
      bottomseq <- seq(min, statistic, .001)
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

      plot(
        c(min, statistic, max),
        c(1, 1, 1),
        pch = c(".", "^", "."),
        ylab = " ",
        xlab = "difference in process means",
        ylim = c(1, 1)
      )
      abline(v = statistic, col = "gray")
      text(min * 1.01, 1, labels = paste(multconflevel, "% CI:"))
      text(statistic, .9, labels = signif(statistic, 4))
      text(lower, 1, labels = signif(lower, 4), pos = 3)
      text(upper, 1, labels = signif(upper, 4), pos = 3)
      points(c(lower, upper), c(1, 1), pch = c("[", "]"))
      lines(c(lower, upper), c(1, 1))
    }
  }
  if (!is.null(alternative)) {
    cat("p-value:", pvalue, "\n")
    invisible(list(
      "tvalue" = tvalue,
      "df" = df,
      "pvalue" = pvalue,
      "lower" = lower,
      "upper" = upper
    ))
  }
  withr::local_par(mfrow = c(1, 1))
}


#' Tail Probability for t-distribution
#'
#' @param xval observed value.
#' @param df degrees of freedom.
#' @param direction direction for probability calculation, "above" or "below"; if
#'  "outside" or "between" are used, a second larger observation, `xval2` must be
#'  specified
#' @param xval2 second observation value.
#'
#' @return The tail probability in the specified direction using the given
#' parameters.
#' @export
#'
#' @examples
tprob <- function(xval, df, direction, xval2 = NULL) {
  withr::local_par(mar = c(4, 4, 2, 1))

  minx <- min(-5, -1 * abs(xval) - .5)
  maxx <- max(5, abs(xval) + .5)
  thisx <- seq(minx, maxx, .001)
  plot(
    thisx,
    dt(thisx, df),
    xlim = c(minx, maxx),
    type = "l",
    xlab = "",
    ylab = "",
    panel.first = grid()
  )
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 2, "t-values")
  mtext(side = 2, line = 2, "density")

  if (direction == "below") {
    probseq <- seq(minx, max(minx, xval), .001)
    tprob <- pt(xval, df)
    showprob <- format(tprob, digits = 4)
    polygon(
      c(probseq, max(minx, xval), minx),
      c(dt(probseq, df), 0, 0),
      col = "red",
      border = "red"
    )
    text(
      minx,
      dt(0, df) / 2,
      labels = paste("P(X \u2264", xval, ") \n =", showprob),
      col = "red",
      pos = 4
    )
  } else if (direction == "above") {
    probseq <- seq(min(xval, maxx), maxx, .001)
    tprob <- pt(xval, df, lower.tail = FALSE)
    showprob <- format(tprob, digits = 4)
    polygon(
      c(min(maxx, xval), probseq, maxx),
      c(0, dt(probseq, df), 0),
      col = "red",
      border = "red"
    )
    print(xval)
    print(maxx)
    text(
      maxx,
      dt(0, df) / 2,
      labels = paste("P(X \u2265", xval, ") \n =", showprob),
      col = "red",
      pos = 2
    )
  } else if (direction == "between") {
    if (is.null(xval2)) {
      stop("You need to specify a second observation value.")
    }
    if (xval2 < xval) {
      temp <- xval
      xval <- xval2
      xval2 <- temp
    }
    probseq <- seq(xval, xval2, .001)
    tprob <- pt(xval2, df) - pt(xval, df)
    showprob <- format(tprob, digits = 4)
    polygon(
      c(xval, probseq, xval2),
      c(0, dt(probseq, df), 0),
      col = "red",
      border = "red"
    )
    text(
      minx,
      dt(0, df) / 2,
      labels = paste("P(", xval, "\u2264 X\u2264", xval2, ") \n =", showprob),
      col = "red",
      pos = 4
    )
  } else if (direction == "outside") {
    maxx <- max(maxx, xval2)
    if (is.null(xval2)) {
      stop("You need to specify a second observation value.")
    }
    if (xval2 < xval) {
      temp <- xval
      xval <- xval2
      xval2 <- temp
    }
    probseq1 <- seq(minx, xval, .001)
    probseq2 <- seq(xval2, maxx, .001)
    tprob <- 1 - (pt(xval2, df) - pt(xval, df))
    showprob <- format(tprob, digits = 4)
    polygon(
      c(minx, probseq1, xval),
      c(0, dt(probseq1, df), 0),
      col = "red",
      border = "red"
    )
    polygon(
      c(xval2, probseq2, maxx),
      c(0, dt(probseq2, df), 0),
      col = "red",
      border = "red"
    )
    text(
      -2,
      dt(0, df) / 2,
      labels = paste(
        "P(X \u2264",
        xval,
        ") and \n P(X \u2265",
        xval2,
        ") \n =",
        showprob
      ),
      col = "red",
      pos = 2
    )
  } else {
    stop(
      "Use \"above\", \"below\", \"between\", or \"outside\" as the direction."
    )
  }

  title(substitute(paste("t(", df == x3, ")"), list(x3 = df)))
  cat(c("probability:", showprob), "\n")
}
