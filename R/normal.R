#' Normal Tail Probability
#'
#' `normprob` finds a p-value and plots it onto a normal distribution with mean
#'  and standard deviation as specified. The function can find the probability
#'  above, below, between, or outside of the observed value, as specified by
#'  `directions`.
#'
#' @param xval observed value.
#' @param mean mean of normal distribution.
#' @param sd standard deviation of normal distribution.
#' @param direction direction for probability calculation, "above" or "below"; if
#'  "outside" or "between" are used, a second larger observation, `xval2` must be
#'  specified
#' @param label horizontal axis label.
#' @param xval2 second observation value.
#' @param digits number of digits to display.
#'
#' @return a p-value and a plot of the normal distribution with shaded area
#'  representing probability of the observed value or more extreme occurring.
#' @export
#'
#' @examples
#' iscamnormprob(1.96, direction = "above")
#' iscamnormprob(-1.5, mean = 1, sd = 2, direction = "below")
#' iscamnormprob(0, xval2 = 1.5, direction = "between")
#' iscamnormprob(-1, xval2 = 1, direction = "outside")
iscamnormprob <- function(
  xval,
  mean = 0,
  sd = 1,
  direction,
  label = NULL,
  xval2 = NULL,
  digits = 4
) {
  withr::local_par(mar = c(4, 3, 2, 1))

  if (is.null(xval2)) {
    xval2 <- abs(xval)
  }
  if (xval2 < xval) {
    temp <- xval
    xval <- xval2
    xval2 <- temp
  }
  xvallabel <- format(xval, digits = digits)
  xval2label <- format(xval2, digits = digits)
  withr::local_options("scipen" = 100, "digits" = digits)

  minx <- min(mean - 4 * sd, xval - .15 * abs(xval))
  maxx <- max(mean + 4 * sd, xval2 + .15 * xval2)

  thisx <- seq(minx, maxx, .001)
  xlabel <- "x-variable"
  if (!is.null(label)) {
    xlabel <- label
  }

  plot(
    thisx,
    dnorm(thisx, mean, sd),
    xlim = c(minx, maxx),
    type = "l",
    xlab = "",
    ylab = " ",
    lwd = 1,
    panel.first = grid()
  )
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 2, xlabel)
  mtext(side = 2, line = 2, "Density")

  if (direction == "below") {
    probseq <- seq(minx, max(minx, xval), .001)
    normprob <- pnorm(xval, mean, sd)
    showprob <- format(normprob, digits = digits)
    polygon(
      c(probseq, max(minx, xval), minx),
      c(dnorm(probseq, mean, sd), 0, 0),
      col = "red",
      border = "red"
    )
    text(
      minx,
      max(dnorm(mean, mean, sd)) * .9,
      labels = paste("P(X \u2264", xvallabel, ") \n =", showprob),
      col = "red",
      pos = 4
    )
  } else if (direction == "above") {
    probseq <- seq(min(xval, maxx), maxx, .001)
    normprob <- pnorm(xval, mean, sd, lower.tail = FALSE)
    showprob <- format(normprob, digits = digits)
    polygon(
      c(min(maxx, xval), probseq, maxx),
      c(0, dnorm(probseq, mean, sd), 0),
      col = "red",
      border = "red"
    )
    text(
      maxx,
      max(dnorm(mean, mean, sd)) * .9,
      labels = paste("P(X \u2265", xvallabel, ") \n =", showprob),
      col = "red",
      pos = 2
    )
  } else if (direction == "between") {
    if (is.null(xval2)) {
      stop("You need to specify a second observation value.")
    }
    probseq <- seq(xval, xval2, .001)
    normprob <- pnorm(xval2, mean, sd) - pnorm(xval, mean, sd)
    showprob <- format(normprob, digits = digits)
    polygon(
      c(xval, probseq, xval2),
      c(0, dnorm(probseq, mean, sd), 0),
      col = "red",
      border = "red"
    )
    text(
      minx,
      max(dnorm(mean, mean, sd)) * .9,
      labels = paste(
        "P(",
        xvallabel,
        "\u2264 X\u2264",
        xval2label,
        ") \n =",
        showprob
      ),
      col = "red",
      pos = 4
    )
  } else if (direction == "outside") {
    if (is.null(xval2)) {
      stop("You need to specify a second observation value.")
    }
    probseq1 <- seq(minx, xval, .001)
    probseq2 <- seq(xval2, maxx, .001)
    normprob <- 1 - (pnorm(xval2, mean, sd) - pnorm(xval, mean, sd))
    showprob <- format(normprob, digits = digits)
    polygon(
      c(minx, probseq1, xval),
      c(0, dnorm(probseq1, mean, sd), 0),
      col = "red",
      border = "red"
    )
    polygon(
      c(xval2, probseq2, maxx),
      c(0, dnorm(probseq2, mean, sd), 0),
      col = "red",
      border = "red"
    )
    text(
      minx,
      max(dnorm(mean, mean, sd)) * .9,
      labels = paste(
        "P(X \u2264",
        xvallabel,
        ") and \n P(X \u2265",
        xval2label,
        ")  =",
        showprob
      ),
      col = "red",
      pos = 4
    )
  } else {
    stop(
      "Use \"above\", \"below\", \"between\", or \"outside\" as the direction."
    )
  }

  title(substitute(
    paste("Normal(", mean == x3, ",  ", SD == x4, ")"),
    list(x3 = mean, x4 = signif(sd, 4))
  ))
  cat(c("probability:", showprob), "\n")

  showprob
}

#' Inverse Normal Calculation
#'
#' @param prob1 probability to find normal quantile of.
#' @param mean mean of normal distribution.
#' @param sd standard deviation of normal distribution.
#' @param direction direction for probability calculation: "above", "below",
#'  "outside", "between".
#'
#' @return a plot of the normal distribution with the quantile of the specified
#' probability highlighted.
#'
#' @export
#'
#' @examples
#' iscaminvnorm(0.05, direction = "below")
#' iscaminvnorm(0.90, mean = 100, sd = 15, direction = "above")
#' iscaminvnorm(0.10, direction = "outside")
#' iscaminvnorm(0.95, direction = "between")
iscaminvnorm <- function(prob1, mean = 0, sd = 1, direction) {
  withr::local_par(mar = c(4, 3, 2, 2))
  min <- mean - 4 * sd
  max <- mean + 4 * sd
  thisx <- seq(min, max, .001)
  varx <- "X"
  if (mean == 0 & sd == 1) {
    varx <- "Z"
  }
  plot(
    thisx,
    dnorm(thisx, mean, sd),
    xlab = "",
    ylab = "density",
    type = "l",
    panel.first = grid()
  )
  newtitle <- paste("Normal (mean =", mean, ", SD = ", sd, ")")
  title(newtitle)
  mtext(side = 1, line = 2, paste(varx, " = variable"))
  mtext(side = 2, line = 2, "density")
  ymax <- max(dnorm(mean, mean, sd))
  abline(h = 0, col = "black")
  if (direction == "below") {
    answer <- signif(qnorm(prob1, mean, sd, TRUE), 4)
    thisrange <- seq(min, answer, .001)
    polygon(
      c(thisrange, answer, answer),
      c(0, dnorm(thisrange, mean, sd), 0),
      col = "pink"
    )
    text(
      (min + answer) / 2,
      dnorm(answer, mean, sd) / 2,
      labels = prob1,
      col = "blue"
    )
    text(
      answer,
      min(dnorm(answer, mean, sd), ymax * .85),
      labels = paste(varx, " \u2264 ", answer),
      col = "red",
      pos = 3
    )
    cat("The observation with", prob1, "probability below is", answer, "\n")
  } else if (direction == "above") {
    answer <- signif(qnorm(prob1, mean, sd, FALSE), 4)
    thisrange <- seq(answer, max, .001)
    polygon(
      c(answer, thisrange, max),
      c(0, dnorm(thisrange, mean, sd), 0),
      col = "pink"
    )
    text(
      (answer + max) / 2,
      (dnorm(answer, mean, sd) / 2),
      labels = prob1,
      col = "blue"
    )
    text(
      answer,
      min(dnorm(answer, mean, sd), ymax * .85),
      labels = paste(varx, " \u2265 ", answer),
      col = "red",
      pos = 3
    )
    cat("The observation with", prob1, "probability above is", answer, "\n")
  } else if (direction == "between") {
    answer1 <- signif(qnorm((1 - prob1) / 2, mean, sd, TRUE), 4)
    answer2 <- mean + (mean - answer1)
    thisrange <- seq(answer1, answer2, .001)
    polygon(
      c(answer1, thisrange, answer2),
      c(0, dnorm(thisrange, mean, sd), 0),
      col = "pink"
    )
    text(mean, dnorm(mean, mean, sd) / 2, labels = prob1, col = "blue")
    text(
      answer1,
      min(dnorm(answer1, mean, sd), ymax * .85),
      labels = paste(varx, " \u2265", answer1),
      col = "red",
      pos = 3
    )
    text(
      answer2,
      min(dnorm(answer1, mean, sd), ymax * .85),
      labels = paste(varx, "\u2264", answer2),
      col = "red",
      pos = 3
    )
    cat("There is", prob1, "probability between", answer1, "and", answer2, "\n")
  } else if (direction == "outside") {
    answer1 <- signif(qnorm(prob1 / 2, mean, sd, TRUE), 4)
    answer2 <- mean + (mean - answer1)
    thisrange1 <- seq(min, answer1, .001)
    thisrange2 <- seq(answer2, max, .001)
    polygon(
      c(min, thisrange1, answer1),
      c(0, dnorm(thisrange1, mean, sd), 0),
      col = "pink"
    )
    polygon(
      c(answer2, thisrange2, max),
      c(0, dnorm(thisrange2, mean, sd), 0),
      col = "pink"
    )
    text(
      answer1,
      dnorm(answer2, mean, sd) / 2,
      labels = prob1 / 2,
      col = "blue",
      pos = 2
    )
    text(
      answer2,
      dnorm(answer2, mean, sd) / 2,
      labels = prob1 / 2,
      col = "blue",
      pos = 4
    )
    text(
      answer1,
      min(dnorm(answer2, mean, sd), ymax * .85),
      labels = paste(varx, " \u2264", answer1),
      col = "red",
      pos = 3
    )
    text(
      answer2,
      min(dnorm(answer2, mean, sd), ymax * .85),
      labels = paste(varx, " \u2265", answer2),
      col = "red",
      pos = 3
    )
    cat("There is", prob1, "probability outside", answer1, "and", answer2, "\n")
  }
}

#' Rejection Region for Normal
#'
#' `normpower` determines the rejection region corresponding to the level of
#'  significance and the first probability and shows the normal distribution
#'  shading its corresponding region.
#'
#' @param LOS A numeric value representing the level of significance; 0 < `LOS`< 1
#' @param n A numeric value representing the sample size
#' @param prob1 A numeric value representing the first probability
#' @param alternative "less", "greater", or "two.sided"
#' @param prob2 A numeric value representing the second probability
#'
#' @return A plot of the normal distribution with the rejection region highlighted.
#'
#' @export
#'
#' @examples
#' iscamnormpower(0.05, n = 100, prob1 = 0.5, alternative = "greater", prob2 = 0.6)
#' iscamnormpower(0.10, n = 50, prob1 = 0.25, alternative = "less", prob2 = 0.15)
#' iscamnormpower(0.05, n = 200, prob1 = 0.8, alternative = "two.sided", prob2 = 0.7)
iscamnormpower <- function(LOS, n, prob1, alternative, prob2) {
  withr::local_par(mar = c(5, 4, 1, 1), mfrow = c(2, 1))

  minx <- max(
    0,
    min(
      prob1 - 4 * sqrt(prob1 * (1 - prob1) / n),
      prob2 - 4 * sqrt(prob2 * (1 - prob2) / n)
    )
  )
  maxx <- min(
    n,
    max(
      prob1 + 4 * sqrt(prob1 * (1 - prob1) / n),
      prob2 + 4 * sqrt(prob2 * (1 - prob2) / n)
    )
  )
  mean <- prob1
  std <- sqrt(prob1 * (1 - prob1) / n)
  myy1 <- dnorm(mean, mean, std) / 2
  drawseq <- seq(minx, maxx, .001)
  plot(
    drawseq,
    dnorm(drawseq, mean, std),
    type = "l",
    xlab = "Probability of Success",
    ylab = "Density",
    panel.first = grid()
  )

  if (alternative == "less") {
    rr <- qnorm(LOS, mean, std)
    this.prob1 <- pnorm(rr, mean, std)
    showprob1 <- format(this.prob1, digits = 4)
    drawseq <- seq(minx, rr, .001)
    polygon(
      c(drawseq, rr, minx),
      c(dnorm(drawseq, mean, std), 0, 0),
      col = "red"
    )
    rr <- format(rr, digits = 4)
    text(
      minx,
      myy1,
      labels = paste("P(p-hat \u2264", rr, ")\n =", showprob1),
      pos = 4,
      col = "red"
    )
    cat("Null: Probability", rr, "and below =", this.prob1, "\n")
  } else if (alternative == "greater") {
    rr <- qnorm(LOS, mean, std, FALSE)
    this.prob1 <- 1 - pnorm(rr, mean, std)

    showprob1 <- format(this.prob1, digits = 4)
    drawseq <- seq(rr, maxx, .001)
    polygon(
      c(drawseq, maxx, rr),
      c(dnorm(drawseq, mean, std), 0, 0),
      col = "red"
    )
    rr <- format(rr, digits = 4)
    text(
      maxx,
      myy1,
      labels = paste("P(p-hat \u2265", rr, ")\n =", showprob1),
      pos = 2,
      col = "red"
    )
    cat("Null: Probability", rr, "and above =", this.prob1, "\n")
  } else if (alternative == "two.sided") {
    lowerrr <- qnorm(LOS / 2, mean, std)
    upperrr <- qnorm(LOS / 2, mean, std, FALSE)
    lowerprob1 <- pnorm(lowerrr, mean, std)
    upperprob1 <- pnorm(upperrr, mean, std, FALSE)
    showlowerprob1 <- format(lowerprob1, digits = 4)
    showupperprob1 <- format(upperprob1, digits = 4)
    showprob1 <- format(lowerprob1 + upperprob1, digits = 4)
    drawseq <- seq(minx, lowerrr, .001)
    polygon(
      c(drawseq, lowerrr, minx),
      c(dnorm(drawseq, mean, std), 0, 0),
      col = "red"
    )
    drawseq <- seq(upperrr, maxx, .001)
    polygon(
      c(drawseq, maxx, upperrr),
      c(dnorm(drawseq, mean, std), 0, 0),
      col = "red"
    )
    showupperrr <- format(upperrr, digits = 4)
    showlowerrr <- format(lowerrr, digits = 4)
    text(
      (maxx + prob1) / 2,
      myy1,
      labels = paste(
        "P(p-hat \u2264",
        showlowerrr,
        ")+P(p-hat \u2265",
        showupperrr,
        ")\n =",
        showlowerprob1,
        "+",
        showupperprob1,
        "\n =",
        showprob1
      ),
      pos = 3,
      col = "red"
    )
    cat("Null: Probability in rejection region", showprob1, "\n")
  } else {
    stop("Check input for alternative")
  }

  newtitle <- substitute(
    paste("Null: Normal (", mu == x1, ", ", sigma == x2, ")", ),
    list(x1 = mean, x2 = std)
  )
  title(newtitle)

  if (!is.null(prob2)) {
    mean2 <- prob2
    std2 <- sqrt(prob2 * (1 - prob2) / n)
    drawseq <- seq(minx, maxx, .001)
    plot(
      drawseq,
      dnorm(drawseq, mean2, std2),
      type = "l",
      xlab = "Probability of Success",
      ylab = "Density",
      panel.first = grid()
    )

    if (alternative == "less") {
      rr <- qnorm(LOS, mean, std)
      this.prob2 <- pnorm(rr, mean2, std2)
      showprob2 <- format(this.prob2, digits = 4)
      drawseq <- seq(minx, rr, .001)
      polygon(
        c(drawseq, rr, minx),
        c(dnorm(drawseq, mean2, std2), 0, 0),
        col = "blue"
      )
      rr <- format(rr, digits = 4)
      text(
        minx,
        myy1,
        labels = paste("P(p-hat\u2264", rr, ")\n =", showprob2),
        pos = 4,
        col = "blue"
      )
      cat("Alt: Probability", rr, "and below =", this.prob2, "\n")
    } else if (alternative == "greater") {
      rr <- qnorm(LOS, mean, std, FALSE)
      this.prob2 <- 1 - pnorm(rr, mean2, std2)
      showprob2 <- format(this.prob2, digits = 4)
      drawseq <- seq(rr, maxx, .001)
      polygon(
        c(drawseq, maxx, rr),
        c(dnorm(drawseq, mean2, std2), 0, 0),
        col = "blue"
      )
      rr <- format(rr, digits = 4)
      text(
        maxx,
        myy1,
        labels = paste("P(p-hat\u2265", rr, ")\n =", showprob2),
        pos = 2,
        col = "blue"
      )
      cat("Alt: Probability", rr, "and above =", this.prob2, "\n")
    } else if (alternative == "two.sided") {
      lowerrr <- qnorm(LOS / 2, mean, std)
      upperrr <- qnorm(LOS / 2, mean, std, FALSE)
      lowerprob2 <- pnorm(lowerrr, mean2, std2)
      upperprob2 <- pnorm(upperrr, mean2, std2, FALSE)
      showlowerprob2 <- format(lowerprob2, digits = 4)
      showupperprob2 <- format(upperprob2, digits = 4)
      showprob2 <- format(lowerprob2 + upperprob2, digits = 4)
      drawseq <- seq(minx, lowerrr, .001)
      polygon(
        c(drawseq, lowerrr, minx),
        c(dnorm(drawseq, mean2, std2), 0, 0),
        col = "blue"
      )
      drawseq <- seq(upperrr, maxx, .001)
      polygon(
        c(drawseq, maxx, upperrr),
        c(dnorm(drawseq, mean2, std2), 0, 0),
        col = "blue"
      )
      showupperrr <- format(upperrr, digits = 4)
      showlowerrr <- format(lowerrr, digits = 4)
      text2 <- (maxx + prob2) / 2
      if (prob1 < prob2) {
        text2 <- (minx + prob1) / 2
      }
      text(
        text2,
        myy1,
        labels = paste(
          "P(p-hat \u2264",
          showlowerrr,
          ")+P(p-hat \u2265",
          showupperrr,
          ")\n =",
          showlowerprob2,
          "+",
          showupperprob2,
          "\n =",
          showprob2
        ),
        pos = 3,
        col = "blue"
      )
      cat("Alt: Probability in rejection region", showprob2, "\n")
    }

    newtitle <- substitute(
      paste("Alt: Normal (", mu == x1, ", ", sigma == x2, ")", ),
      list(x1 = mean2, x2 = std2)
    )
    title(newtitle)
  }

  withr::local_par(mfrow = c(1, 1))
}

#' One Proportion Z-Test and Interval
#'
#' iscamonepropztest calculates a one-proportion z-test and/or a corresponding
#' confidence interval.
#'
#' @param observed The observed number of successes. If a value less than 1 is
#' provided, it is assumed to be the sample proportion.
#' @param n The sample size.
#' @param hypothesized The hypothesized probability of success under the null
#' hypothesis. This is an optional parameter.
#' @param alternative A character string specifying the form of the alternative
#' hypothesis. Must be one of "less", "greater", or "two.sided". This is
#' an optional parameter.
#' @param conf.level The confidence level(s) for a two-sided confidence
#' interval. This is an optional parameter.
#'
#' @return This function prints the results of the one-proportion z-test and/or
#' the confidence interval. It also generates plots to visualize the test and
#' interval.
#' @export
#'
#' @examples
#' iscamonepropztest(observed = 35, n = 50, hypothesized = 0.5)
#'
#' iscamonepropztest(
#'   observed = 0.8,
#'   n = 100,
#'   hypothesized = 0.75,
#'   alternative = "greater",
#'   conf.level = 0.95
#' )
#'
#' iscamonepropztest(observed = 60, n = 100, conf.level = 0.90)
iscamonepropztest <- function(
  observed,
  n,
  hypothesized = NULL,
  alternative = "two.sided",
  conf.level = NULL
) {
  withr::local_par(mar = c(5, 3, 1, 1))

  if (observed < 1) {
    observed = round(n * observed)
  }
  myout = prop.test(observed, n, hypothesized, alternative, correct = FALSE)
  cat("\n", "One Proportion z test\n", sep = "", "\n")
  statistic = signif(observed / n, 4)
  cat(paste(
    "Data: observed successes = ",
    observed,
    ", sample size = ",
    n,
    ", sample proportion = ",
    statistic,
    "\n\n",
    sep = ""
  ))
  zvalue = NULL
  pvalue = NULL
  if (!is.null(hypothesized)) {
    cat(paste("Null hypothesis       : pi =", hypothesized, sep = " "), "\n")
    altname = switch(
      alternative,
      less = "<",
      greater = ">",
      two.sided = "<>",
      not.equal = "<>"
    )
    cat(
      paste("Alternative hypothesis: pi", altname, hypothesized, sep = " "),
      "\n"
    )
    zvalue = (statistic - hypothesized) /
      sqrt(hypothesized * (1 - hypothesized) / n)
    cat("z-statistic:", signif(zvalue, 4), "\n")
    pvalue = signif(myout$p.value, 4)
    cat("p-value:", pvalue, "\n")
    SD = sqrt(hypothesized * (1 - hypothesized) / n)
    min = min(hypothesized - 4 * SD, hypothesized - abs(zvalue) * SD - .001)
    max = max(hypothesized + 4 * SD, hypothesized + abs(zvalue) * SD + .001)
    x = seq(min, max, .001)
    plot(
      x,
      dnorm(x, hypothesized, SD),
      xlab = "",
      ylab = "",
      type = "l",
      ylim = c(0, dnorm(hypothesized, hypothesized, SD)),
      panel.first = grid()
    )

    zseq = c(
      hypothesized - 3 * SD,
      hypothesized - 2 * SD,
      hypothesized - SD,
      hypothesized,
      hypothesized + SD,
      hypothesized + 2 * SD,
      hypothesized + 3 * SD
    )
    axis(
      side = 1,
      at = zseq,
      labels = c("z=-3", "z=-2", "z=-1", "z=0", "z=1", "z=2", "z=3"),
      padj = 1.2,
      tick = FALSE,
      col.axis = "blue"
    )
    abline(h = 0, col = "black")
    mtext(side = 1, line = 3, "\u2190 Sample Proportions \u2192")
    mtext(side = 2, line = 2, "Density")
    if (alternative == "less") {
      drawseq = seq(min, statistic, .001)
      polygon(
        c(drawseq, statistic, min),
        c(dnorm(drawseq, hypothesized, SD), 0, 0),
        col = "red"
      )
      text(
        min,
        max(dnorm(hypothesized, hypothesized, SD)) * .9,
        labels = paste("z-statistic:", signif(zvalue, 4)),
        pos = 4,
        col = "blue"
      )
      text(
        min,
        max(dnorm(hypothesized, hypothesized, SD)) * .8,
        labels = paste("p-value:", pvalue),
        pos = 4,
        col = "red"
      )
    } else if (alternative == "greater") {
      drawseq = seq(statistic, max, .001)
      polygon(
        c(statistic, drawseq, max),
        c(0, dnorm(drawseq, hypothesized, SD), 0),
        col = "red"
      )
      text(
        max,
        max(dnorm(hypothesized, hypothesized, SD)) * .9,
        labels = paste("z-statistic:", signif(zvalue, 4)),
        pos = 2,
        col = "blue"
      )
      text(
        max,
        max(dnorm(hypothesized, hypothesized, SD)) * .8,
        labels = paste("p-value:", pvalue),
        pos = 2,
        col = "red"
      )
    } else if (alternative == "two.sided" || alternative == "not.equal") {
      if (statistic < hypothesized) {
        drawseq1 = seq(min, statistic, .001)
        drawseq2 = seq(hypothesized + (hypothesized - statistic), max, .001)
      } else {
        drawseq1 = seq(min, hypothesized - (statistic - hypothesized), .001)
        drawseq2 = seq(statistic, max, .001)
      }
      polygon(
        c(min, drawseq1, drawseq1[length(drawseq1)]),
        c(0, dnorm(drawseq1, hypothesized, SD), 0),
        col = "red"
      )
      polygon(
        c(drawseq2[1], drawseq2, max),
        c(0, dnorm(drawseq2, hypothesized, SD), 0),
        col = "red"
      )
      text(
        min,
        max(dnorm(hypothesized, hypothesized, SD)) * .9,
        labels = paste("z-statistic:", signif(zvalue, 4)),
        pos = 4,
        col = "blue"
      )
      text(
        min,
        max(dnorm(hypothesized, hypothesized, SD)) * .8,
        labels = paste("two-sided p-value:", pvalue),
        pos = 4,
        col = "red"
      )
    }
  }
  withr::local_par(mfrow = c(3, 1))
  if (length(conf.level) > 1) {
    withr::local_par(mar = c(4, 2, 1.5, 4), mfrow = c(length(conf.level), 1))
  }
  lower = 0
  upper = 0
  if (!is.null(conf.level)) {
    for (k in 1:length(conf.level)) {
      if (conf.level[k] > 1) {
        conf.level[k] = conf.level[k] / 100
      }
      myout = prop.test(
        observed,
        n,
        p = statistic,
        alternative = "two.sided",
        conf.level[k],
        correct = FALSE
      )
      criticalvalue = qnorm((1 - conf.level[k]) / 2)
      lower[k] = statistic +
        criticalvalue * sqrt(statistic * (1 - statistic) / n)
      upper[k] = statistic -
        criticalvalue * sqrt(statistic * (1 - statistic) / n)
      multconflevel = 100 * conf.level[k]
      cat(
        multconflevel,
        "% Confidence interval for pi: (",
        lower[k],
        ", ",
        upper[k],
        ") \n"
      )
    }
    if (is.null(hypothesized)) {
      SDphat = sqrt(statistic * (1 - statistic) / n)
      min = statistic - 4 * SDphat
      max = statistic + 4 * SDphat
      CIseq = seq(min, max, .001)

      if (length(conf.level) == 1) {
        myxlab = substitute(
          paste("Normal (", mean == x1, ", ", SD == x2, ")", ),
          list(x1 = signif(lower[1], 4), x2 = signif(SDphat, 4))
        )
        plot(CIseq, dnorm(CIseq, lower[1], SDphat), type = "l", xlab = " ")
        mtext("sample proportions", side = 1, line = 1.75, adj = .5, cex = .75)
        topseq = seq(statistic, max, .001)
        polygon(
          c(statistic, topseq, max),
          c(0, dnorm(topseq, lower[1], SDphat), 0),
          col = "red"
        )
        title(myxlab)
        myxlab = substitute(
          paste("Normal (", mean == x1, ", ", SD == x2, ")", ),
          list(x1 = signif(upper[1], 4), x2 = signif(SDphat, 4))
        )
        plot(
          seq(min, max, .001),
          dnorm(seq(min, max, .001), upper[1], SDphat),
          type = "l",
          xlab = " "
        )
        mtext("sample proportions", side = 1, line = 1.75, adj = .5, cex = .75)
        bottomseq = seq(min, statistic, .001)
        polygon(
          c(min, bottomseq, statistic, statistic),
          c(
            0,
            dnorm(bottomseq, upper[1], SDphat),
            dnorm(statistic, upper[1], SDphat),
            0
          ),
          col = "red"
        )
        newtitle = substitute(
          paste("Normal (", mean == x1, ", ", SD == x2, ")", ),
          list(x1 = signif(upper[1], 4), x2 = signif(SDphat, 4))
        )
        title(newtitle)
      }
      for (k in 1:length(conf.level)) {
        plot(
          c(min, statistic, max),
          c(1, 1, 1),
          pch = c(".", "^", "."),
          ylab = " ",
          xlab = "process probability",
          ylim = c(1, 1)
        )
        abline(v = statistic, col = "gray")
        text(min * 1.1, 1, labels = paste(conf.level[k] * 100, "% CI:"))
        text(statistic, .9, labels = signif(statistic, 4))
        text(lower[k], 1, labels = signif(lower[k], 4), pos = 3)
        text(upper[k], 1, labels = signif(upper[k], 4), pos = 3)
        points(c(lower[k], upper[k]), c(1, 1), pch = c("[", "]"))
        lines(c(lower[k], upper[k]), c(1, 1))
      }
    }
  }
  withr::local_par(mfrow = c(1, 1))
  invisible(list(
    "zvalue" = zvalue,
    "pvalue" = pvalue,
    "lower" = lower,
    "upper" = upper
  ))
}
