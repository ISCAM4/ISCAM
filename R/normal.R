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
normprob <- function(
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
invnorm <- function(prob1, mean = 0, sd = 1, direction) {
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
normpower <- function(LOS, n, prob1, alternative, prob2) {
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
