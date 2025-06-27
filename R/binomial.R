#' Overlays Normal Approximation onto Binomial
#'
#' `binomnorm` creates a binomial distribution of the given inputs and overlays a normal approximation.
#'
#' @param k number of successes of interest
#' @param n number of trials
#' @param prob success probability
#' @param direction "above", "below", or "two.sided"
#'
#' @return A plot of the binomial distribution overlayed with the normal approximation
#'
#' @export
#'
#' @examples
#' binomnorm(10, 20, 0.5, "two.sided")
binomnorm <- function(k, n, prob, direction) {
  withr::local_par(mar = c(5, 3, 1, 1))

  thisx <- 0:n
  phat <- thisx / n
  minx <- max(0, n * prob - 4 * sqrt(prob * (1 - prob) * n))
  maxx <- max(k + 1, min(n, n * prob + 4 * sqrt(prob * (1 - prob) * n)))
  myy <- max(dbinom(floor(n * prob), n, prob))
  plot(
    thisx,
    dbinom(thisx, size = n, prob),
    xlab = "",
    ylab = " ",
    type = "h",
    xlim = c(minx, maxx),
    panel.first = grid(),
    lwd = 2
  )
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 3, "Number of Successes (Proportion)")
  mtext(side = 2, line = 2, "Probability (Density)")

  axis(
    side = 1,
    at = thisx,
    labels = signif(phat, 2),
    padj = 1.2,
    tick = FALSE,
    col.axis = "blue"
  )
  normmean <- n * prob
  normsd <- sqrt(n * prob * (1 - prob))
  normseq <- seq(0, n, .001)
  lines(normseq, dnorm(normseq, normmean, normsd), col = "grey")
  if (direction == "below") {
    probseq <- seq(0, k, .001)
    phatseq <- probseq / n
    withcorrect <- seq(0, k + .5, .001)
    this.prob <- pbinom(k, n, prob)
    normprob <- pnorm(k, normmean, normsd)
    normprob2 <- pnorm(k + .5, normmean, normsd)
    showprob <- format(this.prob, digits = 4)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    polygon(
      c(withcorrect, k + .5, 0),
      c(dnorm(withcorrect, normmean, normsd), 0, 0),
      col = 6,
      border = 6
    )
    polygon(
      c(probseq, k, 0),
      c(dnorm(probseq, normmean, normsd), 0, 0),
      col = "light blue",
      border = "blue"
    )
    lines(0:k, dbinom(0:k, size = n, prob), col = "red", type = "h", lwd = 2)
    # lines(phatseq, dnorm(probseq,normmean,normsd))
    text(
      minx,
      myy * .9,
      labels = paste("P(X \u2264", k, ") \n =", showprob),
      col = "red",
      pos = 4
    )
  } else if (direction == "above") {
    this.prob <- 1 - pbinom(k - 1, n, prob)
    probseq <- seq(k, n, .001)
    withcorrect <- seq(k - .5, n, .001)
    normprob <- pnorm(k, normmean, normsd, lower.tail = FALSE)
    normprob2 <- pnorm(k - .5, normmean, normsd, lower.tail = FALSE)
    showprob <- format(this.prob, digits = 4)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    polygon(
      c(k - .5, withcorrect, n),
      c(0, dnorm(withcorrect, normmean, normsd), 0),
      col = 6,
      border = 6
    )
    polygon(
      c(k, probseq, n),
      c(0, dnorm(probseq, normmean, normsd), 0),
      col = "light blue",
      border = "blue"
    )
    lines(k:n, dbinom(k:n, size = n, prob), col = "red", type = "h", lwd = 2)
    text(
      maxx,
      myy * .9,
      labels = paste0("P(X \u2265 ", k, ")\n = ", showprob),
      col = "red",
      pos = 2
    )
  } else if (direction == "two.sided") {
    if (k < normmean) {
      k1 <- k
      k2 <- floor(min(normmean - k + normmean, n))
      newvalue <- dbinom(k2, size = n, prob)
      if (newvalue <= dbinom(k1, size = n, prob) + .00001) {
        k2 <- k2
      } else {
        k2 <- k2 + 1
      }
    } else {
      k1 <- floor(min(normmean - (k - normmean), n))
      k2 <- k
      newvalue <- dbinom(k1, size = n, prob)
      if (newvalue <= dbinom(k, size = n, prob) + .00001) {
        k1 <- k1
      } else {
        k1 <- k1 - 1
      }
    }
    this.prob <- pbinom(k1, n, prob) +
      pbinom(k2 - 1, n, prob, lower.tail = FALSE)
    normprob <- pnorm(k1, normmean, normsd) +
      pnorm(k2, normmean, normsd, lower.tail = FALSE)
    normprob2 <- pnorm(k1 + .5, normmean, normsd) +
      pnorm(k2 - .5, normmean, normsd, lower.tail = FALSE)
    showprob <- format(this.prob, digits = 4)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    probseq1 <- seq(0, k1, .001)
    probseq2 <- seq(k2, n, .001)
    withcorrect <- seq(0, k1 + .5, .001)
    withcorrect2 <- seq(k2 - .5, n, .001)
    polygon(
      c(withcorrect, k1 + .5, 0),
      c(dnorm(withcorrect, normmean, normsd), 0, 0),
      col = 6,
      border = 6
    )
    polygon(
      c(probseq1, k1, 0),
      c(dnorm(probseq1, normmean, normsd), 0, 0),
      col = "light blue",
      border = "blue"
    )
    polygon(
      c(k2 - .5, withcorrect2, n),
      c(0, dnorm(withcorrect2, normmean, normsd), 0),
      col = 6,
      border = 6
    )
    polygon(
      c(k2, probseq2, n),
      c(0, dnorm(probseq2, normmean, normsd), 0),
      col = "light blue",
      border = "blue"
    )
    lines(0:k1, dbinom(0:k1, size = n, prob), col = "red", type = "h")
    lines(k2:n, dbinom(k2:n, size = n, prob), col = "red", type = "h")
    text(
      minx,
      myy * .85,
      labels = paste(
        "P(X \u2264",
        k1,
        ") + P(X \u2265",
        k2,
        ") \n =",
        showprob
      ),
      col = "red",
      pos = 4
    )
  }
  newtitle <- substitute(
    paste(
      "Binomial (",
      n == x1,
      ", ",
      pi == x2,
      "), Normal(",
      mean == x3,
      ",  ",
      SD == x4,
      ")"
    ),
    list(x1 = n, x2 = prob, x3 = prob, x4 = signif(normsd / n, 4))
  )
  title(newtitle)
  full <- c(
    c(" binomial:", showprob),
    c("\n normal approx:", showprob2),
    c("\n normal approx with continuity:", showprob3)
  )
  cat(full, "\n")
}

#' Rejection Region for Binomial
#'
#' `binompower` determines the rejection region corresponding to the level of
#'  significance and the first probability and shows the binomial distribution
#'  shading its corresponding region.
#'
#' @param LOS A numeric value representing the level of significance
#' @param n A numeric value representing the sample size
#' @param prob1 A numeric value representing the first probability
#' @param alternative "less", "greater", or "two.sided"
#' @param prob2 A numeric value representing the second probability
#'
#' @return A plot of the binomial distribution with the rejection region highlighted.
#'
#' @export
#'
#' @examples
binompower <- function(LOS, n, prob1, alternative, prob2 = NULL) {
  Description <- "iscambinompower(LOS, n, prob1, alternative, prob2) \n This function determines the rejection region \n corresponding to the level of significance and the first probability \n and shows the second distribution shading its corresponding region. \n alternative can be \"less\", \"greater\", or \"two.sided\"."

  if (as.character(LOS) == "?") {
    stop(Description)
  }

  thisx <- 0:max(n)
  minx <- max(
    0,
    min(
      n * prob1 - 4 * sqrt(prob1 * (1 - prob1) * n),
      n * prob2 - 4 * sqrt(prob2 * (1 - prob2) * n)
    )
  )
  maxx <- min(
    n,
    max(
      n * prob1 + 4 * sqrt(prob1 * (1 - prob1) * n),
      n * prob2 + 4 * sqrt(prob2 * (1 - prob2) * n)
    )
  )
  myy1 <- dbinom(floor(n * prob1), n, prob1) / 2
  withr::local_par(mfrow = c(2, 1))
  withr::local_par(mar = c(4, 3, 2, 2))
  plot(
    thisx,
    dbinom(thisx, size = n, prob1),
    xlab = "",
    ylab = " ",
    type = "h",
    xlim = c(minx, maxx),
    panel.first = grid(),
    lwd = 2
  )
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 2, "Number of Successes")
  mtext(side = 2, line = 2, "Probability")

  if (alternative == "less") {
    rr <- qbinom(LOS, n, prob1) - 1
    this.prob1 <- pbinom(rr, n, prob1)
    showprob1 <- format(this.prob1, digits = 4)
    lines(0:rr, dbinom(0:rr, size = n, prob1), col = "red", type = "h")
    text(
      (minx + n * prob1) / 2,
      myy1,
      labels = paste0("P(X \u2264 ", rr, ")\n = ", showprob1),
      pos = 3,
      col = "red"
    )
    cat("Null: Probability", rr, "and below =", this.prob1, "\n")
  } else if (alternative == "greater") {
    rr <- qbinom(LOS, n, prob1, FALSE) + 1
    this.prob1 <- 1 - pbinom(rr - 1, n, prob1)
    showprob1 <- format(this.prob1, digits = 4)
    lines(rr:n, dbinom(rr:n, size = n, prob1), col = "red", type = "h")
    text(
      (maxx + n * prob1) / 2,
      myy1,
      labels = paste0("P(X \u2265 ", rr, ")\n = ", showprob1),
      pos = 3,
      col = "red"
    )
    cat("Null: Probability", rr, "and above =", this.prob1, "\n")
  } else if (alternative == "two.sided") {
    lowerrr <- qbinom(LOS / 2, n, prob1) - 1
    upperrr <- qbinom(LOS / 2, n, prob1, FALSE) + 1
    lowerprob1 <- pbinom(lowerrr, n, prob1)
    upperprob1 <- pbinom(upperrr - 1, n, prob1, FALSE)
    showlowerprob1 <- format(lowerprob1, digits = 4)
    showupperprob1 <- format(upperprob1, digits = 4)
    showprob1 <- format(lowerprob1 + upperprob1, digits = 4)
    lines(
      0:lowerrr,
      dbinom(0:lowerrr, size = n, prob1),
      col = "red",
      type = "h"
    )
    lines(
      upperrr:n,
      dbinom(upperrr:n, size = n, prob1),
      col = "red",
      type = "h"
    )
    text(
      (maxx + n * prob1) / 2,
      myy1,
      labels = paste(
        "P(X \u2264",
        lowerrr,
        ")+P(X \u2265",
        upperrr,
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
    paste("Binomial (", n == x1, ", ", pi == x2, ") - null", ),
    list(x1 = n, x2 = prob1)
  )
  title(newtitle)

  if (!is.null(prob2)) {
    plot(
      thisx,
      dbinom(thisx, size = n, prob2),
      xlab = " ",
      ylab = " ",
      type = "h",
      xlim = c(minx, maxx),
      lwd = 2,
      panel.first = grid()
    )
    abline(h = 0, col = "gray")
    mtext(side = 1, line = 2, "Number of Successes")
    mtext(side = 2, line = 2, "Probability")
    myy2 <- dbinom(floor(n * prob2), n, prob2) / 2
    if (alternative == "less") {
      this.prob2 <- pbinom(rr, n, prob2)
      showprob2 <- format(this.prob2, digits = 4)
      lines(0:rr, dbinom(0:rr, size = n, prob2), col = "red", type = "h")
      text(
        (minx + n * prob2) / 2,
        myy2,
        labels = paste("P(X \u2264", rr, ")\n =", showprob2),
        pos = 3,
        col = "red"
      )
      cat("Alternative: Probability", rr, "and below =", this.prob2, "\n")
    } else if (alternative == "greater") {
      this.prob2 <- 1 - pbinom(rr - 1, n, prob2)
      showprob2 <- format(this.prob2, digits = 4)
      lines(rr:n, dbinom(rr:n, size = n, prob2), col = "red", type = "h")
      text(
        (maxx + n * prob2) / 2,
        myy2,
        labels = paste("P(X \u2265", rr, ")\n =", showprob2),
        pos = 3,
        col = "red"
      )
      cat("Alternative: Probability", rr, "and above =", this.prob2, "\n")
    } else if (alternative == "two.sided") {
      this.prob2 <- pbinom(lowerrr, n, prob2) +
        pbinom(upperrr - 1, n, prob2, FALSE)
      showprob2 <- format(this.prob2, digits = 4)
      lines(
        0:lowerrr,
        dbinom(0:lowerrr, size = n, prob2),
        col = "red",
        type = "h"
      )
      lines(
        upperrr:n,
        dbinom(upperrr:n, size = n, prob2),
        col = "red",
        type = "h"
      )
      text(
        (maxx + n * prob2) / 2,
        myy1,
        labels = paste(
          "P(X \u2264",
          lowerrr,
          ")+P(X \u2265",
          upperrr,
          ")\n =",
          showprob2
        ),
        pos = 3,
        col = "red"
      )
      cat("Alternative: Probability in rejection region", this.prob2, "\n")
    }
    newtitle <- substitute(
      paste("Binomial (", n == x1, ", ", pi == x2, ") - alternative", ),
      list(x1 = n, x2 = prob2)
    )
    title(newtitle)
  }
  withr::local_par(mfrow = c(1, 1))
}

#' Calculate Binomial Tail Probabilities
#'
#' `binomprob` calculates the probability of the number of success of interest
#'    using a binomial distribution and plots the distribution.
#'
#' @param k number of successes of interest.
#' @param n number of trials.
#' @param prob success probability. Numeric between 0 & 1.
#' @param lower.tail Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#'
#' @return The probability of the binomial distribution along with a graph of the distribution.
#' @export
#'
#' @examples
binomprob <- function(k, n, prob, lower.tail) {
  Description <- "iscambinomprob(k, n, prob, lower.tail) \n This function calculates tail probabilities from the binomial distribution.\r
k is the number of successes of interest (must be integer), n and prob are the number of trials and success probability \n lower.tail is a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)"
  # TODO Stop if probability is wrong

  if (as.character(k) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(4, 3, 2, 2))
  thisx <- 0:n
  minx <- max(0, n * prob - 4 * sqrt(prob * (1 - prob) * n))
  maxx <- min(n, n * prob + 4 * sqrt(prob * (1 - prob) * n))
  maxx <- max(k + 1, maxx)
  #   myy=dbinom(floor(n*prob), n, prob)/2
  myy <- dbinom(floor(n * prob), n, prob)
  plot(
    thisx,
    dbinom(thisx, size = n, prob),
    xlab = " ",
    ylab = " ",
    type = "h",
    xlim = c(minx, maxx),
    panel.first = grid(),
    lwd = 2
  )
  abline(h = 0, col = "gray")

  if (lower.tail) {
    this.prob <- pbinom(k, n, prob)
    showprob <- format(this.prob, digits = 4)
    lines(0:k, dbinom(0:k, size = n, prob), col = "red", type = "h", lwd = 2)
    text(
      (minx + n * prob) / 4,
      myy,
      labels = paste("P(X \u2264", k, ")\n =", showprob),
      pos = 1,
      col = "red"
    )
    cat("Probability", k, "and below =", this.prob, "\n")
  }
  if (!lower.tail) {
    this.prob <- 1 - pbinom(k - 1, n, prob)
    showprob <- format(this.prob, digits = 4)
    lines(k:n, dbinom(k:n, size = n, prob), col = "red", type = "h", lwd = 2)
    text(
      (maxx + n * prob) * 9 / 16,
      myy,
      labels = paste("P(X \u2265", k, ")\n =", showprob),
      pos = 1,
      col = "red"
    )
    cat("Probability", k, "and above =", this.prob, "\n")
  }
  newtitle <- substitute(
    paste("Binomial (", n == x1, ", ", pi == x2, ")", ),
    list(x1 = n, x2 = prob)
  )
  title(newtitle)
  mtext(side = 1, line = 2, "Number of Successes")
  mtext(side = 2, line = 2, "Probability")

  return(this.prob)
}

#' Exact Binomial Test
#'
#' `binomtest` calculates performas an exact binomial test and graphs the
#'    binomial distribution and/or binomial confidence interval.
#'
#' @param observed The observed number of successes or sample proportion (assumed
#'    to be proportion if value less than one.)
#' @param n number of trials.
#' @param hypothesized hypothesized probability of success.
#' @param alternative "less", "greater", or "two.sided"
#' @param conf.level Confidence level for a two-sided confidence interval.
#'
#' @return P-value along with a plot of the binomial distribution and/or
#'  binomial confidence interval.
#' @export
#'
#' @examples
binomtest <- function(
  observed,
  n,
  hypothesized = NULL,
  alternative,
  conf.level = NULL
) {
  # TODO Better documentation that takes into account all parts of description
  Description <- "iscambinomtest(observed, n, hypothesized=NULL, alternative, conf.level=NULL) \n This function performs an exact binomial test and graphs the binomial distribution and/or binomial confidence interval.\n Input the observed number of successes or sample proportion (assumed if value less than one),\n Input n = the sample size and the hypothesized probability of success  \n Optional: Input the hypothesized probability of success and form of alternative (\"less\", \"greater\", or \"two.sided\") \n Optional: Input a confidence level (one or more values) for a two-sided confidence interval.\n "

  if (as.character(observed) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(4, 3, 2, 2))

  if (observed < 1) {
    observed <- round(n * observed)
  }
  pvalue <- NULL
  if (!is.null(hypothesized)) {
    minx <- max(
      0,
      n * hypothesized - 4 * sqrt(hypothesized * (1 - hypothesized) * n)
    )
    maxx <- min(
      n,
      n * hypothesized + 4 * sqrt(hypothesized * (1 - hypothesized) * n)
    )
    maxx <- max(observed + 1, maxx)
    myy <- max(dbinom(floor(n * hypothesized), n, hypothesized)) * .9
    x <- 0:n
    plot(
      x,
      dbinom(x, size = n, prob = hypothesized),
      xlab = "",
      ylab = " ",
      type = "h",
      xlim = c(minx, maxx),
      panel.first = grid(),
      lwd = 2
    )
    newtitle <- substitute(
      paste("Binomial (", n == x1, ", ", pi == x2, ")", ),
      list(x1 = n, x2 = hypothesized)
    )
    title(newtitle)
    mtext(side = 1, line = 2, "Number of Successes")
    mtext(side = 2, line = 2, "Probability")

    if (alternative == "less") {
      pvalue <- pbinom(observed, size = n, prob = hypothesized, TRUE)
      lines(
        0:observed,
        dbinom(0:observed, size = n, prob = hypothesized),
        col = "red",
        type = "h",
        lwd = 2
      )
      text(
        minx,
        myy,
        labels = paste("p-value:", signif(pvalue, 4)),
        pos = 4,
        col = "red"
      )
    } else if (alternative == "greater") {
      value <- observed - 1
      pvalue <- pbinom(value, size = n, prob = hypothesized, FALSE)
      lines(
        observed:n,
        dbinom(observed:n, size = n, prob = hypothesized),
        col = "red",
        type = "h",
        lwd = 2
      )
      text(
        maxx,
        myy,
        labels = paste("p-value:", signif(pvalue, 4)),
        pos = 2,
        col = "red"
      )
    } else {
      pvalue <- 0
      firstvalue <- dbinom(observed, size = n, prob = hypothesized)
      for (y in 0:n) {
        newvalue <- dbinom(y, size = n, prob = hypothesized)
        if (newvalue <= firstvalue + .00001) {
          pvalue <- pvalue + newvalue
          lines(y, newvalue, col = "red", type = "h", lwd = 2)
        }
      }
      text(
        minx,
        myy,
        labels = paste("two-sided p-value:\n", signif(pvalue, 4)),
        pos = 4,
        col = "red"
      )
    }
    pvalue <- signif(pvalue, 5)
    abline(h = 0, col = "gray")
    abline(v = 0, col = "gray")
  }
  cat("\n", "Exact Binomial Test\n", sep = "", "\n")
  statistic <- signif(observed / n, 4)
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

  if (!is.null(hypothesized)) {
    cat(paste("Null hypothesis       : pi =", hypothesized, sep = " "), "\n")
    altname <- switch(alternative, less = "<", greater = ">", two.sided = "<>")
    cat(
      paste("Alternative hypothesis: pi", altname, hypothesized, sep = " "),
      "\n"
    )
    cat(paste("p-value:", pvalue, sep = " "), "\n")
  }
  p.L <- function(x, alpha) {
    if (x == 0) {
      0
    } else {
      qbeta(alpha, x, n - x + 1)
    }
  }
  p.U <- function(x, alpha) {
    if (x == n) {
      1
    } else {
      qbeta(1 - alpha, x + 1, n - x)
    }
  }
  CINT <- 0
  multconflevel <- 0
  lower1 <- NULL
  upper1 <- NULL
  if (!is.null(conf.level)) {
    for (k in 1:length(conf.level)) {
      if (conf.level[k] > 1) {
        conf.level[k] <- conf.level[k] / 100
      }
      alpha <- (1 - conf.level[k]) / 2
      CINT <- c(
        signif(p.L(observed, alpha), 5),
        ",",
        signif(p.U(observed, alpha), 5)
      )
      multconflevel <- 100 * conf.level[k]
      cat(multconflevel, "% Confidence interval for pi: (", CINT, ") \n")
      lower1[k] <- as.numeric(CINT[1])
      upper1[k] <- as.numeric(CINT[3])
    }
  }
  withr::local_par(mar = c(4, 2, 1.5, .5), mfrow = c(3, 1))
  if (length(conf.level) > 1) {
    withr::local_par(mar = c(4, 2, 1.5, .4), mfrow = c(length(conf.level), 1))
  }

  if (is.null(hypothesized)) {
    statistic <- observed / n
    # lower=lower1[1]; upper=upper1[1]
    SDphat <- sqrt(statistic * (1 - statistic) / n)
    min <- statistic - 4 * SDphat
    max <- statistic + 4 * SDphat
    CIseq <- seq(min, max, .01)
    minx <- as.integer(max(0, min * n))
    maxx <- as.integer(min(n, max * n))

    if (length(conf.level) == 1) {
      myxlab <- substitute(
        paste("Binomial (", n == x1, ", ", pi == x2, ")", ),
        list(x1 = n, x2 = signif(lower1[1], 4))
      )
      plot(
        seq(minx, maxx),
        dbinom(seq(minx, maxx), size = n, prob = lower1[1]),
        xlab = "  ",
        ylab = " ",
        type = "h",
        xlim = c(minx, maxx)
      )
      mtext("Number of successes", side = 1, line = 1.75, adj = .5, cex = .75)
      title(myxlab)
      lines(
        observed:n,
        dbinom(observed:n, size = n, prob = lower1[1]),
        col = "red",
        type = "h"
      )

      myxlab <- substitute(
        paste("Binomial (", n == x1, ", ", pi == x2, ")", ),
        list(x1 = n, x2 = signif(upper1[1], 4))
      )
      plot(
        seq(minx, maxx),
        dbinom(seq(minx, maxx), size = n, prob = upper1[1]),
        xlab = " ",
        ylab = " ",
        type = "h",
        xlim = c(minx, maxx)
      )
      lines(
        0:observed,
        dbinom(0:observed, size = n, prob = upper1[1]),
        col = "red",
        type = "h"
      )

      mtext("Number of successes", side = 1, line = 1.75, adj = .5, cex = .75)
      title(myxlab)
    } # end only one interval

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
      text(min, 1, labels = paste(conf.level[k] * 100, "% CI:"))
      text(statistic, .9, labels = signif(statistic, 4))
      text(lower1[k], 1, labels = signif(lower1[k], 4), pos = 3)
      text(upper1[k], 1, labels = signif(upper1[k], 4), pos = 3)
      points(c(lower1[k], upper1[k]), c(1, 1), pch = c("[", "]"))
      lines(c(lower1[k], upper1[k]), c(1, 1))
    } # end intervals loop
  } # end no hypothesized

  withr::local_par(mfrow = c(1, 1))
  invisible(list("pvalue" = pvalue, "lower" = lower1, "upper" = upper1))
}

#' Inverse Binomial Probability
#'
#' @param alpha The probability of interest.
#' @param n The number of trials.
#' @param prob The probability of success.
#' @param lower.tail Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#'
#' @return numeric which achieves at most the stated probability
#' @export
#'
#' @examples
invbinom <- function(alpha, n, prob, lower.tail) {
  Description <- "iscaminvbinom(alpha, n, prob, lower.tail) \n This function calculates the binomial quantile of a specified probability. \n Input the desired probability and the parameters of the binomial distribution. \n Specify whether you want this is an upper tail (FALSE) or lower tail (TRUE) \n The integer that achieves at most the stated probability will be returned."

  if (as.character(alpha) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(4, 3, 2, 2))

  thisx <- 0:n
  minx <- max(0, n * prob - 4 * sqrt(prob * (1 - prob) * n))
  maxx <- min(n, n * prob + 4 * sqrt(prob * (1 - prob) * n))
  myy <- dbinom(floor(n * prob), n, prob) / 2
  plot(
    thisx,
    dbinom(thisx, size = n, prob),
    xlab = " ",
    ylab = " ",
    type = "h",
    xlim = c(minx, maxx),
    panel.first = grid(),
    lwd = 2
  )
  newtitle <- substitute(
    paste("Binomial (", n == x1, ", ", pi == x2, ")", ),
    list(x1 = n, x2 = prob)
  )
  title(newtitle)
  mtext(side = 1, line = 2, "X = Number of Successes")
  mtext(side = 2, line = 2, "Probability")

  abline(h = 0, col = "gray")
  if (lower.tail) {
    answer <- qbinom(alpha, n, prob, lower.tail) - 1
    actualprob <- format(pbinom(answer, n, prob, lower.tail), digits = 4)
    lines(
      0:answer,
      dbinom(0:answer, size = n, prob),
      col = "red",
      type = "h",
      lwd = 2
    )
    text(
      x = minx,
      y = myy,
      labels = paste(
        "P(X \u2264",
        answer,
        ")=",
        actualprob,
        "\nP(X \u2264",
        answer + 1,
        ")=",
        format(pbinom(answer + 1, n, prob, lower.tail), digits = 4)
      ),
      col = "red",
      pos = 4
    )
    # text(answer, dbinom(answer, size=n, prob), labels=actualprob, pos=2, col="red")
    text(
      answer,
      dbinom(answer, size = n, prob),
      labels = paste("X=", answer),
      col = "red",
      pos = 3
    )
    cat(
      "The observation with at most",
      alpha,
      "probability at or below is",
      answer,
      "\n"
    )
  }
  if (!lower.tail) {
    answer <- qbinom(alpha, n, prob, lower.tail) + 1
    actualprob <- format(pbinom(answer - 1, n, prob, lower.tail), digits = 4)
    lines(
      answer:n,
      dbinom(answer:n, size = n, prob),
      col = "red",
      type = "h",
      lwd = 2
    )
    myx <- (maxx + n * prob) / 2
    text(
      x = myx,
      y = myy,
      labels = paste(
        "P(X \u2265",
        answer,
        ")=",
        actualprob,
        "\n P(X \u2265",
        answer - 1,
        ")=",
        format(pbinom(answer - 2, n, prob, lower.tail), digits = 4)
      ),
      col = "red"
    )
    # text(answer, dbinom(answer, size=n, prob), labels=actualprob, pos=2, col="red")
    text(
      answer,
      dbinom(answer, n, prob),
      labels = paste("X=", answer),
      col = "red",
      pos = 3
    )
    cat(
      "The observation with at most",
      alpha,
      "probability at or above is",
      answer,
      "\n"
    )
  }
  answer
}
