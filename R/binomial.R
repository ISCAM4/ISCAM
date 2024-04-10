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
#' @importFrom graphics axis legend polygon text title
#' @importFrom stats dbinom pbinom pnorm
#' @export
#'
#' @examples
binomnorm <- function(k, n, prob, direction) {
  Description <- "iscambinomnorm(k, n, prob, direction) \n This function illustrates the normal approximation to the binomial.\n  k is the number of successes of interest, n and prob are the number of trials and success probability \n direction allows you to specify whether you want to find the probability \"above\" or \"below\" k \n or a symmetric \"two.sided\" probability "

  if (as.character(k) == "?") stop(Description)
  par(mar = c(5, 3, 1, 1))

  thisx <- 0:n
  phat <- thisx / n
  minx <- max(0, n * prob - 4 * sqrt(prob * (1 - prob) * n))
  maxx <- min(n, n * prob + 4 * sqrt(prob * (1 - prob) * n))
  maxx <- max(k + 1, maxx)
  myy <- max(dbinom(floor(n * prob), n, prob))
  plot(thisx, dbinom(thisx, size = n, prob), xlab = "", ylab = " ", type = "h", xlim = c(minx, maxx), panel.first = grid(), lwd = 2)
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 3, "Number of Successes (Proportion)")
  mtext(side = 2, line = 2, "Probability (Density)")

  axis(side = 1, at = thisx, labels = signif(phat, 2), padj = 1.2, tick = FALSE, col.axis = "blue")
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
    polygon(c(withcorrect, k + .5, 0), c(dnorm(withcorrect, normmean, normsd), 0, 0), col = 6, border = 6)
    polygon(c(probseq, k, 0), c(dnorm(probseq, normmean, normsd), 0, 0), col = "light blue", border = "blue")
    lines(0:k, dbinom(0:k, size = n, prob), col = "red", type = "h", lwd = 2)
    # lines(phatseq, dnorm(probseq,normmean,normsd))
    text(minx, myy * .9, labels = paste("P(X\u2264", k, ") \n =", showprob), col = "red", pos = 4)
  } else if (direction == "above") {
    this.prob <- 1 - pbinom(k - 1, n, prob)
    probseq <- seq(k, n, .001)
    withcorrect <- seq(k - .5, n, .001)
    normprob <- pnorm(k, normmean, normsd, lower.tail = FALSE)
    normprob2 <- pnorm(k - .5, normmean, normsd, lower.tail = FALSE)
    showprob <- format(this.prob, digits = 4)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    polygon(c(k - .5, withcorrect, n), c(0, dnorm(withcorrect, normmean, normsd), 0), col = 6, border = 6)
    polygon(c(k, probseq, n), c(0, dnorm(probseq, normmean, normsd), 0), col = "light blue", border = "blue")
    lines(k:n, dbinom(k:n, size = n, prob), col = "red", type = "h", lwd = 2)
    text(maxx, myy * .9, labels = paste("P(X\u2265", k, ")\n =", showprob), col = "red", pos = 2)
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
    this.prob <- pbinom(k1, n, prob) + pbinom(k2 - 1, n, prob, lower.tail = FALSE)
    normprob <- pnorm(k1, normmean, normsd) + pnorm(k2, normmean, normsd, lower.tail = FALSE)
    normprob2 <- pnorm(k1 + .5, normmean, normsd) + pnorm(k2 - .5, normmean, normsd, lower.tail = FALSE)
    showprob <- format(this.prob, digits = 4)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    probseq1 <- seq(0, k1, .001)
    probseq2 <- seq(k2, n, .001)
    withcorrect <- seq(0, k1 + .5, .001)
    withcorrect2 <- seq(k2 - .5, n, .001)
    polygon(c(withcorrect, k1 + .5, 0), c(dnorm(withcorrect, normmean, normsd), 0, 0), col = 6, border = 6)
    polygon(c(probseq1, k1, 0), c(dnorm(probseq1, normmean, normsd), 0, 0), col = "light blue", border = "blue")
    polygon(c(k2 - .5, withcorrect2, n), c(0, dnorm(withcorrect2, normmean, normsd), 0), col = 6, border = 6)
    polygon(c(k2, probseq2, n), c(0, dnorm(probseq2, normmean, normsd), 0), col = "light blue", border = "blue")
    lines(0:k1, dbinom(0:k1, size = n, prob), col = "red", type = "h")
    lines(k2:n, dbinom(k2:n, size = n, prob), col = "red", type = "h")
    text(minx, myy * .85, labels = paste("P(X\u2264", k1, ") + P(X\u2265", k2, ") \n =", showprob), col = "red", pos = 4)
  }
  newtitle <- substitute(paste("Binomial (", n == x1, ", ", pi == x2, "), Normal(", mean == x3, ",  ", SD == x4, ")"), list(x1 = n, x2 = prob, x3 = prob, x4 = signif(normsd / n, 4)))
  title(newtitle)
  full <- c(c(" binomial:", showprob), c("\n normal approx:", showprob2), c("\n normal approx with continuity:", showprob3))
  cat(full, "\n")
}

#' Regection Region for Binomial
#'
#' @param LOS A numeric value representing the level of significance
#' @param n A numeric value representing the sample size
#' @param prob1 A numeric value representing the first probability
#' @param alternative A character value representing the alternative hypothesis
#' @param prob2 A numeric value representing the second probability
#'
#' @return A plot of the binomial distribution with the rejection region highlighted.
#'
#' @importFrom stats qbinom
#' @export
#'
#' @examples
binompower <- function(LOS, n, prob1, alternative, prob2 = NULL) {
  Description <- "iscambinompower(LOS, n, prob1, alternative, prob2) \n This function determines the rejection region \n corresponding to the level of significance and the first probability \n and shows the second distribution shading its corresponding region. \n alternative can be \"less\", \"greater\", or \"two.sided\"."

  if (as.character(LOS) == "?") stop(Description)

  thisx <- 0:max(n)
  minx <- max(0, min(n * prob1 - 4 * sqrt(prob1 * (1 - prob1) * n), n * prob2 - 4 * sqrt(prob2 * (1 - prob2) * n)))
  maxx <- min(n, max(n * prob1 + 4 * sqrt(prob1 * (1 - prob1) * n), n * prob2 + 4 * sqrt(prob2 * (1 - prob2) * n)))
  myy1 <- dbinom(floor(n * prob1), n, prob1) / 2
  par(mfrow = c(2, 1))
  par(mar = c(4, 3, 2, 2))
  plot(thisx, dbinom(thisx, size = n, prob1), xlab = "", ylab = " ", type = "h", xlim = c(minx, maxx), panel.first = grid(), lwd = 2)
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 2, "Number of Successes")
  mtext(side = 2, line = 2, "Probability")

  if (alternative == "less") {
    rr <- qbinom(LOS, n, prob1) - 1
    this.prob1 <- pbinom(rr, n, prob1)
    showprob1 <- format(this.prob1, digits = 4)
    lines(0:rr, dbinom(0:rr, size = n, prob1), col = "red", type = "h")
    text((minx + n * prob1) / 2, myy1, labels = paste("P(X\u2264", rr, ")\n =", showprob1), pos = 3, col = "red")
    cat("Null: Probability", rr, "and below =", this.prob1, "\n")
  } else if (alternative == "greater") {
    rr <- qbinom(LOS, n, prob1, FALSE) + 1
    this.prob1 <- 1 - pbinom(rr - 1, n, prob1)
    showprob1 <- format(this.prob1, digits = 4)
    lines(rr:n, dbinom(rr:n, size = n, prob1), col = "red", type = "h")
    text((maxx + n * prob1) / 2, myy1, labels = paste("P(X\u2265", rr, ")\n =", showprob1), pos = 3, col = "red")
    cat("Null: Probability", rr, "and above =", this.prob1, "\n")
  } else if (alternative == "two.sided") {
    lowerrr <- qbinom(LOS / 2, n, prob1) - 1
    upperrr <- qbinom(LOS / 2, n, prob1, FALSE) + 1
    lowerprob1 <- pbinom(lowerrr, n, prob1)
    upperprob1 <- pbinom(upperrr - 1, n, prob1, FALSE)
    showlowerprob1 <- format(lowerprob1, digits = 4)
    showupperprob1 <- format(upperprob1, digits = 4)
    showprob1 <- format(lowerprob1 + upperprob1, digits = 4)
    lines(0:lowerrr, dbinom(0:lowerrr, size = n, prob1), col = "red", type = "h")
    lines(upperrr:n, dbinom(upperrr:n, size = n, prob1), col = "red", type = "h")
    text((maxx + n * prob1) / 2, myy1,
      labels =
        paste("P(X\u2264", lowerrr, ")+P(X\u2265", upperrr, ")\n =", showlowerprob1, "+", showupperprob1, "\n =", showprob1), pos = 3, col = "red"
    )
    cat("Null: Probability in rejection region", showprob1, "\n")
  } else {
    stop("Check input for alternative")
  }

  newtitle <- substitute(paste("Binomial (", n == x1, ", ", pi == x2, ") - null", ), list(x1 = n, x2 = prob1))
  title(newtitle)


  if (!is.null(prob2)) {
    plot(thisx, dbinom(thisx, size = n, prob2), xlab = " ", ylab = " ", type = "h", xlim = c(minx, maxx), lwd = 2, panel.first = grid())
    abline(h = 0, col = "gray")
    mtext(side = 1, line = 2, "Number of Successes")
    mtext(side = 2, line = 2, "Probability")
    myy2 <- dbinom(floor(n * prob2), n, prob2) / 2
    if (alternative == "less") {
      this.prob2 <- pbinom(rr, n, prob2)
      showprob2 <- format(this.prob2, digits = 4)
      lines(0:rr, dbinom(0:rr, size = n, prob2), col = "red", type = "h")
      text((minx + n * prob2) / 2, myy2, labels = paste("P(X\u2264", rr, ")\n =", showprob2), pos = 3, col = "red")
      cat("Alternative: Probability", rr, "and below =", this.prob2, "\n")
    } else if (alternative == "greater") {
      this.prob2 <- 1 - pbinom(rr - 1, n, prob2)
      showprob2 <- format(this.prob2, digits = 4)
      lines(rr:n, dbinom(rr:n, size = n, prob2), col = "red", type = "h")
      text((maxx + n * prob2) / 2, myy2, labels = paste("P(X\u2265", rr, ")\n =", showprob2), pos = 3, col = "red")
      cat("Alternative: Probability", rr, "and above =", this.prob2, "\n")
    } else if (alternative == "two.sided") {
      this.prob2 <- pbinom(lowerrr, n, prob2) + pbinom(upperrr - 1, n, prob2, FALSE)
      showprob2 <- format(this.prob2, digits = 4)
      lines(0:lowerrr, dbinom(0:lowerrr, size = n, prob2), col = "red", type = "h")
      lines(upperrr:n, dbinom(upperrr:n, size = n, prob2), col = "red", type = "h")
      text((maxx + n * prob2) / 2, myy1, labels = paste("P(X\u2264", lowerrr, ")+P(X\u2265", upperrr, ")\n =", showprob2), pos = 3, col = "red")
      cat("Alternative: Probability in rejection region", this.prob2, "\n")
    }
    newtitle <- substitute(paste("Binomial (", n == x1, ", ", pi == x2, ") - alternative", ), list(x1 = n, x2 = prob2))
    title(newtitle)
  }
  par(mfrow = c(1, 1))
}
