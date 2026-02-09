#' Hypergeometric p-value and Distribution Overlaid with Normal Distribution
#'
#' @param k Number of successes of interest or difference in conditional proportions
#' @param total Total number of observations in the study
#' @param succ Overall number of successes
#' @param n Number of observations in group A
#' @param lower.tail Boolean for finding the probability above (FALSE) or
#'  below (TRUE) the inputted value (inclusive)
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return Tail probabilities from the hypergeometric distribution,
#'  hypergeometric distribution with normal distribution overlayed with the
#'  observed statistic and more extreme shaded.
#' @export
#'
#' @examples
#' iscamhypernorm(1, 20, 5, 10, TRUE)
iscamhypernorm <- function(k, total, succ, n, lower.tail, verbose = TRUE) {
  if (.iscam_maybe_help(k, "iscamhypernorm")) {
    return(invisible())
  }

  # TODO rewrite so that it uses hyperprob and overlay normal?
  old <- par(mar = c(4, 4, 2, 1))
  on.exit(par(old), add = TRUE)

  if (k < 1) {
    k <- round((k * n * (total - n) + n * succ) / total)
  }

  fail <- total - succ
  thisx <- max(0, n - fail):min(n, succ)
  normseq <- seq(max(0, n - fail), min(n, succ), 0.001)
  .iscam_plot_discrete_distribution(
    x = thisx,
    prob_y = dhyper(thisx, succ, fail, n),
    x_label = "Number of Successes",
    y_label = "Probability"
  )

  normmean <- n * succ / total
  normsd <- sqrt(
    n * succ / total * (total - n) / total * (total - succ) / (total - 1)
  )
  minx <- max(0, normmean - 4 * normsd)
  maxx <- min(n, normmean + 4 * normsd)
  myy <- dhyper(floor(normmean), succ, fail, n) / 2

  if (lower.tail) {
    this.prob <- phyper(k, succ, fail, n)
    showprob <- format(this.prob, digits = 4)
    this.prob2 <- pnorm(k, normmean, normsd)
    showprob2 <- format(this.prob2, digits = 4)
    this.prob3 <- pnorm(k + 0.5, normmean, normsd)
    showprob3 <- format(this.prob3, digits = 4)
    withcorrect <- seq(0, k + 0.5, 0.001)
    probseq <- seq(0, k, 0.001)
    polygon(
      c(withcorrect, k + 0.5, 0),
      c(dnorm(withcorrect, normmean, normsd), 0, 0),
      col = "light grey"
    )
    polygon(
      c(probseq, k, 0),
      c(dnorm(probseq, normmean, normsd), 0, 0),
      col = "light blue"
    )
    lines(
      normseq,
      dnorm(normseq, normmean, normsd),
      lwd = 1,
      col = "light grey"
    )
    lines(0:k, dhyper(0:k, succ, fail, n), col = "red", type = "h", lwd = 2)
    text(
      (minx + normmean) / 4,
      myy,
      labels = bquote(atop(P(X <= .(k)), "=" ~ .(showprob))),
      col = "red"
    )
  }
  if (!lower.tail) {
    this.prob <- 1 - phyper(k - 1, succ, fail, n)
    showprob <- format(this.prob, digits = 4)
    this.prob2 <- pnorm(k, normmean, normsd, FALSE)
    showprob2 <- format(this.prob2, digits = 4)
    this.prob3 <- pnorm(k - 0.5, normmean, normsd, FALSE)
    showprob3 <- format(this.prob3, digits = 4)
    withcorrect <- seq(k - 0.5, n, 0.001)
    probseq <- seq(k, n, 0.001)
    polygon(
      c(withcorrect, n, k - 0.5),
      c(dnorm(withcorrect, normmean, normsd), 0, 0),
      col = "light grey"
    )
    polygon(
      c(probseq, n, k),
      c(dnorm(probseq, normmean, normsd), 0, 0),
      col = "light blue"
    )
    lines(normseq, dnorm(normseq, normmean, normsd), lwd = 1, col = "grey")
    lines(k:n, dhyper(k:n, succ, fail, n), col = "red", type = "h", lwd = 2)
    text(
      (maxx + normmean) * 9 / 16,
      myy,
      labels = bquote(atop(P(X >= .(k)), "=" ~ .(showprob))),
      pos = 1,
      col = "red"
    )
  }

  newtitle <- substitute(
    paste(
      "Hypergeometric (",
      N == x1,
      ", ",
      M == x2,
      ",",
      n == x3,
      "), Normal(",
      mean == x4,
      ",  ",
      SD == x5,
      ")"
    ),
    list(x1 = total, x2 = succ, x3 = n, x4 = normmean, x5 = normsd)
  )
  title(newtitle)
  full <- c(
    c(" hypergeometric:", showprob),
    c("\n normal approx:", showprob2),
    c("\n normal approx with continuity:", showprob3)
  )
  if (verbose) {
    cat(full, "\n")
  }
}

#' Hypergeometric p-value and Distribution
#'
#' @param k Number of successes of interest or difference in conditional proportions
#' @param total Total number of observations in the study
#' @param succ Overall number of successes
#' @param n Number of observations in group A
#' @param lower.tail Boolean for finding the probability above (FALSE) or
#'  below (TRUE) the inputted value (inclusive)
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return Tail probabilities from the hypergeometric distribution,
#'  hypergeometric distribution with the observed statistic and more extreme
#'  shaded.
#'
#' @export
#'
#' @examples
#' iscamhyperprob(1, 20, 5, 10, TRUE)
iscamhyperprob <- function(k, total, succ, n, lower.tail, verbose = TRUE) {
  if (.iscam_maybe_help(k, "iscamhyperprob")) {
    return(invisible())
  }

  old <- par(mar = c(4, 4, 2, 1))
  on.exit(par(old), add = TRUE)

  if (k < 1 && k > 0) {
    k <- round((k * (total - n) * n + succ * n) / total)
  }

  fail <- total - succ
  thisx <- max(0, n - fail):min(n, succ)
  .iscam_plot_discrete_distribution(
    x = thisx,
    prob_y = dhyper(thisx, succ, fail, n),
    x_label = "Number of Successes",
    y_label = "Probability"
  )

  if (lower.tail) {
    this.prob <- phyper(k, succ, fail, n)
    showprob <- format(this.prob, digits = 4)
    lines(0:k, dhyper(0:k, succ, fail, n), col = "red", type = "h", lwd = 2)
    xtext <- max(2, k - 0.5)
    text(
      xtext,
      dhyper(k, succ, fail, n),
      labels = bquote(atop(P(X <= .(k)), "=" ~ .(showprob))),
      pos = 3,
      col = "red"
    )
    if (verbose) {
      cat("Probability", k, "and below =", this.prob, "\n")
    }
  }
  if (!lower.tail) {
    this.prob <- 1 - phyper(k - 1, succ, fail, n)
    showprob <- format(this.prob, digits = 4)
    lines(k:n, dhyper(k:n, succ, fail, n), col = "red", type = "h", lwd = 2)
    # text(k, dhyper(k, succ, fail, n), labels=showprob, pos=4, col="red")
    xtext <- min(k + 0.5, succ - 2)
    text(
      xtext,
      dhyper(k, succ, fail, n),
      labels = bquote(atop(P(X >= .(k)), "=" ~ .(showprob))),
      pos = 3,
      col = "red"
    )
    if (verbose) {
      cat("Probability", k, "and above =", this.prob, "\n")
    }
  }
  newtitle <- substitute(
    paste("Hypergeometric (", N == x1, ", ", M == x2, ",", n == x3, ")"),
    list(x1 = total, x2 = succ, x3 = n)
  )
  title(newtitle)
  invisible(this.prob)
}
