#' Overlays Normal Approximation onto Binomial
#'
#' `binomnorm` creates a binomial distribution of the given inputs and overlays a normal approximation.
#'
#' @param k number of successes of interest
#' @param n number of trials
#' @param prob success probability
#' @param direction "above", "below", or "two.sided"
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return A plot of the binomial distribution overlayed with the normal approximation
#'
#' @export
#'
#' @examples
#' iscambinomnorm(k = 10, n = 20, prob = 0.5, direction = "two.sided")
iscambinomnorm <- function(k, n, prob, direction, verbose = TRUE) {
  if (.iscam_maybe_help(k, "iscambinomnorm")) {
    return(invisible())
  }

  old <- par(mar = c(5, 3, 1, 1))
  on.exit(par(old), add = TRUE)
  thisx <- 0:n
  phat <- thisx / n
  minx <- max(0, n * prob - 4 * sqrt(prob * (1 - prob) * n))
  maxx <- max(k + 1, min(n, n * prob + 4 * sqrt(prob * (1 - prob) * n)))
  myy <- max(dbinom(floor(n * prob), n, prob))
  .iscam_plot_discrete_distribution(
    x = thisx,
    prob_y = dbinom(thisx, size = n, prob),
    xlim = c(minx, maxx),
    x_label = "Number of Successes (Proportion)",
    y_label = "Probability (Density)",
    x_line = 3
  )

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
  normseq <- seq(0, n, 0.001)
  lines(normseq, dnorm(normseq, normmean, normsd), col = "grey")
  if (direction == "below") {
    probseq <- seq(0, k, 0.001)
    withcorrect <- seq(0, k + 0.5, 0.001)
    tail_out <- .iscam_discrete_tail_probability(
      k = k,
      lower.tail = TRUE,
      cdf_lower = function(x) pbinom(x, n, prob),
      cdf_upper = function(x) 1 - pbinom(x - 1, n, prob),
      digits = 4
    )
    this.prob <- tail_out$prob
    showprob <- tail_out$showprob
    normprob <- pnorm(k, normmean, normsd)
    normprob2 <- pnorm(k + 0.5, normmean, normsd)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    polygon(
      c(withcorrect, k + 0.5, 0),
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
    .iscam_draw_discrete_tail_spikes(
      k = k,
      upper = n,
      lower.tail = TRUE,
      pmf_fn = function(x) dbinom(x, size = n, prob),
      col = "red",
      lwd = 2
    )
    text(
      minx,
      myy * 0.9,
      labels = .iscam_discrete_tail_label(
        k = k,
        showprob = showprob,
        lower.tail = TRUE
      ),
      col = "red",
      pos = 4
    )
  } else if (direction == "above") {
    tail_out <- .iscam_discrete_tail_probability(
      k = k,
      lower.tail = FALSE,
      cdf_lower = function(x) pbinom(x, n, prob),
      cdf_upper = function(x) 1 - pbinom(x - 1, n, prob),
      digits = 4
    )
    this.prob <- tail_out$prob
    showprob <- tail_out$showprob
    probseq <- seq(k, n, 0.001)
    withcorrect <- seq(k - 0.5, n, 0.001)
    normprob <- pnorm(k, normmean, normsd, lower.tail = FALSE)
    normprob2 <- pnorm(k - 0.5, normmean, normsd, lower.tail = FALSE)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    polygon(
      c(k - 0.5, withcorrect, n),
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
    .iscam_draw_discrete_tail_spikes(
      k = k,
      upper = n,
      lower.tail = FALSE,
      pmf_fn = function(x) dbinom(x, size = n, prob),
      col = "red",
      lwd = 2
    )
    text(
      maxx,
      myy * 0.9,
      labels = .iscam_discrete_tail_label(
        k = k,
        showprob = showprob,
        lower.tail = FALSE
      ),
      col = "red",
      pos = 2
    )
  } else if (direction == "two.sided") {
    if (k < normmean) {
      k1 <- k
      k2 <- floor(min(normmean - k + normmean, n))
      newvalue <- dbinom(k2, size = n, prob)
      if (newvalue <= dbinom(k1, size = n, prob) + 0.00001) {
        k2 <- k2
      } else {
        k2 <- k2 + 1
      }
    } else {
      k1 <- floor(min(normmean - (k - normmean), n))
      k2 <- k
      newvalue <- dbinom(k1, size = n, prob)
      if (newvalue <= dbinom(k, size = n, prob) + 0.00001) {
        k1 <- k1
      } else {
        k1 <- k1 - 1
      }
    }
    this.prob <- pbinom(k1, n, prob) +
      pbinom(k2 - 1, n, prob, lower.tail = FALSE)
    normprob <- pnorm(k1, normmean, normsd) +
      pnorm(k2, normmean, normsd, lower.tail = FALSE)
    normprob2 <- pnorm(k1 + 0.5, normmean, normsd) +
      pnorm(k2 - 0.5, normmean, normsd, lower.tail = FALSE)
    showprob <- format(this.prob, digits = 4)
    showprob2 <- format(normprob, digits = 4)
    showprob3 <- format(normprob2, digits = 4)
    probseq1 <- seq(0, k1, 0.001)
    probseq2 <- seq(k2, n, 0.001)
    withcorrect <- seq(0, k1 + 0.5, 0.001)
    withcorrect2 <- seq(k2 - 0.5, n, 0.001)
    polygon(
      c(withcorrect, k1 + 0.5, 0),
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
      c(k2 - 0.5, withcorrect2, n),
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
      myy * 0.85,
      labels = bquote(
        atop(
          P(X <= .(k1)) + P(X >= .(k2)),
          "=" ~ .(showprob)
        )
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
  if (verbose) {
    cat(full, "\n")
  }
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
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return A plot of the binomial distribution with the rejection region highlighted.
#'
#' @export
#'
#' @examples
#' iscambinompower(LOS = 0.05, n = 20, prob1 = 0.5, alternative = "less")
#'
#' iscambinompower(LOS = 0.05, n = 20, prob1 = 0.5, alternative = "greater", prob2 = 0.75)
#'
#' iscambinompower(LOS = 0.10, n = 30, prob1 = 0.4, alternative = "two.sided")
#'
#' iscambinompower(LOS = 0.10, n = 30, prob1 = 0.4, alternative = "two.sided", prob2 = 0.2)
iscambinompower <- function(
  LOS,
  n,
  prob1,
  alternative,
  prob2 = NULL,
  verbose = TRUE
) {
  if (.iscam_maybe_help(LOS, "iscambinompower")) {
    return(invisible())
  }

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
  old <- par(mfrow = c(2, 1))
  on.exit(par(old), add = TRUE)
  old <- par(mar = c(4, 3, 2, 2))
  on.exit(par(old), add = TRUE)
  .iscam_plot_binom_distribution(
    n = n,
    prob = prob1,
    xlim = c(minx, maxx),
    x_axis_label = "Number of Successes",
    y_axis_label = "Probability",
    add_title = FALSE
  )

  if (alternative == "less") {
    rr <- qbinom(LOS, n, prob1) - 1
    this.prob1 <- pbinom(rr, n, prob1)
    showprob1 <- format(this.prob1, digits = 4)
    lines(0:rr, dbinom(0:rr, size = n, prob1), col = "red", type = "h")
    text(
      (minx + n * prob1) / 2,
      myy1,
      labels = bquote(atop(P(X <= .(rr)), "=" ~ .(showprob1))),
      pos = 3,
      col = "red"
    )
    if (verbose) {
      cat("Null: Probability", rr, "and below =", this.prob1, "\n")
    }
  } else if (alternative == "greater") {
    rr <- qbinom(LOS, n, prob1, FALSE) + 1
    this.prob1 <- 1 - pbinom(rr - 1, n, prob1)
    showprob1 <- format(this.prob1, digits = 4)
    lines(rr:n, dbinom(rr:n, size = n, prob1), col = "red", type = "h")
    text(
      (maxx + n * prob1) / 2,
      myy1,
      labels = bquote(atop(P(X >= .(rr)), "=" ~ .(showprob1))),
      pos = 3,
      col = "red"
    )
    if (verbose) {
      cat("Null: Probability", rr, "and above =", this.prob1, "\n")
    }
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
      bquote(
        atop(
          P(X <= .(lowerrr)) + P(X >= .(upperrr)),
          atop("=" ~ .(showlowerprob1) + .(showupperprob1), "=" ~ .(showprob1))
        )
      ),
      pos = 3,
      col = "red"
    )
    if (verbose) {
      cat("Null: Probability in rejection region", showprob1, "\n")
    }
  } else {
    stop("Check input for alternative")
  }

  newtitle <- substitute(
    paste("Binomial (", n == x1, ", ", pi == x2, ") - null", ),
    list(x1 = n, x2 = prob1)
  )
  title(newtitle)

  if (!is.null(prob2)) {
    .iscam_plot_binom_distribution(
      n = n,
      prob = prob2,
      xlim = c(minx, maxx),
      x_axis_label = "Number of Successes",
      y_axis_label = "Probability",
      add_title = FALSE
    )
    myy2 <- dbinom(floor(n * prob2), n, prob2) / 2
    if (alternative == "less") {
      this.prob2 <- pbinom(rr, n, prob2)
      showprob2 <- format(this.prob2, digits = 4)
      lines(0:rr, dbinom(0:rr, size = n, prob2), col = "red", type = "h")
      text(
        (minx + n * prob2) / 2,
        myy2,
        labels = bquote(atop(P(X <= .(rr)), "=" ~ .(showprob2))),
        pos = 3,
        col = "red"
      )
      if (verbose) {
        cat("Alternative: Probability", rr, "and below =", this.prob2, "\n")
      }
    } else if (alternative == "greater") {
      this.prob2 <- 1 - pbinom(rr - 1, n, prob2)
      showprob2 <- format(this.prob2, digits = 4)
      lines(rr:n, dbinom(rr:n, size = n, prob2), col = "red", type = "h")
      text(
        (maxx + n * prob2) / 2,
        myy2,
        labels = bquote(atop(P(X >= .(rr)), "=" ~ .(showprob2))),
        pos = 3,
        col = "red"
      )
      if (verbose) {
        cat("Alternative: Probability", rr, "and above =", this.prob2, "\n")
      }
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
        labels = bquote(atop(
          P(X <= .(lowerrr)) + P(X >= .(upperrr)),
          "=" ~ .(showprob2)
        )),
        pos = 3,
        col = "red"
      )
      if (verbose) {
        cat("Alternative: Probability in rejection region", this.prob2, "\n")
      }
    }
    newtitle <- substitute(
      paste("Binomial (", n == x1, ", ", pi == x2, ") - alternative", ),
      list(x1 = n, x2 = prob2)
    )
    title(newtitle)
  }
  old <- par(mfrow = c(1, 1))
  on.exit(par(old), add = TRUE)
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
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return The probability of the binomial distribution along with a graph of the distribution.
#' @export
#'
#' @examples
#' iscambinomprob(k = 5, n = 20, prob = 0.4, lower.tail = TRUE)
#' iscambinomprob(k = 15, n = 30, prob = 0.3, lower.tail = FALSE)
#' iscambinomprob(k = 22, n = 25, prob = 0.9, lower.tail = TRUE)
iscambinomprob <- function(k, n, prob, lower.tail, verbose = TRUE) {
  if (.iscam_maybe_help(k, "iscambinomprob")) {
    return(invisible())
  }

  if (prob < 0 || prob > 1) {
    stop("Error: `prob` (probability) must be a numeric value between 0 and 1.")
  }

  old <- par(mar = c(4, 3, 2, 2))
  on.exit(par(old), add = TRUE)
  limits <- .iscam_binom_limits(n, prob, include_upper = k + 1)
  minx <- limits[1]
  maxx <- limits[2]
  myy <- dbinom(floor(n * prob), n, prob)
  .iscam_plot_binom_distribution(
    n = n,
    prob = prob,
    xlim = c(minx, maxx),
    x_axis_label = "Number of Successes"
  )

  tail_out <- .iscam_discrete_tail_probability(
    k = k,
    lower.tail = lower.tail,
    cdf_lower = function(x) pbinom(x, n, prob),
    cdf_upper = function(x) 1 - pbinom(x - 1, n, prob),
    digits = 4
  )
  this.prob <- tail_out$prob
  showprob <- tail_out$showprob
  .iscam_draw_discrete_tail_spikes(
    k = k,
    upper = n,
    lower.tail = lower.tail,
    pmf_fn = function(x) dbinom(x, size = n, prob),
    col = "red",
    lwd = 2
  )
  x_text <- if (lower.tail) {
    (minx + n * prob) / 4
  } else {
    (maxx + n * prob) * 9 / 16
  }
  text(
    x_text,
    myy,
    labels = .iscam_discrete_tail_label(
      k = k,
      showprob = showprob,
      lower.tail = lower.tail
    ),
    pos = 1,
    col = "red"
  )
  .iscam_print_discrete_tail_probability(
    verbose = verbose,
    k = k,
    prob = this.prob,
    lower.tail = lower.tail
  )

  invisible(this.prob)
}

#' Exact Binomial Test
#'
#' `binomtest` calculates performs an exact binomial test and graphs the
#'    binomial distribution and/or binomial confidence interval.
#'
#' @param observed The observed number of successes or sample proportion (assumed
#'    to be proportion if value less than one.)
#' @param n number of trials.
#' @param hypothesized hypothesized probability of success.
#' @param alternative "less", "greater", or "two.sided"
#' @param conf.level Confidence level for a two-sided confidence interval.
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return a list of the p-value along with lower and upper bound for the calculated confidence interval.
#' @export
#'
#' @examples
#'
#' iscambinomtest(
#'   observed = 17,
#'   n = 25,
#'   hypothesized = 0.5,
#'   alternative = "greater"
#' )
#'
#' iscambinomtest(
#'   observed = 12,
#'   n = 80,
#'   hypothesized = 0.10,
#'   alternative = "two.sided",
#'   conf.level = 0.95
#' )
#'
#' iscambinomtest(
#'   observed = 0.14,
#'   n = 100,
#'   hypothesized = 0.20,
#'   alternative = "less"
#' )
#'
#' iscambinomtest(observed = 17, n = 25, conf.level = 0.95)
#'
#' iscambinomtest(observed = 12, n = 80, conf.level = c(0.90, 0.95, 0.99))
iscambinomtest <- function(
  observed,
  n,
  hypothesized = NULL,
  alternative,
  conf.level = NULL,
  verbose = TRUE
) {
  if (.iscam_maybe_help(observed, "iscambinomtest")) {
    return(invisible())
  }

  old <- par(mar = c(4, 3, 2, 2))
  on.exit(par(old), add = TRUE)

  observed <- .iscam_as_count(observed, n)
  pvalue <- NULL
  if (!is.null(hypothesized)) {
    limits <- .iscam_binom_limits(n, hypothesized, include_upper = observed + 1)
    minx <- limits[1]
    maxx <- limits[2]
    myy <- max(dbinom(floor(n * hypothesized), n, hypothesized)) * 0.9
    .iscam_plot_binom_distribution(
      n = n,
      prob = hypothesized,
      xlim = c(minx, maxx),
      x_axis_label = "Number of Successes"
    )
    pvalue <- .iscam_plot_exact_binom_region(
      observed = observed,
      n = n,
      hypothesized = hypothesized,
      alternative = alternative,
      minx = minx,
      maxx = maxx,
      myy = myy
    )
    pvalue <- signif(pvalue, 5)
    abline(v = 0, col = "gray")
  }
  if (verbose) {
    cat("\nExact Binomial Test\n\n")
  }
  statistic <- signif(observed / n, 4)
  if (verbose) {
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
  }

  if (!is.null(hypothesized)) {
    .iscam_print_hypotheses(
      verbose = verbose,
      null_name = "pi",
      alt_name = "pi",
      hypothesized = hypothesized,
      alternative = alternative,
      include_not_equal = FALSE
    )
    if (verbose) {
      cat(paste("p-value:", pvalue, sep = " "), "\n")
    }
  }
  lower1 <- NULL
  upper1 <- NULL
  if (!is.null(conf.level)) {
    conf.level <- .iscam_normalize_conf_levels(conf.level)
    for (k in seq_along(conf.level)) {
      ci_bounds <- signif(.iscam_binom_ci(observed, n, conf.level[k]), 5)
      CINT <- c(ci_bounds["lower"], ",", ci_bounds["upper"])
      if (verbose) {
        cat(
          100 * conf.level[k],
          "% Confidence interval for pi: (",
          CINT,
          ") \n"
        )
      }
      lower1[k] <- as.numeric(CINT[1])
      upper1[k] <- as.numeric(CINT[3])
    }
  }
  old <- par(mar = c(4, 2, 1.5, 0.5), mfrow = c(3, 1))
  on.exit(par(old), add = TRUE)
  if (length(conf.level) > 1) {
    old <- par(mar = c(4, 2, 1.5, 0.4), mfrow = c(length(conf.level), 1))
    on.exit(par(old), add = TRUE)
  }

  if (is.null(hypothesized)) {
    statistic <- observed / n
    SDphat <- sqrt(statistic * (1 - statistic) / n)
    min <- statistic - 4 * SDphat
    max <- statistic + 4 * SDphat
    minx <- as.integer(max(0, min * n))
    maxx <- as.integer(min(n, max * n))

    if (length(conf.level) == 1) {
      myxlab <- .iscam_binom_title(n, signif(lower1[1], 4))
      plot(
        seq(minx, maxx),
        dbinom(seq(minx, maxx), size = n, prob = lower1[1]),
        xlab = "  ",
        ylab = " ",
        type = "h",
        xlim = c(minx, maxx)
      )
      mtext("Number of successes", side = 1, line = 1.75, adj = 0.5, cex = 0.75)
      title(myxlab)
      lines(
        observed:n,
        dbinom(observed:n, size = n, prob = lower1[1]),
        col = "red",
        type = "h"
      )

      myxlab <- .iscam_binom_title(n, signif(upper1[1], 4))
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

      mtext("Number of successes", side = 1, line = 1.75, adj = 0.5, cex = 0.75)
      title(myxlab)
    }

    for (k in seq_along(conf.level)) {
      .iscam_plot_ci_strip(
        min_x = min,
        statistic = statistic,
        max_x = max,
        lower = lower1[k],
        upper = upper1[k],
        x_label = "process probability",
        ci_label = paste(conf.level[k] * 100, "% CI:")
      )
    }
  }

  old <- par(mfrow = c(1, 1))
  on.exit(par(old), add = TRUE)
  invisible(list("pvalue" = pvalue, "lower" = lower1, "upper" = upper1))
}

#' Inverse Binomial Probability
#'
#' @param alpha The probability of interest.
#' @param n The number of trials.
#' @param prob The probability of success.
#' @param lower.tail Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return numeric which achieves at most the stated probability
#' @export
#'
#' @examples
#' iscaminvbinom(alpha = 0.05, n = 30, prob = 0.5, lower.tail = TRUE)
#'
#' iscaminvbinom(alpha = 0.05, n = 30, prob = 0.5, lower.tail = FALSE)
#'
#' iscaminvbinom(alpha = 0.01, n = 60, prob = 0.10, lower.tail = FALSE)
iscaminvbinom <- function(alpha, n, prob, lower.tail, verbose = TRUE) {
  if (.iscam_maybe_help(alpha, "iscaminvbinom")) {
    return(invisible())
  }

  old <- par(mar = c(4, 3, 2, 2))
  on.exit(par(old), add = TRUE)

  limits <- .iscam_binom_limits(n, prob)
  minx <- limits[1]
  maxx <- limits[2]
  myy <- dbinom(floor(n * prob), n, prob) / 2
  .iscam_plot_binom_distribution(
    n = n,
    prob = prob,
    xlim = c(minx, maxx),
    x_axis_label = "X = Number of Successes"
  )
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
      labels = bquote(
        atop(
          P(X <= .(answer)) == .(actualprob),
          P(X <= .(answer + 1)) ==
            .(format(
              pbinom(answer + 1, n, prob, lower.tail = lower.tail),
              digits = 4
            ))
        )
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
    if (verbose) {
      cat(
        "The observation with at most",
        alpha,
        "probability at or below is",
        answer,
        "\n"
      )
    }
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
      labels = bquote(
        atop(
          P(X >= .(answer)) == .(actualprob),
          P(X >= .(answer - 1)) ==
            .(format(
              pbinom(answer - 2, n, prob, lower.tail = lower.tail),
              digits = 4
            ))
        )
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
    if (verbose) {
      cat(
        "The observation with at most",
        alpha,
        "probability at or above is",
        answer,
        "\n"
      )
    }
  }
  invisible(answer)
}

#' Plot a binomial mass function with shared ISCAM styling
#'
#' Internal plotting utility for binomial-family functions.
#'
#' @param n Number of trials.
#' @param prob Success probability.
#' @param xlim X-axis limits.
#' @param x_axis_label X-axis label.
#' @param y_axis_label Y-axis label.
#' @param add_title Logical; if `TRUE`, draw the standard binomial title.
#'
#' @keywords internal
#' @noRd
.iscam_plot_binom_distribution <- function(
  n,
  prob,
  xlim,
  x_axis_label = "Number of Successes",
  y_axis_label = "Probability",
  add_title = TRUE
) {
  thisx <- 0:n
  .iscam_plot_discrete_distribution(
    x = thisx,
    prob_y = dbinom(thisx, size = n, prob),
    xlim = xlim,
    x_label = x_axis_label,
    y_label = y_axis_label,
    lwd = 2
  )
  if (add_title) {
    title(.iscam_binom_title(n, prob))
  }
}

#' Draw the highlighted region for an exact binomial test
#'
#' Computes and draws the one- or two-sided exact test region and returns the
#' corresponding p-value.
#'
#' @param observed Observed successes.
#' @param n Number of trials.
#' @param hypothesized Null success probability.
#' @param alternative Tail direction (`"less"`, `"greater"`, `"two.sided"`).
#' @param minx Left x-position for annotation.
#' @param maxx Right x-position for annotation.
#' @param myy Y-position for annotation.
#'
#' @return Numeric p-value for the highlighted region.
#'
#' @keywords internal
#' @noRd
.iscam_plot_exact_binom_region <- function(
  observed,
  n,
  hypothesized,
  alternative,
  minx,
  maxx,
  myy
) {
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
    return(pvalue)
  }

  if (alternative == "greater") {
    pvalue <- pbinom(observed - 1, size = n, prob = hypothesized, FALSE)
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
    return(pvalue)
  }

  xvals <- 0:n
  probabilities <- dbinom(xvals, size = n, prob = hypothesized)
  cutoff <- dbinom(observed, size = n, prob = hypothesized) + 0.00001
  keep <- probabilities <= cutoff
  pvalue <- sum(probabilities[keep])
  lines(
    xvals[keep],
    probabilities[keep],
    col = "red",
    type = "h",
    lwd = 2
  )
  text(
    minx,
    myy,
    labels = paste("two-sided p-value:\n", signif(pvalue, 4)),
    pos = 4,
    col = "red"
  )
  pvalue
}
