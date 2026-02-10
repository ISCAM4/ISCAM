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

  tail_out <- .iscam_discrete_tail_probability(
    k = k,
    lower.tail = lower.tail,
    cdf_lower = function(x) phyper(x, succ, fail, n),
    cdf_upper = function(x) 1 - phyper(x - 1, succ, fail, n),
    digits = 4
  )
  tail_regions <- .iscam_discrete_tail_regions(
    k = k,
    lower_bound = 0,
    upper_bound = n,
    lower.tail = lower.tail
  )
  norm_out <- .iscam_normal_tail_probs_with_cc(
    k = k,
    mean = normmean,
    sd = normsd,
    lower.tail = lower.tail,
    digits = 4
  )
  this.prob <- tail_out$prob
  showprob <- tail_out$showprob
  showprob2 <- norm_out$showprob
  showprob3 <- norm_out$showprob_corrected

  if (lower.tail) {
    polygon(
      c(tail_regions$corrected_seq, tail_regions$corrected_cutoff, 0),
      c(dnorm(tail_regions$corrected_seq, normmean, normsd), 0, 0),
      col = "light grey"
    )
    polygon(
      c(tail_regions$tail_seq, k, 0),
      c(dnorm(tail_regions$tail_seq, normmean, normsd), 0, 0),
      col = "light blue"
    )
  } else {
    polygon(
      c(tail_regions$corrected_seq, n, tail_regions$corrected_cutoff),
      c(dnorm(tail_regions$corrected_seq, normmean, normsd), 0, 0),
      col = "light grey"
    )
    polygon(
      c(tail_regions$tail_seq, n, k),
      c(dnorm(tail_regions$tail_seq, normmean, normsd), 0, 0),
      col = "light blue"
    )
  }
  lines(
    normseq,
    dnorm(normseq, normmean, normsd),
    lwd = 1,
    col = .iscam_tail_choice(lower.tail, "light grey", "grey")
  )
  .iscam_draw_discrete_tail_spikes(
    k = k,
    upper = n,
    lower.tail = lower.tail,
    pmf_fn = function(x) dhyper(x, succ, fail, n),
    col = "red",
    lwd = 2
  )
  if (lower.tail) {
    text(
      (minx + normmean) / 4,
      myy,
      labels = .iscam_discrete_tail_label(
        k = k,
        showprob = showprob,
        lower.tail = TRUE
      ),
      col = "red"
    )
  } else {
    text(
      (maxx + normmean) * 9 / 16,
      myy,
      labels = .iscam_discrete_tail_label(
        k = k,
        showprob = showprob,
        lower.tail = FALSE
      ),
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

  tail_out <- .iscam_discrete_tail_probability(
    k = k,
    lower.tail = lower.tail,
    cdf_lower = function(x) phyper(x, succ, fail, n),
    cdf_upper = function(x) 1 - phyper(x - 1, succ, fail, n),
    digits = 4
  )
  this.prob <- tail_out$prob
  showprob <- tail_out$showprob
  .iscam_draw_discrete_tail_spikes(
    k = k,
    upper = n,
    lower.tail = lower.tail,
    pmf_fn = function(x) dhyper(x, succ, fail, n),
    col = "red",
    lwd = 2
  )
  xtext <- if (lower.tail) {
    max(2, k - 0.5)
  } else {
    min(k + 0.5, succ - 2)
  }
  text(
    xtext,
    dhyper(k, succ, fail, n),
    labels = .iscam_discrete_tail_label(
      k = k,
      showprob = showprob,
      lower.tail = lower.tail
    ),
    pos = 3,
    col = "red"
  )
  .iscam_print_discrete_tail_probability(
    verbose = verbose,
    k = k,
    prob = this.prob,
    lower.tail = lower.tail
  )

  newtitle <- substitute(
    paste("Hypergeometric (", N == x1, ", ", M == x2, ",", n == x3, ")"),
    list(x1 = total, x2 = succ, x3 = n)
  )
  title(newtitle)
  invisible(this.prob)
}
