#' Chi-Square Probability
#'
#' `chisqrprob` returns the upper tail probability for the given chi-square
#'  statistic and degress of freedom.
#'
#' @param x_val the value of the chi-square statistic.
#' @param df the degrees of freedom.
#'
#' @return The upper tail probability for the chi-square distribution, and a
#'  plot of the chi-square distribution with the statistic and more extreme
#'  shaded.
#'
#' @importFrom stats dchisq pchisq
#'
#' @export
#'
#' @examples
#' chisqrProb(5, 3)
chisqrprob <- function(x_val, df) {
  withr::local_par(mar = c(4, 4, 2, 1))

  minx <- 0
  maxx <- max(20, x_val, df)
  this_x <- seq(minx, maxx, .001)
  plot(
    this_x,
    dchisq(this_x, df),
    xlim = c(minx, maxx),
    type = "l",
    panel.first = grid(),
    xlab = "",
    ylab = ""
  )
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 2, "chi-square values")
  mtext(side = 2, line = 2, "density")

  prob_seq <- seq(min(x_val, maxx), maxx, .001)
  show_prob <- format(pchisq(x_val, df, lower.tail = FALSE), digits = 4)
  polygon(
    c(min(maxx, x_val), prob_seq, maxx),
    c(0, dchisq(prob_seq, df), 0),
    col = "red",
    border = "red"
  )
  text(
    min(x_val, maxx * .9),
    dchisq(x_val, df),
    labels = paste("P(X \\u2265", x_val, ") \\n =", show_prob),
    col = "red",
    pos = 3
  )
  title(substitute(paste("Chi-Square(\", df == x3, \")"), list(x3 = df)))
  cat(c("probability:", show_prob), "\\n")
  invisible(show_prob)
}
