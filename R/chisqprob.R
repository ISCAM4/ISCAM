#' Chi-Square Probability
#'
#' `chisqrprob` returns the upper tail probability for the given chi-square
#'  statistic and degress of freedom.
#'
#' @param xval the value of the chi-square statistic.
#' @param df the degrees of freedom.
#'
#' @return The upper tail probability for the chi-square distribution, and a
#'  plot of the chi-square distribution with the statistic and more extreme
#'  shaded.
#'
#' @export
#'
#' @examples
#' iscamchisqprob(5, 3)
iscamchisqprob <- function(xval, df) {
  old <- par(mar = c(4, 4, 2, 1))
  on.exit(par(old), add = TRUE)

  minx <- 0
  maxx <- max(20, xval, df)
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

  prob_seq <- seq(min(xval, maxx), maxx, .001)
  show_prob <- format(pchisq(xval, df, lower.tail = FALSE), digits = 4)
  polygon(
    c(min(maxx, xval), prob_seq, maxx),
    c(0, dchisq(prob_seq, df), 0),
    col = "red",
    border = "red"
  )
  text(
    min(xval, maxx * .9),
    dchisq(xval, df),
    labels = paste0("P(X \u2265 ", xval, ") \n = ", show_prob),
    col = "red",
    pos = 3
  )
  title(substitute(paste("Chi-Square (df = ", x, ")"), list(x = df)))
  cat(c("probability:", show_prob), "\n")
  invisible(show_prob)
}
