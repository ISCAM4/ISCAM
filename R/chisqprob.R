#' Chi-Square Probability
#'
#' `chisqrprob` returns the upper tail probability for the given chi-square
#'  statistic and degrees of freedom.
#'
#' @param xval the value of the chi-square statistic.
#' @param df the degrees of freedom.
#' @param verbose Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages
#'
#' @return The upper tail probability for the chi-square distribution, and a
#'  plot of the chi-square distribution with the statistic and more extreme
#'  shaded.
#'
#' @export
#'
#' @examples
#' iscamchisqprob(5, 3)
iscamchisqprob <- function(xval, df, verbose = TRUE) {
  if (.iscam_maybe_help(xval, "iscamchisqprob")) {
    return(invisible())
  }

  old <- par(mar = c(4, 4, 2, 1))
  on.exit(par(old), add = TRUE)

  minx <- 0
  maxx <- max(20, xval, df)
  this_x <- seq(minx, maxx, 0.001)
  .iscam_plot_continuous_distribution(
    x = this_x,
    density_y = dchisq(this_x, df),
    xlim = c(minx, maxx),
    x_label = "chi-square values",
    y_label = "density"
  )

  prob_seq <- seq(min(xval, maxx), maxx, 0.001)
  show_prob <- format(pchisq(xval, df, lower.tail = FALSE), digits = 4)
  polygon(
    c(min(maxx, xval), prob_seq, maxx),
    c(0, dchisq(prob_seq, df), 0),
    col = "red",
    border = "red"
  )
  text(
    min(xval, maxx * 0.9),
    dchisq(xval, df),
    labels = bquote(atop(P(X >= .(xval)), "=" ~ .(show_prob))),
    col = "red",
    pos = 3
  )
  title(substitute(paste("Chi-Square (df = ", x, ")"), list(x = df)))
  .iscam_print_probability(verbose, show_prob)
  invisible(show_prob)
}
