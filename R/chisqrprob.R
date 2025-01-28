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
#' @importFrom stats dchisq pchisq
#'
#' @export
#'
#' @examples
chisqrprob <- function(xval, df) {
  Description <- "iscamchisqprob(xval, df)\n This function calculations the upper tail probability for the chi-square distribution \n"
  if (as.character(xval) == "?") stop(Description)
  withr::local_par(mar = c(4, 4, 2, 1))

  minx <- 0
  maxx <- max(20, xval, df)
  thisx <- seq(minx, maxx, .001)
  plot(thisx, dchisq(thisx, df), xlim = c(minx, maxx), type = "l", panel.first = grid(), xlab = "", ylab = "")
  abline(h = 0, col = "gray")
  mtext(side = 1, line = 2, "chi-square values")
  mtext(side = 2, line = 2, "density")

  probseq <- seq(min(xval, maxx), maxx, .001)
  chisqprob <- pchisq(xval, df, lower.tail = FALSE)
  showprob <- format(chisqprob, digits = 4)
  polygon(c(min(maxx, xval), probseq, maxx), c(0, dchisq(probseq, df), 0), col = "red", border = "red")
  text(min(xval, maxx * .9), dchisq(xval, df), labels = paste("P(X \u2265", xval, ") \n =", showprob), col = "red", pos = 3)
  newtitle <- substitute(paste("Chi-Square(", df == x3, ")"), list(x3 = df))
  title(newtitle)
  cat(c("probability:", showprob), "\n")
  invisible(list("probability" = showprob))
}
