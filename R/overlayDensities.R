#' Overlay an Exponential Density Function on Histogram
#'
#' `addexp` creates a histogram of `x` and overlays an exponential density
#'    function with \eqn{\lambda = \frac{1}{mean}}.
#'
#' @param x A numeric vector representing the data to be plotted.
#'
#' @returns A histogram of x overlayed with an exponential density function.
#'
#' @export
#'
#' @examples
addexp <- function(x) {
  Description <- "iscamaddexp(x) \n This function creates a histogram of the inputted variable \n and overlays an exponetial density function with lambda = 1/mean."

  if (as.character(x[1]) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(4, 3, 1, 1))

  min <- 0
  max <- max(x)
  myseq <- seq(min, max, .001)
  ymax <- max(dexp(myseq, 1 / mean(x)))

  # hist(x, freq=FALSE, xlab = deparse(substitute(x))
  hist(
    x,
    freq = FALSE,
    xlim = c(min, max),
    ylim = c(0, ymax * 1.1),
    yaxs = "i",
    col = "grey",
    add = F,
    main = "",
    xlab = ""
  )
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(x, freq = FALSE, xlab = deparse(substitute(x)), col = "grey", add = T)
  # hist(x, freq=FALSE, xlim=c(min,max),  ylim=c(0, ymax*1.1), yaxs="i", col="grey", add=T)

  lines(myseq, dexp(myseq, 1 / mean(x)), col = "red")
  abline(h = 0, col = "black")
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}


#' Overlay a Log Normal Density Function on Histogram
#'
#' `addlnorm` creates a histogram of `x` and overlays a log normal density function.
#'
#' @param x A numeric vector representing the data to be plotted.
#'
#' @returns A histogram of x overlayed with an log normal density function.
#'
#' @export
#'
#' @examples
addlnorm <- function(x) {
  # TODO Check this definition?
  Description <- "iscamaddlnorm(x) \n This function creates a histogram of the inputted variable \n and overlays an exponetial density function."

  if (as.character(x[1]) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(4, 3, 1, 1))
  min <- 0
  max <- max(x)
  myseq <- seq(min, max, .001)
  ymax <- max(dlnorm(myseq, mean(log(x)), sd(log(x))))

  hist(
    x,
    freq = FALSE,
    xlab = "",
    col = "grey",
    xlim = c(min, max),
    ylim = c(0, ymax * 1.05),
    yaxs = "i",
    add = F,
    main = ""
  )
  # hist(x, freq=FALSE, xlab="", col="grey", add=F)
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(
    x,
    freq = FALSE,
    xlab = "",
    col = "grey",
    xlim = c(min, max),
    ylim = c(0, ymax * 1.05),
    yaxs = "i",
    add = T
  )

  abline(h = 0, col = "black")
  lines(myseq, dlnorm(myseq, mean(log(x)), sd(log(x))), col = "red")
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}

#' Overlay a Normal Density Function on Histogram
#'
#' `addnorm` creates a histogram of `x` and overlays a normal density function.
#'    Optional: Use myxlab to label the x-axis and mytitle to add a title.
#'    mynint controls the number of bins
#'
#' @param x A numeric vector representing the data to be plotted.
#' @param myxlab What to label the x-axis.
#' @param mytitle What to title the graph.
#' @param mynint Number of bins.
#'
#' @returns A histogram of x overlayed with an normal density function.
#'
#' @export
#'
#' @examples
addnorm <- function(
  x,
  myxlab = names(x),
  mytitle = "Histogram with normal curve",
  mynint = NULL
) {
  # TODO why don't all "add" functions have label params? Use "..."

  fullx <- x[!is.na(x)]
  mean <- mean(fullx)
  sd <- sd(fullx)
  # print(sd)
  min <- min(fullx, mean - 3 * sd)
  max <- max(fullx, mean + 3 * sd)
  gran <- (max - min) / 1000

  withr::local_par(mar = c(4, 3, 1, 1))
  myseq <- seq(min, max, gran)
  myhist <- hist(
    x,
    freq = FALSE,
    xlim = c(min, max),
    xlab = myxlab,
    main = mytitle,
    nclass = mynint
  )
  ymax <- max(dnorm(myseq, mean, sd), myhist$density)
  # grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  # hist(x, freq=FALSE, xlim=c(min,max),  ylim=c(0, ymax*1.05), main = "", xlab=myxlab, ylab="", yaxs="i", col="grey", add=T)
  abline(h = 0, col = "black")

  lines(myseq, dnorm(myseq, mean, sd), col = "red")
  # mtext(side=1, line=2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}

#' Overlay a t Density Function on Histogram
#'
#' @param x A numeric vector representing the data to be plotted.
#' @param df A numeric value representing the degrees of freedom of `x`.
#'
#' @returns A histogram of x overlayed with an t density function.
#'
#' @export
#'
#' @examples
addt <- function(x, df) {
  Description <- "iscamaddt(x, df) \n This function creates a histogram of the inputted variable \n and overlays a t density function with df degrees of freedom."

  if (as.character(x[1]) == "?") {
    stop(Description)
  }
  withr::local_par(mar = c(4, 3, 1, 1))

  min <- min(x, mean(x) - 3 * sd(x))
  max <- max(x, mean(x) + 3 * sd(x))
  myseq <- seq(min, max, .001)
  myhist <- hist(x, freq = FALSE, xlim = c(min, max))
  ymax <- max(dt(myseq, df), myhist$density)
  hist(
    x,
    freq = FALSE,
    xlim = c(min, max),
    ylim = c(0, ymax * 1.05),
    main = "",
    xlab = "",
    ylab = "",
    yaxs = "i",
    col = "grey",
    add = F
  )
  # abline(h=seq(0,ymax, ymax/4), lty=3, col="light grey")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(
    x,
    freq = FALSE,
    xlim = c(min, max),
    ylim = c(0, ymax * 1.05),
    main = "",
    xlab = "",
    ylab = "",
    yaxs = "i",
    col = "grey",
    add = T
  )
  lines(myseq, dt(myseq, df), col = "red")
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}

#' Overlay a t Density Function & a Normal Density Function on Histogram
#'
#' @param x A numeric vector representing the data to be plotted.
#' @param df A numeric value representing the degrees of freedom of `x`.
#'
#' @returns A histogram of x overlayed with an t density function and a normal density function.
#'
#' @export
#'
#' @examples
addtnorm <- function(x, df) {
  Description <- "iscamaddt(x, df) \n This function creates a histogram of the inputted variable \n and overlays a t density function with df degrees of freedom."

  if (as.character(x[1]) == "?") {
    stop(Description)
  }

  withr::local_par(mar = c(4, 3, 1, 1))

  min <- min(x, mean(x) - 3 * sd(x))
  max <- max(x, mean(x) + 3 * sd(x))
  myseq <- seq(min, max, .001)
  myhist <- hist(x, freq = FALSE, xlim = c(min, max))
  ymax <- max(dt(myseq, df), myhist$density)
  hist(
    x,
    freq = FALSE,
    xlim = c(min, max),
    ylim = c(0, ymax * 1.05),
    main = "",
    xlab = "",
    ylab = "",
    yaxs = "i",
    col = "grey",
    add = F
  )
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  hist(
    x,
    freq = FALSE,
    xlim = c(min, max),
    ylim = c(0, ymax * 1.05),
    main = "",
    xlab = "",
    ylab = "",
    yaxs = "i",
    col = "grey",
    add = T
  )
  lines(myseq, dnorm(myseq, 0, 1), col = 3)
  lines(myseq, dt(myseq, df), col = 2)
  legend("topleft", c("t", "normal"), text.col = c(2, 3))
  mtext(side = 1, line = 2, deparse(substitute(x)))
  mtext(side = 2, line = 2, "density")
}
