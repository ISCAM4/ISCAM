#' Overlay a Normal Density Function
#'
#' @param x
#' @param myxlab
#' @param mytitle
#' @param mynint
#'
#' @return
#' @export
#'
#' @examples
addnorm <- function(x, myxlab=NULL, mytitle = NULL,  mynint = NULL){
  Description = "iscamaddnorm(x, myxlab, mytitle, mynint) \n This function creates a histogram of the inputted variable \n and overlays a normal density function. Optional: Use myxlab to horizontal axis label and mytitle to add a title. mynint controls the number of bins"

  if(as.character(x[1])=="?") stop(Description)
  if(is.null(myxlab)) myxlab=c(names(x))
  if(is.null(mytitle)) mytitle=c("Histogram with normal curve")

  fullx = x[!is.na(x)]
  mean = mean(fullx); sd = sd(fullx)
  #print(sd)
  min = min(fullx, mean - 3*sd)
  max = max(fullx, mean + 3*sd)
  gran = (max-min)/1000

  par(mar=c(4, 3, 1, 1))
  myseq = seq(min, max, gran)
  myhist = hist(x, freq=FALSE, xlim=c(min, max), xlab = myxlab, main = mytitle, nclass = mynint)
  ymax=max(dnorm(myseq, mean, sd), myhist$density)
  #grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1)
  #hist(x, freq=FALSE, xlim=c(min,max),  ylim=c(0, ymax*1.05), main = "", xlab=myxlab, ylab="", yaxs="i", col="grey", add=T)
  abline(h=0, col="black")

  lines(myseq, dnorm(myseq, mean, sd), col = "red")
  #mtext(side=1, line=2, deparse(substitute(x)))
  mtext(side=2, line=2, "density")

}
