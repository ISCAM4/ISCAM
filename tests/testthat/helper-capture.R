capture_plot_result <- function(expr) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  value <- NULL
  output <- capture.output({
    value <- withVisible(force(expr))
  })
  list(output = output, value = value$value, visible = value$visible)
}

collapse_output <- function(lines) {
  paste(lines, collapse = "\n")
}
