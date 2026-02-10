capture_plot_result <- function(expr) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit(
    {
      grDevices::dev.off()
      unlink(tmp)
    },
    add = TRUE
  )
  value <- NULL
  output <- capture.output({
    value <- withVisible(force(expr))
  })
  list(output = output, value = value$value, visible = value$visible)
}

collapse_output <- function(lines) {
  paste(lines, collapse = "\n")
}

clean_help_output <- function(lines) {
  cleaned <- gsub(".\x08", "", lines)
  cleaned <- gsub("[\u2018\u2019\u201c\u201d]", "'", cleaned)
  sub("\\s+$", "", cleaned)
}

capture_help_output <- function(expr) {
  clean_help_output(capture.output(expr))
}

expect_plot_vdiffr <- function(title, code) {
  code_expr <- substitute(code)
  eval_env <- parent.frame()
  vdiffr::expect_doppelganger(
    title,
    function() {
      eval(code_expr, envir = eval_env)
    }
  )
}

extract_help_section <- function(lines, section) {
  header <- paste0(section, ":")
  start <- match(header, lines)
  if (is.na(start)) {
    return(character(0))
  }
  end <- start + 1
  while (end <= length(lines) && !grepl("^[A-Za-z][A-Za-z ]*:$", lines[end])) {
    end <- end + 1
  }
  lines[start:(end - 1)]
}
