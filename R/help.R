.iscam_show_help <- function(topic) {
  pkg_path <- NULL
  if ("ISCAM" %in% loadedNamespaces()) {
    pkg_path <- getNamespaceInfo("ISCAM", "path")
  }

  rd_candidates <- character(0)
  if (!is.null(pkg_path)) {
    rd_candidates <- c(rd_candidates, file.path(pkg_path, "man"))
  }
  rd_candidates <- c(rd_candidates, file.path(getwd(), "man"))

  rd_candidates <- file.path(rd_candidates, paste0(topic, ".Rd"))
  rd_path <- rd_candidates[file.exists(rd_candidates)][1]
  if (!is.na(rd_path) && nzchar(rd_path)) {
    tools::Rd2txt(rd_path, out = "")
    return(invisible(TRUE))
  }

  help_obj <- utils::help(topic, package = "ISCAM")
  if (length(help_obj) == 0) {
    cat("No documentation available for", topic, "\n")
    return(invisible(FALSE))
  }

  rd_file <- utils:::.getHelpFile(help_obj)
  tools::Rd2txt(rd_file, out = "")
  invisible(TRUE)
}

.iscam_maybe_help <- function(first_arg, topic = NULL) {
  if (!is.character(first_arg) || length(first_arg) != 1 || first_arg != "?") {
    return(FALSE)
  }

  if (is.null(topic)) {
    topic <- deparse(sys.call(sys.parent())[[1]])
    topic <- sub(".*::", "", topic)
  }

  .iscam_show_help(topic)
  TRUE
}
