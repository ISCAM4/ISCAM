.iscam_format_help <- function(lines) {
  lines <- lines |>
    gsub(".\b", "", x = _) |>
    gsub("[\u2018\u2019\u201c\u201d]", "'", x = _)
  formatted <- character(0)
  in_args <- FALSE

  for (line in lines) {
    if (in_args && grepl("^[A-Za-z][A-Za-z ]*:$", line)) {
      break
    }

    if (!in_args && grepl("^Arguments:\\s*$", line)) {
      in_args <- TRUE
      formatted <- c(formatted, line)
      next
    }

    if (in_args) {
      if (grepl("^\\s*$", line)) {
        next
      }
      match <- regexec("^\\s*([A-Za-z0-9_.]+)\\s*:\\s*(.*)$", line)
      parts <- regmatches(line, match)[[1]]
      if (length(parts) > 0) {
        formatted <- c(formatted, sprintf("     %s: %s", parts[2], parts[3]))
        next
      }
      trimmed <- sub("^\\s+", "", line)
      formatted <- c(
        formatted,
        if (nzchar(trimmed)) paste0("     ", trimmed) else ""
      )
      next
    }
  }

  sub("\\s+$", "", formatted)
}

.iscam_show_help <- function(topic) {
  pkg_man <- system.file("man", package = "ISCAM")
  pkg_rd <- if (nzchar(pkg_man)) {
    file.path(pkg_man, paste0(topic, ".Rd"))
  } else {
    ""
  }
  if (nzchar(pkg_rd) && file.exists(pkg_rd)) {
    capture.output(tools::Rd2txt(pkg_rd, out = "")) |>
      .iscam_format_help() |>
      paste(collapse = "\n") |>
      cat("\n", sep = "")
    return(invisible(TRUE))
  }

  pkg_path <- if ("ISCAM" %in% loadedNamespaces()) {
    getNamespaceInfo("ISCAM", "path")
  } else {
    NULL
  }

  rd_paths <- c(
    if (!is.null(pkg_path)) file.path(pkg_path, "man"),
    file.path(getwd(), "man")
  ) |>
    file.path(paste0(topic, ".Rd")) |>
    Filter(file.exists, x = _)
  rd_path <- rd_paths[1]

  if (!is.na(rd_path) && nzchar(rd_path)) {
    capture.output(tools::Rd2txt(rd_path, out = "")) |>
      .iscam_format_help() |>
      paste(collapse = "\n") |>
      cat("\n", sep = "")
    return(invisible(TRUE))
  }

  help_obj <- utils::help(topic, package = "ISCAM")
  if (length(help_obj) == 0) {
    cat("No documentation available for", topic, "\n")
    return(invisible(FALSE))
  }

  utils:::.getHelpFile(help_obj) |>
    tools::Rd2txt(out = "") |>
    capture.output() |>
    .iscam_format_help() |>
    paste(collapse = "\n") |>
    cat("\n", sep = "")
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
  invisible(TRUE)
}
