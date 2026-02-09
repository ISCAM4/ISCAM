.iscam_format_help <- function(lines) {
  lines <- gsub("[\u2018\u2019\u201c\u201d]", "'", gsub(".\b", "", lines))
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

.iscam_rd_to_lines <- function(rd) {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  tools::Rd2txt(rd, out = tmp)
  readLines(tmp, warn = FALSE)
}

.iscam_report_missing_help <- function(topic) {
  cat("No documentation available for", topic, "\n")
  invisible(FALSE)
}

.iscam_print_help <- function(lines) {
  help_text <- paste(.iscam_format_help(lines), collapse = "\n")
  cat(help_text, "\n", sep = "")
  invisible(TRUE)
}

.iscam_print_help_from_rd <- function(rd_path) {
  .iscam_print_help(.iscam_rd_to_lines(rd_path))
}

.iscam_working_rd_paths <- function(topic, pkg_path) {
  rd_paths <- c(
    if (!is.null(pkg_path)) file.path(pkg_path, "man"),
    file.path(getwd(), "man")
  )
  rd_paths <- file.path(rd_paths, paste0(topic, ".Rd"))
  Filter(file.exists, rd_paths)
}

.iscam_show_help <- function(topic) {
  pkg_man <- system.file("man", package = "ISCAM")
  pkg_rd <- if (nzchar(pkg_man)) {
    file.path(pkg_man, paste0(topic, ".Rd"))
  } else {
    ""
  }
  if (nzchar(pkg_rd) && file.exists(pkg_rd)) {
    return(.iscam_print_help_from_rd(pkg_rd))
  }

  pkg_path <- if ("ISCAM" %in% loadedNamespaces()) {
    getNamespaceInfo("ISCAM", "path")
  } else {
    NULL
  }

  rd_paths <- .iscam_working_rd_paths(topic, pkg_path)
  rd_path <- rd_paths[1]

  if (!is.na(rd_path) && nzchar(rd_path)) {
    return(.iscam_print_help_from_rd(rd_path))
  }

  help_obj <- utils::help(topic, package = "ISCAM")
  if (length(help_obj) == 0) {
    return(.iscam_report_missing_help(topic))
  }

  rd_db <- tools::Rd_db("ISCAM")
  rd_key <- intersect(c(paste0(topic, ".Rd"), topic), names(rd_db))

  if (length(rd_key) == 0) {
    return(.iscam_report_missing_help(topic))
  }

  .iscam_print_help_from_rd(rd_db[[rd_key[1]]])
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
