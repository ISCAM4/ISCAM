.iscam_format_help <- function(lines) {
  lines <- gsub(".\b", "", lines)
  lines <- gsub("[\u2018\u2019\u201c\u201d]", "'", lines)
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
        formatted <- c(formatted, line)
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

.iscam_help_output <- function(rd_path) {
  output <- capture.output(tools::Rd2txt(rd_path, out = ""))
  .iscam_format_help(output)
}

.iscam_show_help <- function(topic) {
  pkg_man <- system.file("man", package = "ISCAM")
  if (nzchar(pkg_man)) {
    pkg_rd <- file.path(pkg_man, paste0(topic, ".Rd"))
    if (file.exists(pkg_rd)) {
      cat(paste(.iscam_help_output(pkg_rd), collapse = "\n"), "\n", sep = "")
      return(invisible(TRUE))
    }
  }

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
    cat(paste(.iscam_help_output(rd_path), collapse = "\n"), "\n", sep = "")
    return(invisible(TRUE))
  }

  help_obj <- utils::help(topic, package = "ISCAM")
  if (length(help_obj) == 0) {
    cat("No documentation available for", topic, "\n")
    return(invisible(FALSE))
  }

  rd_file <- utils:::.getHelpFile(help_obj)
  cat(paste(.iscam_help_output(rd_file), collapse = "\n"), "\n", sep = "")
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
