pkgs <- c(
  "rvest",
  "purrr",
  "dplyr",
  "readxl",
  "usethis",
  "tools"
)
installed_pkgs <- purrr::map_lgl(pkgs, requireNamespace)
if (!all(installed_pkgs)) {
  missing_installed_pkgs <- pkgs[!installed_pkgs]
  stop(
    "The following suggested packages are required for this function: ",
    paste(missing_installed_pkgs, collapse = ", "),
    ".\nPlease install them to proceed.",
    call. = FALSE
  )
}

all_links <- rvest::read_html(
  "https://www.rossmanchance.com/iscam3/files.html"
) |>
  rvest::html_nodes(
    xpath = '//tr[contains(., "Inv Data files") or contains(., "HW Data files")]'
  ) |>
  rvest::html_nodes("a")

data_files <- dplyr::tibble(
  URL = all_links |>
    rvest::html_attr("href") |>
    trimws(),
  filename = all_links |>
    rvest::html_text() |>
    trimws() |>
    tools::file_path_sans_ext()
) |>
  dplyr::filter(!grepl("\\.(jsl|R|mac|jmp)$", URL, ignore.case = TRUE)) |>
  dplyr::mutate(
    filename = sub("(stacked)", "SpockPersStacked", filename, fixed = TRUE)
  )

errored_urls <- character(0)
process_and_save <- function(url, filename, verbose = FALSE) {
  tryCatch(
    {
      file_ext <- tolower(tools::file_ext(url))
      raw_data <- if (file_ext == "txt") {
        read.table(
          url,
          header = TRUE,
          sep = "\t",
          stringsAsFactors = FALSE,
          fill = TRUE
        )
      } else if (file_ext == "csv") {
        read.csv(url, stringsAsFactors = FALSE)
      } else if (file_ext %in% c("xls", "xlsx")) {
        temp_file <- tempfile(fileext = paste0(".", file_ext))
        download.file(url, temp_file, mode = "wb", quiet = TRUE)
        readxl::read_excel(temp_file)
      } else {
        stop("Unsupported file type", call. = FALSE)
      }

      assign(filename, raw_data, envir = .GlobalEnv)
      suppressMessages(
        do.call(usethis::use_data, list(as.name(filename), overwrite = TRUE))
      )
    },
    error = function(e) {
      if (verbose) {
        message("\n-----------------------------------------")
        message(paste0("ERROR processing file: '", filename, "'"))
        message(paste0("URL: ", url))
        message(paste0("Error message: ", e$message))
        message("-----------------------------------------\n")
      }
      errored_urls <<- c(errored_urls, url)
    }
  )
}

purrr::walk2(
  data_files$URL,
  data_files$filename,
  purrr::in_parallel(\(URL, file) process_and_save(URL, file)),
  .progress = TRUE
)

if (length(errored_urls) > 0) {
  cat("--------------------------------------------------\n")
  cat("The following", length(errored_urls), "URLs failed to process:\n")
  cat("--------------------------------------------------\n")
  for (url in unique(errored_urls)) {
    cat(url, "\n")
  }
} else {
  cat("---------------------------------\n")
  cat("All URLs processed successfully.\n")
  cat("---------------------------------\n")
}
