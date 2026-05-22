library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(httr2)
library(arrow)
library(glue)

# ---- Configuration ----

DATA_TYPES <- c("accessions", "separations", "employment")
BASE_URL <- "https://data.opm.gov/api/blob/download/chunked"
MAX_VERSIONS <- 5

# ---- Helpers ----

opm_url <- function(type, year, month, version) {
  glue("{BASE_URL}/{type}_{year}{sprintf('%02d', month)}_{version}.txt")
}

out_path <- function(type, year, month) {
  glue("slices/{type}-{year}{sprintf('%02d', month)}.parquet")
}

# Try one version; returns temp file path or NULL
try_download <- function(url) {
  tmp <- tempfile(fileext = ".txt")

  resp <-
    request(url) |>
    req_timeout(600) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform(path = tmp)

  if (resp_status(resp) != 200) {
    unlink(tmp)
    return(NULL)
  }

  tmp
}

# Download the highest available version; returns temp path or NULL
download_month <- function(type, year, month) {
  best <- NULL

  for (v in seq_len(MAX_VERSIONS)) {
    path <- try_download(opm_url(type, year, month, v))
    if (is.null(path)) {
      break
    }
    if (!is.null(best)) {
      unlink(best)
    }
    best <- path
  }

  best
}

# ---- Main ----

dir.create("slices", showWarnings = FALSE, recursive = TRUE)

crossing(
  type = DATA_TYPES,
  year = 2005:as.integer(format(Sys.Date(), "%Y")),
  month = 1:12
) |>
  filter(as.Date(sprintf("%d-%02d-01", year, month)) <= Sys.Date()) |>
  filter(!file.exists(out_path(type, year, month))) |>
  pwalk(function(type, year, month) {
    message(sprintf("\n%s %d-%02d", type, year, month))

    tmp <- download_month(type, year, month)

    if (is.null(tmp)) {
      message("  Not yet available on OPM")
      return(invisible(NULL))
    }

    read_delim(
      tmp,
      delim = "|",
      col_types = cols(.default = "c"),
      show_col_types = FALSE
    ) |>
      write_parquet(out_path(type, year, month), compression = "ZSTD")

    unlink(tmp)
  })
