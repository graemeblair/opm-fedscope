library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(httr2)
library(arrow)
library(glue)

source("code/schemas.R")

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

    raw <-
      read_delim(
        tmp,
        delim = "|",
        col_types = cols(.default = "c"),
        show_col_types = FALSE
      )

    # Apply target schema: NA-clean numeric cols, Y/N → logical for bools,
    # then cast to int/float/bool/dictionary types defined in schemas.R.
    present <- names(raw)
    for (col in intersect(c(INT_COLS, FLOAT_COLS), present)) {
      raw[[col]] <- ifelse(raw[[col]] %in% NA_STRINGS, NA_character_, raw[[col]])
    }
    for (col in intersect(BOOL_COLS, present)) {
      raw[[col]] <- dplyr::case_when(
        raw[[col]] == "Y" ~ TRUE,
        raw[[col]] == "N" ~ FALSE,
        TRUE ~ NA
      )
    }

    target <- slice_schema(type)
    keep <- intersect(target$names, present)
    tbl <- arrow::as_arrow_table(raw[, keep, drop = FALSE])
    cast_fields <- lapply(tbl$schema$names, function(n) target$GetFieldByName(n))
    cast_schema <- do.call(arrow::schema, cast_fields)
    tbl <- tbl$cast(cast_schema)

    write_parquet(tbl, out_path(type, year, month),
      compression = "zstd", compression_level = 15L, use_dictionary = TRUE
    )

    unlink(tmp)
  })
