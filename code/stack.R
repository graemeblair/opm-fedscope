library(dplyr)
library(purrr)
library(stringr)
library(arrow)

# Stack monthly slices into one combined parquet per type.
#
# accessions/separations are small enough for the R-tibble path
# (map_dfr + arrange across the agency/series/station codes).
#
# employment expands to too many rows for `bind_rows` even after retyping
# (the row count alone — ~440M rows — overwhelms R's data-frame overhead).
# For employment we stay in arrow's Table representation end-to-end: each
# slice's dict-encoded columns concat without expansion, then we write
# straight from arrow.

stack_r_tibble <- function(type) {
  list.files(
    "slices",
    pattern = paste0("^", type, "-\\d{6}\\.parquet$"),
    full.names = TRUE
  ) |>
    map_dfr(\(path) {
      date_part <- sub(".*-(\\d{6})\\.parquet$", "\\1", basename(path))
      read_parquet(path) |>
        mutate(
          year = as.integer(substr(date_part, 1, 4)),
          month = as.integer(substr(date_part, 5, 6))
        )
    }) |>
    arrange(
      pick(any_of(c(
        "agency_subelement_code",
        "occupational_series_code",
        "duty_station_code"
      ))),
      year,
      month
    )
}

stack_arrow <- function(type) {
  files <- sort(list.files(
    "slices",
    pattern = paste0("^", type, "-\\d{6}\\.parquet$"),
    full.names = TRUE
  ))

  tables <- vector("list", length(files))
  for (i in seq_along(files)) {
    date_part <- sub(".*-(\\d{6})\\.parquet$", "\\1", basename(files[i]))
    tables[[i]] <- read_parquet(files[i], as_data_frame = FALSE) |>
      mutate(
        year = as.integer(substr(date_part, 1, 4)),
        month = as.integer(substr(date_part, 5, 6))
      ) |>
      compute()
    if (i %% 25 == 0) message(sprintf("  %s: loaded %d/%d", type, i, length(files)))
  }

  combined <- concat_tables(!!!tables, unify_schemas = TRUE)
  rm(tables); gc(verbose = FALSE)
  combined
}

write_stacked <- function(stacked, type) {
  arrow::write_parquet(
    stacked,
    paste0("data/opm-", type, ".parquet"),
    compression = "zstd",
    compression_level = 15L,
    use_dictionary = TRUE
  )
}

for (type in c("accessions", "separations")) {
  message(sprintf("=== %s (R-tibble path)", type))
  write_stacked(stack_r_tibble(type), type)
}

message("=== employment (arrow-native path)")
write_stacked(stack_arrow("employment"), "employment")
