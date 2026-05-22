library(dplyr)
library(purrr)
library(stringr)
library(arrow)

for (type in c("accessions", "employment", "separations")) {
  stacked <-
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

  arrow::write_parquet(
    stacked,
    paste0("data/opm-", type, ".parquet"),
    compression = "zstd",
    compression_level = 22,
    use_dictionary = TRUE
  )
}
