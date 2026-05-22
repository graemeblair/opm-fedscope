library(dplyr)
library(purrr)
library(arrow)

counts_df <-
  c("accessions", "employment", "separations") |>
  set_names() |>
  map_dfr(
    \(type) {
      list.files(
        "slices",
        pattern = paste0("^", type, "-\\d{6}\\.parquet$"),
        full.names = TRUE
      ) |>
        set_names() |>
        map_dfr(
          \(path) {
            date_part <- sub(".*-(\\d{6})\\.parquet$", "\\1", basename(path))
            read_parquet(
              path,
              col_select = c("agency", "agency_subelement", "count")
            ) |>
              group_by(agency, agency_subelement) |>
              summarise(n = sum(as.integer(count)), .groups = "drop") |>
              mutate(
                year = as.integer(substr(date_part, 1, 4)),
                month = as.integer(substr(date_part, 5, 6))
              )
          },
          .id = "file"
        )
    },
    .id = "type"
  )

arrow::write_parquet(
  counts_df,
  "data/opm-counts.parquet",
  compression = "ZSTD"
)
