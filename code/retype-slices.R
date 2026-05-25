library(dplyr)
library(arrow)
library(parallel)

source("code/schemas.R")

slice_type <- function(path) {
  sub("^(accessions|separations|employment)-.*$", "\\1", basename(path))
}

# True if the file's `count` column already has the target int32 type, i.e.
# the slice has been retyped.
already_retyped <- function(path) {
  tryCatch({
    s <- read_parquet(path, as_data_frame = FALSE)$schema
    "count" %in% s$names &&
      s$GetFieldByName("count")$type$ToString() == "int32"
  }, error = function(e) FALSE)
}

retype_slice <- function(path) {
  type <- slice_type(path)
  tbl <- read_parquet(path, as_data_frame = FALSE)
  present <- tbl$schema$names

  expr <- tbl

  for (col in intersect(c(INT_COLS, FLOAT_COLS), present)) {
    expr <- expr |>
      mutate(!!sym(col) := if_else(!!sym(col) %in% NA_STRINGS, NA_character_, !!sym(col)))
  }
  for (col in intersect(BOOL_COLS, present)) {
    expr <- expr |>
      mutate(!!sym(col) := case_when(
        !!sym(col) == "Y" ~ TRUE,
        !!sym(col) == "N" ~ FALSE,
        TRUE ~ NA
      ))
  }

  target <- slice_schema(type)
  keep <- intersect(target$names, present)

  materialized <- expr |>
    select(all_of(keep)) |>
    compute()

  cast_fields <- lapply(materialized$schema$names, function(n) target$GetFieldByName(n))
  cast_schema <- do.call(schema, cast_fields)

  retyped <- materialized$cast(cast_schema)

  # Drop lazy references so the source file can be safely replaced.
  rm(tbl, expr, materialized); gc(verbose = FALSE)

  tmp <- tempfile(fileext = ".parquet")
  write_parquet(retyped, tmp,
    compression = "zstd", compression_level = 15L, use_dictionary = TRUE
  )
  file.rename(tmp, path)
  invisible(NULL)
}

retype_safe <- function(path) {
  tryCatch({
    retype_slice(path)
    paste0("OK ", basename(path))
  }, error = function(e) {
    paste0("ERR ", basename(path), ": ", conditionMessage(e))
  })
}

files <- sort(list.files(
  "slices",
  pattern = "^(accessions|separations|employment)-\\d{6}\\.parquet$",
  full.names = TRUE
))

todo <- files[!vapply(files, already_retyped, logical(1))]

message(sprintf("retype: %d / %d slices remaining", length(todo), length(files)))

if (length(todo) > 0) {
  n_workers <- min(8L, length(todo))
  message(sprintf("running mclapply with %d workers", n_workers))
  results <- mclapply(todo, retype_safe, mc.cores = n_workers, mc.preschedule = FALSE)
  errors <- grep("^ERR ", results, value = TRUE)
  if (length(errors) > 0) {
    message(sprintf("%d failures:", length(errors)))
    for (e in errors) message("  ", e)
  } else {
    message("all retypes OK")
  }
}

message("done")
