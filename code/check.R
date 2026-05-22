library(dplyr)
library(purrr)
library(arrow)
library(glue)
library(pointblank)

EXPECTED_COLS <- list(
  employment = sort(c(
    "age_bracket", "agency", "agency_code", "agency_subelement",
    "agency_subelement_code", "annualized_adjusted_basic_pay",
    "appointment_type", "appointment_type_code", "bargaining_unit",
    "bargaining_unit_code", "bargaining_unit_status", "cfo_act_agency_indicator",
    "consolidated_statistical_area", "consolidated_statistical_area_code",
    "core_based_statistical_area", "core_based_statistical_area_code", "count",
    "department", "department_code", "duty_station_city",
    "duty_station_code", "duty_station_country", "duty_station_country_code",
    "duty_station_county", "duty_station_county_code", "duty_station_state",
    "duty_station_state_abbreviation",
    "duty_station_state_country_territory_code", "education_level",
    "education_level_bracket", "education_level_code", "flsa_category",
    "flsa_category_code", "grade", "length_of_service_years",
    "locality_pay_area", "locality_pay_area_code", "nsftp_indicator",
    "occupational_category", "occupational_category_code", "occupational_group",
    "occupational_group_code", "occupational_series", "occupational_series_code",
    "pay_basis", "pay_basis_code", "pay_plan", "pay_plan_code",
    "personnel_office_identifier_code", "position_occupied",
    "position_occupied_code", "service_computation_date_leave",
    "snapshot_yyyymm", "stem_occupation", "stem_occupation_type",
    "step_or_rate_type", "step_or_rate_type_code", "supervisory_status",
    "supervisory_status_code", "tenure", "tenure_code", "veteran_indicator",
    "work_schedule", "work_schedule_code"
  )),
  accessions = sort(c(
    "accession_category", "accession_category_code", "age_bracket", "agency",
    "agency_code", "agency_subelement", "agency_subelement_code",
    "annualized_adjusted_basic_pay", "appointment_not_to_exceed_date",
    "appointment_type", "appointment_type_code", "bargaining_unit",
    "bargaining_unit_code", "bargaining_unit_status", "cfo_act_agency_indicator",
    "consolidated_statistical_area", "consolidated_statistical_area_code",
    "core_based_statistical_area", "core_based_statistical_area_code", "count",
    "department", "department_code", "duty_station_city",
    "duty_station_code", "duty_station_country", "duty_station_country_code",
    "duty_station_county", "duty_station_county_code", "duty_station_state",
    "duty_station_state_abbreviation",
    "duty_station_state_country_territory_code", "education_level",
    "education_level_bracket", "education_level_code", "flsa_category",
    "flsa_category_code", "grade", "length_of_service_years",
    "locality_pay_area", "locality_pay_area_code", "nsftp_indicator",
    "occupational_category", "occupational_category_code", "occupational_group",
    "occupational_group_code", "occupational_series", "occupational_series_code",
    "pathways_group",
    "pay_basis", "pay_basis_code", "pay_plan", "pay_plan_code",
    "personnel_action_effective_date_yyyymm", "personnel_office_identifier_code",
    "position_occupied", "position_occupied_code", "service_computation_date_leave",
    "stem_occupation", "stem_occupation_type", "step_or_rate_type",
    "step_or_rate_type_code", "supervisory_status", "supervisory_status_code",
    "tenure", "tenure_code", "veteran_indicator", "work_schedule",
    "work_schedule_code"
  )),
  separations = sort(c(
    "age_bracket", "agency", "agency_code", "agency_subelement",
    "agency_subelement_code", "annualized_adjusted_basic_pay",
    "appointment_not_to_exceed_date", "appointment_type", "appointment_type_code",
    "bargaining_unit", "bargaining_unit_code", "bargaining_unit_status",
    "cfo_act_agency_indicator", "consolidated_statistical_area",
    "consolidated_statistical_area_code", "core_based_statistical_area",
    "core_based_statistical_area_code", "count",
    "department", "department_code", "drp_indicator", "duty_station_city",
    "duty_station_code", "duty_station_country", "duty_station_country_code",
    "duty_station_county", "duty_station_county_code", "duty_station_state",
    "duty_station_state_abbreviation",
    "duty_station_state_country_territory_code", "education_level",
    "education_level_bracket", "education_level_code", "flsa_category",
    "flsa_category_code", "grade", "length_of_service_years",
    "locality_pay_area", "locality_pay_area_code", "nsftp_indicator",
    "occupational_category", "occupational_category_code", "occupational_group",
    "occupational_group_code", "occupational_series", "occupational_series_code",
    "pathways_group",
    "pay_basis", "pay_basis_code", "pay_plan", "pay_plan_code",
    "personnel_action_effective_date_yyyymm", "personnel_office_identifier_code",
    "position_occupied", "position_occupied_code", "separation_category",
    "separation_category_code", "service_computation_date_leave", "stem_occupation",
    "stem_occupation_type", "step_or_rate_type", "step_or_rate_type_code",
    "supervisory_status", "supervisory_status_code", "tenure", "tenure_code",
    "veteran_indicator", "work_schedule", "work_schedule_code"
  ))
)

DATA_TYPES <- names(EXPECTED_COLS)
MAX_LAG_MONTHS <- 3

agents <- map(DATA_TYPES, \(type) {
  files <- sort(list.files("slices", pattern = glue("^{type}-\\d{{6}}\\.parquet$"), full.names = TRUE))
  months_on_disk <- sub(".*-(\\d{6})\\.parquet$", "\\1", basename(files))
  latest_yyyymm <- tail(months_on_disk, 1)
  latest_date <- as.Date(paste0(latest_yyyymm, "01"), "%Y%m%d")

  lag_months <- (as.integer(format(Sys.Date(), "%Y")) - as.integer(format(latest_date, "%Y"))) * 12 +
    as.integer(format(Sys.Date(), "%m")) - as.integer(format(latest_date, "%m"))

  inventory <- tibble(latest_yyyymm = latest_yyyymm, lag = lag_months)

  inventory_agent <- inventory |>
    create_agent(label = glue("{type} — file inventory"), actions = action_levels(warn_at = 1, stop_at = 1)) |>
    col_vals_lte(columns = vars(lag), value = MAX_LAG_MONTHS, label = glue("latest month within {MAX_LAG_MONTHS}-month lag window")) |>
    interrogate()

  df <- read_parquet(tail(files, 1))
  date_col <- if (type == "employment") "snapshot_yyyymm" else "personnel_action_effective_date_yyyymm"
  date_regex <- if (type == "employment") glue("^{latest_yyyymm}$") else "^\\d{6}$"

  content_agent <- df |>
    create_agent(label = glue("{type} — {latest_yyyymm}"), actions = action_levels(warn_at = 0.0001, stop_at = 0.01)) |>
    col_schema_match(
      schema = do.call(col_schema, as.list(setNames(rep("character", length(EXPECTED_COLS[[type]])), EXPECTED_COLS[[type]]))),
      complete = FALSE, in_order = FALSE,
      label = "expected columns present and character type"
    ) |>
    col_vals_not_null(columns = vars(count), label = "count not null") |>
    col_vals_regex(columns = vars(count), regex = "^[1-9]\\d*$", label = "count is positive integer string") |>
    col_vals_not_null(columns = vars(agency_code), label = "agency_code not null") |>
    col_vals_regex(columns = all_of(date_col), regex = date_regex, label = glue("date field valid ({date_col})")) |>
    interrogate()

  list(inventory_agent, content_agent)
}) |> list_flatten()

all_pass <- every(agents, \(a) all(a$validation_set$all_passed))

md <- map_chr(agents, \(a) {
  header <- glue("### {a$label}")
  rows <- a$validation_set |>
    transmute(
      status = if_else(all_passed, "PASS", "FAIL"),
      label,
      detail = glue("n={format(n, big.mark=',')} failing={format(n_failed, big.mark=',')}")
    ) |>
    pmap_chr(\(status, label, detail) {
      icon <- if (status == "PASS") ":white_check_mark:" else ":x:"
      line <- glue("- {icon} **{status}** {label}")
      if (status != "PASS") line <- glue("{line}  \n  {detail}")
      line
    })
  paste(c(header, rows), collapse = "\n")
}) |> paste(collapse = "\n\n")

overall <- if (all_pass) ":white_check_mark: **PASS**" else ":x: **FAIL**"
md <- paste0("## Data validation: ", overall, "\n\n", md, "\n")

out_path <- Sys.getenv("CHECK_SUMMARY_PATH", "check-summary.md")
writeLines(md, out_path)

cat(md, "\n")
if (!all_pass) stop("Some checks FAILED — see output above.", call. = FALSE)
