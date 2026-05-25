library(arrow)

# Logical type assignments for slice columns.
#
# Every column outside these lists is dictionary-encoded utf8 — the right
# representation for the high-cardinality-but-repeating labels that
# dominate FedScope (agency, occupational_series, duty_station_city, etc.).

INT_COLS <- c(
  "count",
  "snapshot_yyyymm",
  "personnel_action_effective_date_yyyymm"
)

# Pay is reported as dollars but sometimes formatted with a trailing ".0"
# (e.g. "87198.0"), so it can't cast to int. float32 has enough precision
# for the realistic salary range.
FLOAT_COLS <- c(
  "annualized_adjusted_basic_pay",
  "length_of_service_years"
)

BOOL_COLS <- c(
  "veteran_indicator",
  "cfo_act_agency_indicator",
  "drp_indicator",
  "nsftp_indicator"
)

# Values that should become NA in numeric columns before casting.
NA_STRINGS <- c("", "*", "REDACTED")

# Expected column list per slice type. Kept in sync with code/check.R.
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

slice_schema <- function(type) {
  cols <- EXPECTED_COLS[[type]]
  fields <- lapply(cols, function(col) {
    t <-
      if (col %in% INT_COLS) int32()
      else if (col %in% FLOAT_COLS) float32()
      else if (col %in% BOOL_COLS) bool()
      else dictionary(int32(), utf8())
    field(col, t)
  })
  do.call(schema, fields)
}
