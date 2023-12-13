#' rename_cols
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# MISCELLANEOUS UTILITY FUNCTION FOR APP

# DATA FNS.
rename_rules <- list(
  period = "Period",
  level = "Temperature",
  agegroup = "Age-group",
  an = "AN",
  af = "AF",
  rate = "Rate",
  an_low = "AN_Low",
  an_high = "AN_High",
  af_low = "AF_Low",
  af_high = "AF_High",
  rate_low = "Rate_Low",
  rate_high = "Rate_High",
  stdrate = "Std.rate",
  stdrate_low = "Std.rate_Low",
  stdrate_high = "Std.rate_High",
  ssp = "SSP",
  sc = "Sub-scenario",
  range = "Clim-range",
  POP_2020 = "Population",
  AREA_KM2 = "Area",
  country_code = "Country-ID",
  country_name = "Country",
  city_name = "City",
  region = "Region",
  latitude = "Lat.",
  longitude = "Long.",
  Group = "Group"
)

rename_columns <- function(data, rr=rename_rules) {
  names(data) <- sapply(names(data), function(col) ifelse(col %in% names(rr), rr[[col]], col))
  return(data)
}
