#' connect_arrow
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_connect_arrow <- function(datamode, spatres) {

  if (datamode=="Period"){

    data <- switch(spatres,
                   "City"    = arrow::open_dataset(sources = "data/city_period", partitioning = "agegroup"),
                   "Country" = arrow::open_dataset(sources = "data/country_period", partitioning = "agegroup"),
                   "Region"  = arrow::open_dataset(sources = "data/region_period", partitioning = "agegroup"),
                   default   = NULL)

  } else {

    data <- switch(spatres,
                   "City"    = arrow::open_dataset(sources = "data/city_level", partitioning = "agegroup"),
                   "Country" = arrow::open_dataset(sources = "data/country_level", partitioning = "agegroup"),
                   "Region"  = arrow::open_dataset(sources = "data/region_level", partitioning = "agegroup"),
                   default   = NULL)

  }

  return(data)

}
