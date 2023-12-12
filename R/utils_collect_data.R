#' collect_data
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_collect_data <- function(query, spatres) {

  if ( spatres != "City") {

    d <- query %>% sfarrow::read_sf_dataset()

    return(d)

  } else {

    d <- dplyr::collect(query)

    return(d)

  }

}
