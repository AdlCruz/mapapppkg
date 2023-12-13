#' filter_data
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_filter_data <- function(conn, datamode, spatres,
                              per, lvl, agegr, ss, s, rang,
                              locations) {

  query <- conn
  #
  # DATA TYPE
  if (datamode=="Period"){

    query <- query %>% dplyr::filter(period==per)

  } else if (datamode=="Temperature increase (\u00B0C)") {

    query <- query %>% dplyr::filter(level==lvl)

  }

  # FILTERS
  query <- query %>%
    dplyr::filter(agegroup == agegr,
           ssp == ss,
           sc == s,
           range == rang) %>%
    dplyr::mutate(an = round(an, 0))

  # LOCATION
  if (spatres == "Region" | any(locations %in% "All")) {

    return(query)

  } else {

    #
    pattern <- paste(locations, collapse = "|")
    # 
    # 
    query %>% dplyr::filter(base::grepl(pattern, country_name))

  }

}
