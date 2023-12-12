#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import leaflet
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(outputId = ns(id), height = "90vh")#, width = "95vh") # width = "100%", height = "100%") #
  )
}

#'
#'       #lastCountryCount = 0, polygonsc = NULL, polygonsr = NULL
