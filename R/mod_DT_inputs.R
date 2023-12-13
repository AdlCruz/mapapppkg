#' dt_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DT_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,offset=2,shinyWidgets::awesomeRadio(ns("spatres"), "Spatial domain",spatres_opts,selected="Region", inline = TRUE)),
      column(4,offset=2 ,shinyWidgets::awesomeRadio(ns("datamode"), "By",datamode_opts,selected = "Period",inline=TRUE))
    )
  )
}

#' dt_inputs Server Functions
#'
#' @noRd
mod_DT_inputs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(list(spatres=reactive(input$spatres),
                datamode=reactive(input$datamode)))

  })
}

## To be copied in the UI
# mod_dt_inputs_ui("dt_inputs_1")

## To be copied in the server
# mod_dt_inputs_server("dt_inputs_1")
