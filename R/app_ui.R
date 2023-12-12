#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # for special javascript calls
    shinyjs::useShinyjs(),
    # APP UI LOGIC
    navbarPage(
      title="Health Impact Projections",
      # MAP #
      tabPanel(
        "Map",
        fluidRow(
          column(3,
                 wellPanel(mod_inputs_ui_1("inputsMAP_L"))
          ),
          column(6,
                 fluidRow(
                   mod_map_ui("map")
                   )
          ),
          column(3,
                 wellPanel(mod_inputs_ui_2("inputsMAP_R"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mapapppkg"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
