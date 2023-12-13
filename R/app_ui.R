#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import markdown
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
      ),
      # RANK #
      tabPanel(
        "Rank",
        fluidRow(
          column(3,
                 tagList(wellPanel(mod_inputs_ui_1("inputsRANK_L")),
                         wellPanel(mod_inputs_ui_2("inputsRANK_R")))
          ),
          column(9,
                 mod_rank_ui("rank")
          )
        )
      ),
      # # ALL DATA #
      tabPanel(
        "All data",
        wellPanel(id="dtwell", mod_DT_inputs_ui("inputsDT_UI")),
        mod_DT_ui("dt")
      ),
      # ABOUT #
      tabPanel(
        "About/Help/?",
        fluidRow(
          column(8,offset=2,
                 wellPanel(htmltools::includeMarkdown("inst/app/www/about.md")))
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
    favicon(ext="png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "hipmapp"
    )#,shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # UNCOMMENT FOR CSS STYLING
  )
}
