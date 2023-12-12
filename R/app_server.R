#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom arrow open_dataset
#' @importFrom dplyr slice_sample collect filter mutate show_query
#' @importFrom sfarrow read_sf_dataset
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  ## MAP ##

  # READ REACTIVE INPUTS

  inp_map_L <- mod_inputs_server_1("inputsMAP_L")
  inp_map_R <- mod_inputs_server_2("inputsMAP_R")

  # ESTABLISH ARROW FILE CONNECTIONS

  data_conn_map <- reactive({
    print("connect arrow")
    utils_connect_arrow( inp_map_L$datamode(), inp_map_L$spatres() )
    })

  # BUILD LAZY QUERY

  data_map_query <- reactive({
    print("start query")

    utils_filter_data(
      conn = data_conn_map(),
      datamode = isolate( inp_map_L$datamode() ),
      spatres = isolate( inp_map_L$spatres() ),
      per = inp_map_L$per(),
      lvl = inp_map_L$level(),
      agegr = inp_map_R$agegr(),
      ss = inp_map_R$ss(),
      s = inp_map_R$s(),
      rang = inp_map_R$rang(),
      locations = inp_map_R$location()
    )
  })

  # COLLECT SUBSET DATA
  map_data <- reactive({
    print("start collect")

    utils_collect_data(query = data_map_query(),
                       spatres = isolate( inp_map_L$spatres() ))
    #print(head(r,2))
  })

  # MAP MODULES #

  observe({

    spatres <- inp_map_L$spatres()

    if (inp_map_L$spatres() == "City") {
      mod_map_city_server("map", data = map_data, spatres = inp_map_L$spatres, varname = inp_map_L$varselect)
      } else if (inp_map_L$spatres() == "Country") {
        mod_map_country_server("map", data = map_data, spatres = inp_map_L$spatres, varname = inp_map_L$varselect)
      } else if(inp_map_L$spatres() == "Region") {
        mod_map_region_server("map", data = map_data, spatres = inp_map_L$spatres, varname = inp_map_L$varselect)
      }

  })



}
