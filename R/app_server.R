#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom arrow open_dataset
#' @importFrom dplyr slice_sample collect filter mutate show_query
#' @import data.table
#' @importFrom sfarrow read_sf_dataset
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

######################### MAP #################################################

  # READ REACTIVE INPUTS

  inp_map_L <- mod_inputs_server_1("inputsMAP_L")
  inp_map_R <- mod_inputs_server_2("inputsMAP_R")

  # ESTABLISH ARROW FILE CONNECTIONS

  data_conn_map <- reactive({
    
    utils_connect_arrow( inp_map_L$datamode(), inp_map_L$spatres() )
    })

  # BUILD LAZY QUERY

  data_map_query <- reactive({
    

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
    

    utils_collect_data(query = data_map_query(),
                       spatres = isolate( inp_map_L$spatres() ))
    #
  })

  # MAP MODULES #

  observe({

    sr = inp_map_L$spatres()

    if (sr == "City") {
      mod_map_city_server("map", data = map_data, spatres = inp_map_L$spatres, varname = inp_map_L$varselect, temp = inp_map_R$rang)
      } else if (sr == "Country") {
        mod_map_country_server("map", data = map_data, spatres = inp_map_L$spatres, varname = inp_map_L$varselect , temp = inp_map_R$rang)
      } else if(sr == "Region") {
        mod_map_region_server("map", data = map_data, spatres = inp_map_L$spatres, varname = inp_map_L$varselect , temp = inp_map_R$rang)
      }

  })

######################### RANK #################################################

  inp_rank_L <- mod_inputs_server_1("inputsRANK_L")
  inp_rank_R <- mod_inputs_server_2("inputsRANK_R")

  data_conn_rank <- reactive({
    
    utils_connect_arrow( inp_rank_L$datamode(), inp_rank_L$spatres() )
  })

  data_rank_query <- reactive({
    

    utils_filter_data(
      conn = data_conn_rank(),
      datamode = isolate( inp_rank_L$datamode() ),
      spatres = isolate( inp_rank_L$spatres() ),
      per = inp_rank_L$per(),
      lvl = inp_rank_L$level(),
      agegr = inp_rank_R$agegr(),
      ss = inp_rank_R$ss(),
      s = inp_rank_R$s(),
      rang = inp_rank_R$rang(),
      locations = inp_rank_R$location()
    )
  })

  # COLLECT SUBSET DATA
  rank_data <- reactive({
    

    utils_collect_data(query = data_rank_query(),
                       spatres = isolate( inp_rank_L$spatres() ))
  })

  mod_rank_server("rank",rank_data, inp_rank_L$spatres, inp_rank_L$varselect)

############################ DATA TABLE ########################################

  inp_dt <- mod_DT_inputs_server("inputsDT_UI")

  data_conn_DT <- reactive({
    
    utils_connect_arrow( inp_dt$datamode(), inp_dt$spatres() )
  })

  mod_DT_server("dt", data_conn_DT,spatres = inp_dt$spatres, datamode = inp_dt$datamode )


}
