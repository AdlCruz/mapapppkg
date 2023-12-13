mod_inputs_ui_1 <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::awesomeRadio(ns("spatres"), "Spatial domain", spatres_opts,selected="City", inline = T),
    shinyWidgets::awesomeRadio(ns("datamode"), "By", datamode_opts,selected = "Period",inline=TRUE),
    shinyWidgets::sliderTextInput(ns("period"), "",period_opts, selected="2015"),
    shinyWidgets::sliderTextInput(ns("level"), "", level_opts),
    shinyWidgets::awesomeRadio(ns("varselect"),"Impact measure",varselect_opts)

  )
}

mod_inputs_ui_2 <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("location"), "Country", country_opts, selected="Spain", multiple=TRUE, width=NULL),
    selectizeInput(ns("agegroup"),"Age group", agegroup_opts, selected="all", multiple=FALSE),
    shinyWidgets::awesomeRadio(ns("ssp"), "SSP-RCP-scenario",ssp_opts, inline=TRUE),
    shinyWidgets::awesomeRadio(ns("sc"), "Sub-scenario",sc_opts, inline=TRUE),
    shinyWidgets::awesomeRadio(ns("range"), "Temperature impact",range_opts,selected="heat", inline=TRUE)
  )
}

mod_inputs_server_1 <- function(id) {
  moduleServer(id, function(input, output, session){

    observe({
      shinyjs::toggle(id = "period", condition = input$datamode == "Period")
      shinyjs::toggle(id = "level", condition = input$datamode == "Temperature increase (\u00B0C)")
    })

    observe({
      if (input$spatres=="Region"){
        updateSelectizeInput(session,"location", selected= "All", choices = country_opts)
      }
      else if (input$spatres!="Region") {
        return()
      }
    })


    # Reactive return
    return(
      list(
        datamode=reactive(input$datamode),
        spatres=reactive(input$spatres),
        per=reactive(input$period),
        level=reactive(input$level),
        varselect=reactive(input$varselect))
    )
  }
)}


mod_inputs_server_2 <- function(id) {
  moduleServer(id, function(input, output, session){

    # Reactive value to store previous selections
    
    prev_selection <- reactiveVal(c("Spain"),label="loc_select")
    

    # FROM ONE COUNTRY TO ALL OR vv - BUG DOUBLE RENDER MAYBE FROM HERE
    observe({

      # Current selection
      curr_selection <- isolate( input$location )
      
      
      # If "All" is newly selected AND wasn't in the previous selection
      if ("All" %in% curr_selection && !"All" %in% isolate(prev_selection())) {
        set <- c("All")
        updateSelectizeInput(session, "location", selected = "All", choices=country_opts)
        curr_selection <- set
        # Update previous selections
        isolate(prev_selection(set))

      }
      # If "All" was selected and a country is newly selected
      else if (length(setdiff(curr_selection, prev_selection())) > 0 && "All" %in% prev_selection()) {

        # set selected to current selection minus All
        set <- setdiff(curr_selection, "All")
        updateSelectizeInput(session, "location", selected = set, choices=c("All", country_opts))
        isolate(prev_selection(set))
      }

      # ONLY REACT WHEN INPUT$COUNTRY IS LONGER THAN THE LAST SET PREVIOUS SELECTION - NOT EXACTLY THE ACTUAL PREV SEL
      # COULD JSUT BE AT INPUT$LOCATION
    }) %>%  bindEvent(input$location)

    return(
      list(
        agegr=reactive(input$agegroup),
        ss=reactive(input$ssp),
        s=reactive(input$sc),
        rang=reactive(input$range),
        location=reactive(input$location))
      )
  }
)}



