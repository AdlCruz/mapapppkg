#' DT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom DT renderDT datatable JS
#' @importFrom shiny NS tagList
#'
mod_DT_ui <- function(id){
  ns <- NS(id)
  tagList(

    downloadButton(ns("dld"), "Raw Data Download (please allow up to a minute)"),
    DT::DTOutput(ns("dt"))

  )
}

#' DT Server Functions
#'
#' @noRd
mod_DT_server <- function(id, data, spatres, datamode){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$dt <- DT::renderDT({

      roundcols <- c(c("an","af","rate"),
                     paste0(c("an","af","rate"),rep(c("_low","_high"),each=3)),
                     c("stdrate","stdrate_low","stdrate_high"))

      # sample at random
      dat <- data() %>% dplyr::slice_sample(n=150) %>% dplyr::collect()

      dtd <- dat %>%
        dplyr::mutate(dplyr::across(any_of(roundcols), ~ round(.x, digits=3))) %>%
        rename_columns(.)  %>%
        dplyr::select(which(sapply(., function(x) !all(is.na(x)))))

      

      if (any(names(dtd)%in%c("geometry","geom"))) { dtd$geometry <- NULL }

      datatable(dtd,
                options = list(
                  lengthMenu = list(c(10, 50,100),c("10", "50","100")),
                  pageLength = 50,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}"),
                  dom = 'lfrtip',
                  serverSide=TRUE
                ))

    })

    # removes geometry col from downloaded data
    dldData <- reactive({

      d <- data() %>% collect()

      if (any(names(d)%in%c("geometry","geom"))) { d$geometry <- NULL }

      return(d)

    })

    output$dld <- downloadHandler(
      filename = function() {
        paste0(ifelse(datamode()=="Period","Period","Temp"),"_", spatres(), "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(dldData(), file, row.names = FALSE)
      }
    )
  })
}

## To be copied in the UI
# mod_DT_ui("DT_1")

## To be copied in the server
# mod_DT_server("DT_1")
