#' rank UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rank_ui <- function(id){
  ns <- NS(id)
  tagList(DT::DTOutput(ns("rank")))
}


#' rank Server Functions
#' @importFrom dplyr ungroup select across arrange ntile where
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @noRd
mod_rank_server <- function(id, data, spatres, varselect){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    drank <- reactive({
      
      d <- data() %>% dplyr::collect()

      if (nrow(d)==0) {
        return(data.frame(NOTICE="This selection results in no data"))}

      if (any(names(d)%in%c("geometry","geom"))) { d$geometry <- NULL }
      v <- varselect()

      if (spatres()!="City" && length(unique(d$country_name))==1) {
        return(data.frame(NOTICE="Please select more than one country or switch to City view"))}

      q <- quantile(d[[v]], seq(0, 1, by = .25), na.rm=TRUE) |> unique()
      d$Group <- cut(d[[v]], breaks = q, labels = seq(length(q) - 1), include.lowest = TRUE)

      d <- d  %>% dplyr::ungroup()  %>% dplyr::select(any_of(
        c("country_name","city_name","region","period","level","agegroup","ssp","sc","range",
          v,paste0(v,"_low"), paste0(v,"_high"), "Group"))) %>%
        dplyr::mutate(across(dplyr::where(is.numeric), round, 3), across(where(is.character), as.factor)) %>%
        dplyr::arrange(dplyr::desc(.data[[v]])) %>%
        rename_columns(data=.)

      return(d)

    })

    output$rank <- DT::renderDT({

      drnk <- drank()

      datatable(drnk,
                selection = list(target = 'row+column'),
                extensions = "Buttons",
                rownames = TRUE,
                options = list(
                  lengthMenu =list(c(10, 50,100,-1),c("10", "50","100","All")),
                  pageLength = 15,
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}"),
                  dom = 'Blfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf'),
                  serverSide = TRUE))
    })

  })
}

## To be copied in the UI
# mod_rank_ui("rank_1")

## To be copied in the server
# mod_rank_server("rank_1")
