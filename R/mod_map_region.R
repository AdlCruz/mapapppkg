#' map_country Server Functions
#'
#' @noRd
mod_map_region_server <- function(id, data, spatres, varname){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # RVS MARKERS POLYGONS SHOWN LOCATIONS
    rv <- reactiveValues(lastCountryCount = 0, polygonsr = NULL)

    fillOp_poly <- 0.4

    # FIRST TWO SECTIONS CAN BE COPY PASTED IN COUNTRY AND REGION

    # MINIMUM VIABLE MAP
    output$map <- leaflet::renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 3)) %>%
        addTiles(options = providerTileOptions(updateWhenZooming = FALSE,
                                               updateWhenIdle = TRUE)) %>%
        setView(lat=49,lng=12, zoom=5) %>%
        setMaxBounds(lng1 = -22,lat1 = 24,lng2 = 42,lat2 = 72)
    })

    # OBS FOR IF NO-DATA
    observe({

      if (nrow(data()) == 0) {
        leafletProxy("map") %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearControls() %>%
          clearPopups()%>%
          setView(lat=50, lng=3, zoom= 3) %>%
          addPopups(
            lat=50, lng=3,
            "<div style='background-color: black; color: white; padding: 10px; font-size: 20px;'>No data available for the current selection.</div>",
            options = popupOptions(maxWidth = 300, minWidth = 200)
          )
        return()  # End the execution of the current observer block
      }
    })

    # OBS FOR MARKER DRAWING AND FILLING AND LEGEND AND PALETTE BUILDING AND TITLING
    observe({
      # GET DATA
      data <- data()
      spatres <- spatres()
      if (nrow(data) != 0 && spatres == "Region") {
        # SET VAR
        varname <- varname()
        varval <- data[[varname]]
        # SET RADIUS
        radius <- log(varval)
        # radius <- sqrt(varval)
        # radius <- ~ifelse(input$map_zoom >= 7, 20+log(AREA_KM2), log(AREA_KM2))
        # SET PAL
        # SET LEGEND

        data <- data

        # create update polygons
        if (!is.null(rv$polygonsr)) {
          leafletProxy("map", data = data) %>%
            clearMarkers() %>% clearShapes()%>% clearPopups()%>%
            addPolygons(
              stroke=TRUE,
              color = "darkgrey",
              weight=2,
              fill=TRUE,
              # fillColor=~pal(var),
              fillColor="black",
              fillOpacity = fillOp_poly,
              label = paste0("<b>",data$region,"<br>",
                             "Value: ",round(varval,3),"</b>") %>%
                lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              )
            ) %>%
            clearControls() #%>%
          # addLegend(
          #   position="bottomleft",
          #   pal=pal,
          #   values=var,
          #   title= legTitle,
          #   labFormat = labFormatFunction
          # )
        } else {
          # initial launch
          leafletProxy("map") %>%
            clearMarkers()%>%
            clearShapes()%>%
            clearPopups()%>%
            addPolygons(
              data = data,
              fillColor = "blue",
              fillOpacity = fillOp_poly,
              color = "darkgrey",
              weight = 4
            ) -> rv$polygonsr
        }
      }
    })
  })
}

