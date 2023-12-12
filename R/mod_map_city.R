
#' map_city Server Functions
#'
#' @noRd
mod_map_city_server <- function(id, data, spatres, varname){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # RVS MARKERS POLYGONS SHOWN LOCATIONS
    rv <- reactiveValues(lastCityCount = 0, markers = NULL)

    fillOp_marker <- 0.6

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
      if (nrow(data) != 0  && spatres == "City" ) {

        # SET VAR
        varname <- varname()
        varval <- data[[varname]]

        # SET RADIUS
        radius <- log(varval)
        # radius <- sqrt(varval)
        # radius <- ~ifelse(input$map_zoom >= 7, 20+log(AREA_KM2), log(AREA_KM2))

        # SET PAL
        # SET LEGEND

        # create OR update markers
        if (!is.null(rv$markers)) {

          leafletProxy("map") %>%
            clearMarkers() %>% clearShapes() %>% clearPopups() %>%
            addCircleMarkers(
              data = data,
              lat = ~latitude,
              lng = ~longitude,
              label = paste0("<b>",data$city_name,", ",data$country_name,"<br>",
                             "Value: ",round(varval,3),"</b>") %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              ),
              #radius = ~ifelse(input$map_zoom >= 7, 20+log(AREA_KM2), log(AREA_KM2)),
              #radius = ~sqrt(data[[varname()]]),
              radius=radius,
              #radius = 10,
              stroke=TRUE,
              color="black",
              weight=1,
              fill=TRUE,
              fillOpacity = fillOp_marker,
              #fillColor = ~pal(varval)
              fillColor = radius
            )# addLegend
        } else {
          # initial launch
          leafletProxy("map") %>%
            setView(lng=mean(data$longitude),lat=mean(data$latitude),zoom=3)%>%
            addCircleMarkers(
              data = data,
              lat = ~latitude,
              lng = ~longitude,
              fillOpacity = fillOp_marker
            ) -> rv$markers
        }
      }
    })
  })
}

