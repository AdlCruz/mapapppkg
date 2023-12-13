#' map_region Server Functions
#'
#' @noRd
mod_map_region_server <- function(id, data, spatres, varname, temp){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # RVS MARKERS POLYGONS SHOWN LOCATIONS
    rv <- reactiveValues(lastRegion = 0, polygonsr = NULL)

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

      if (nrow(data) != 0  && spatres == "Region" ) {

        # SET VAR ITEMS
        vnm <- varname()
        varval <- data[[vnm]]

        # SET LEGEND
        outcome_tag <- ifelse(vnm=="af","Excess fraction of deaths", # A.F
                              ifelse(vnm=="an","Excess deaths", # A.N
                                     ifelse(vnm=="rate","Excess mortality rate", NA)))
        legTitle <- htmltools::HTML(paste0("<b>",outcome_tag,"</b>"), .noWS="outside")

        # SET PAL
        # load outcome variable data and temperature mode inp_map_R$rang
        tempr <- temp()

        # conditional palette
        if (tempr == "heat") {
          palcol = "Reds"
        } else if (tempr == "cold") {
          palcol = "Blues"
        } else if (tempr == "tot") {
          palcol= "viridis"
        }

        # PALETTE - COLORS - LABELS SET WITH UTILITY FUNCTION - CHECK IT OUT IF YOU DARE
        # I HAVE NOT BEEN ABLE TO BREAK IT WHILE USING THE APP
        map_pal_items <- utils_get_set_pal(varval, vnm, palcol, qmax=5, bmax=7, factbin=6)

        pal <- map_pal_items$pal
        pal_colors <- map_pal_items$colors
        pal_labs <- map_pal_items$labs

        # create update polygons
        if (!is.null(rv$polygonsr)) {
          leafletProxy("map", data = data) %>%
            clearMarkers() %>% clearShapes()%>% clearPopups()%>%
            addPolygons(
              stroke=TRUE,
              color = "darkgrey",
              weight=2,
              fill=TRUE,
              fillColor=~pal(varval),
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
            clearControls() %>%
            addLegend(
              position="bottomleft",
              colors = pal_colors,
              labels = pal_labs,
              title= legTitle
            )
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

    observe({

          data <- data()
          filtered_datasf <- sf::st_as_sf(data)
          fcoords <- sf::st_coordinates(filtered_datasf)
          bbox1 <- c(range(fcoords[,"X"]),range(fcoords[,"Y"]))
          flyt <- c(bbox1[1],bbox1[3],bbox1[2],bbox1[4])

          leafletProxy("map") %>%
            flyToBounds(lng1 = flyt[1], lat1 = flyt[2], lng2 = flyt[3], lat2 = flyt[4])

      }) %>% bindEvent(req(spatres()=="Region"))
  })
}

