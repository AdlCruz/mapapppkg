
#' map_city Server Functions
#' @importFrom dplyr lag
#' @importFrom stats quantile
#'
#'
#' @noRd
mod_map_city_server <- function(id, data, spatres, varname, temp){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # RVS MARKERS POLYGONS SHOWN LOCATIONS
    rv <- reactiveValues(lastCityCount = 0, markers = NULL)

    fillOp_marker <- 0.6

      # FNEXT TWO SECTIONS CAN BE COPY PASTED IN COUNTRY AND REGION

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

        # SET VAR ITEMS
        vnm <- varname()
        varval <- data[[vnm]]

        # SET RADIUS
        radius <- log(varval)
        # radius <- sqrt(varval)
        # radius <- ~ifelse(input$map_zoom >= 7, 20+log(AREA_KM2), log(AREA_KM2))

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

        # unique data values
        uvln <- length(unique(varval))

        if (uvln <= 8) {
          pal <- colorFactor(palette = palcol, domain = as.factor(varval), na.color = "transparent")
          pal_colors <- unique(pal(sort(varval))) # hex codes
          pal_labs <- paste(sort(round(unique(varval),2))) # first lag is NA

        } else if (vnm == "an") {

          pal <- colorQuantile(palette = palcol, domain = varval, probs = seq(0, 1, .2), na.color = "transparent")
          pal_colors <- unique(pal(sort(varval))) # hex codes
          pal_labs <- round(quantile(varval, seq(0, 1, .2)),0) # depends on n from pal
          pal_labs <- paste(dplyr::lag(pal_labs), pal_labs, sep = " - ")[-1] # first lag is NA

        } else {
          pal <- leaflet::colorBin(palette = palcol, varval, bins = 7, na.color = "transparent")
          pal_colors <- unique(pal(sort(varval))) # hex codes
          pal_labs <- levels(
            cut(x=varval,breaks=length(pal_colors),  include.lowest=FALSE, right=FALSE)
          )
          brkpts <- paste0(gsub("\\[|\\]|\\(|\\)", "", pal_labs),collapse=",") %>%
            strsplit(.,",") %>% unlist() %>% unique(.)
          pal_labs <- paste(dplyr::lag(brkpts),brkpts,sep = " - ")[-1]
        }

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
              radius=radius,
              stroke=TRUE,
              color="black",
              weight=1,
              fill=TRUE,
              fillOpacity = fillOp_marker,
              fillColor = ~pal(varval)
            ) %>% clearControls() %>%
            addLegend(
              position="bottomleft",
              colors = pal_colors, labels = pal_labs, opacity = 1,
              title = legTitle
              #pal=pal,
              #values = varval, #if (varname == "an") q_breaks else var,
              #,
              #labFormat = labFormatFunction
            )
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

