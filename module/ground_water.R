
casgem_dataUI <- function(id) {
  ns <- NS(id) 
  
  tagList(
    fluidRow(
      column(width = 2, 
             tags$h4("Groundwater Data", style="display: inline;"),
             tags$br(),
             tags$em(tags$p("Available well data from DWR Water Data Library and 
           CASGEM database for Solano County", style="display: inline; font-size=14px;")), 
             tags$hr(), 
             selectInput(ns('well_year_filter'), "Filter by a minmum year of data", 
                         choices=(1950:2017))),
      column(width = 10, leafletOutput(ns("casgem_map")))
    ), 
    tags$br(),
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("casgem_plot")))
    )
  )
}

casgem_data <- function(input, output, session) {
  
  casgem_map_events <- reactiveValues(clicked_marker = NULL)
  
  # listen for a click on a shape file
  observeEvent(input$casgem_map_marker_click, {
    
    casgem_map_events$clicked_marker <- input$casgem_map_marker_click$id
    cat(casgem_map_events$clicked_marker)
  })
  
  casgem_ts <- reactive({
    casgem %>% 
      filter(SITE_CODE == casgem_map_events$clicked_marker, 
             !is.na(wse), wse >= 0, 
             year(MEASUREMENT_DATE) >= input$well_year_filter)
  })
  
  available_casgem_well_data <- reactive({
    casgem %>% 
      filter(year(MEASUREMENT_DATE) >= input$well_year_filter)
  })
  
  casgem_metdata_filtered <- reactive({
    available_stations <- distinct(available_casgem_well_data(), SITE_CODE) %>% pull()
    casgem_metadata %>% 
      filter(SITE_CODE %in% available_stations)
  })
  
  output$casgem_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      addCircleMarkers(data=casgem_metdata_filtered(), 
                       clusterOptions = markerClusterOptions(), 
                       color = "#666666", weight = 1, 
                       fillColor = "#1f78b4", fillOpacity = .6, 
                       layerId = ~SITE_CODE)
  })
  
  output$casgem_plot <- renderPlotly({
    # start up message
    validate(
      need(!is.null(casgem_map_events$clicked_marker), "Select well location above")
    )
    
    # no data message
    validate(need(nrow(casgem_ts()) > 0, "No data found at selected location"))
    
    validate(need(sum(!is.na(casgem_ts()$wse)) > 0, "No data found at selected location"))
 
    casgem_ts() %>%
      plot_ly(x=~MEASUREMENT_DATE, y=~wse, type='scatter', mode='lines',
              connectgaps = TRUE, 
              line = list(color="rgb(124, 124, 124, .2)", width=1), 
              showlegend = FALSE) %>%
      add_markers(y=~wse, marker=list(color="#668ac4"), name="Water Surface Elevation", 
                  showlegend = TRUE) %>%
      add_lines(y=~GS_ELEVATION, line=list(dash="dash", width=3, color="#fc8d62"), 
                showlegend = TRUE, name="Ground Surface Elevation") %>% 
      # this brings up issues for when data set is too small
      # add_lines(y= ~fitted(loess(wse ~ as.numeric(MEASUREMENT_DATE))),
      #           line = list(color="#b2df8a", width=5), name = "Fitted Trend (loess)",
      #           showlegend = TRUE) %>%
      layout(xaxis=list(title=""),
             yaxis=list(title="Water Surface Elevation (ft)"),
             showlegend = TRUE) 

    
  
})}

elevation_changeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h4('Groundwater Elevation Change (ft)'), 
             tags$br(), 
             tags$p("Using the DWR CASGEM groundwater elevation data, we 
                    interpolated water surface elevation (WSE) records by well into surfaces of 
                    groundwater change between 2015 and 2010 for the Spring and Fall seasons. We 
                    compared these contours to the DWR GICIMA on-line tool 
                    (https://gis.water.ca.gov/app/gicima/) in areas where they overlapped and 
                    found reasonable agreement between results. Next, we evaluated the significance of 
                    groundwater level change in different regions of the County using regression analysis. 
                    Together these analysis products provide both a high-level and more detailed 
                    understanding of recent groundwater changes."),
             tags$br(),
             tags$p("Groundwater conditions vary widely across the County and 
                    through time. The surfaces of WSE change show groundwater 
                    levels decreased between 2010 and 2015 in the northern portion 
                    of the County, with spring season groundwater level reductions 
                    being slightly greater than fall season reductions. Actual 
                    change in groundwater elevation per well are shown at the points 
                    on the map. Comparing the points with the surfaces illustrates 
                    how  the interpolation of groundwater change surfaces smooths 
                    out the measured changes at individual wells."
             )),
      column(width = 6,
             tabsetPanel(
               tabPanel('Fall',
                        withSpinner(leafletOutput(ns('elev_mapF'), height=750), type = 8, color = '#666666')),
               tabPanel('Spring',
                        withSpinner(leafletOutput(ns('elev_mapS'), height=750), type = 8, color = '#666666'))
             ))
    )
  )
  
}

elevation_change <- function(input, output, session) {

  fall_pal <- colorNumeric(palette = 'RdYlBu', domain = values(fall_chg), na.color = "transparent")
  
  output$elev_mapF <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>%
      addRasterImage(fall_chg, group = 'Surface', project = FALSE, colors = fall_pal) %>% 
      addPolygons(data = county, group = 'Solano County', color = '#666666', fill = FALSE) %>% 
      addCircles(data = fall_chg_wells, group = 'Wells', opacity = .8, radius = 20,
                 label = ~paste(WSE_F15_10, 'ft'), 
                 popup = ~paste('<b>County</b>', COUNTY, '<br>', '<b>Site ID</b>',SITE_CODE, '<br>', 
                                '<b>Elevation Change</b>', WSE_F15_10, 'ft')) %>% 
      addLayersControl(baseGroups = c('Map', 'Dark Map', 'Satelite'), overlayGroups = c('Surface', 'Wells', 'Solano County')) %>% 
      addLegend(title = 'Elevation Change', pal = fall_pal, values = values(fall_chg), position = 'bottomright',
                opacity = 1, labFormat = labelFormat(suffix = ' ft'))
  })
  
  
  output$elev_mapS <- renderLeaflet({
    spring_pal <- colorNumeric(palette = 'RdYlBu', domain = values(spring_chg), na.color = "transparent")
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>%
      addRasterImage(spring_chg, group = 'Surface', project = FALSE, colors = spring_pal) %>% 
      addPolygons(data = county, group = 'Solano County', color = '#666666', fill = FALSE) %>% 
      addCircles(data = spring_chg_wells, group = 'Wells', opacity = 1, radius = 20, 
                 popup = ~paste('<b>County</b>', COUNTY, '<br>', '<b>Site ID</b>',SITE_CODE, '<br>', 
                                '<b>Elevation Change</b>', WSE_S15_10, 'ft')) %>% 
      addLayersControl(baseGroups = c('Map', 'Dark Map', 'Satelite'), overlayGroups = c('Surface', 'Wells', 'Solano County'))%>% 
      addLegend(title = 'Elevation Change', pal = spring_pal, values = values(spring_chg), position = 'bottomright',
                opacity = 1, labFormat = labelFormat(suffix = ' ft'))
  })
  
}

