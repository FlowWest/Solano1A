to_xts_object <- function(.d) {
  x <- xts::xts(.d$wse, order.by = .d$MEASUREMENT_DATE)
  colnames(x) <- "water surface elevation"
  x
}

casgem_dataUI <- function(id) {
  ns <- NS(id) 
  
  tagList(
    tags$h4("Groundwater Data", style="display: inline;"),
    tags$em(tags$p("available well data from Solano DWR Water Data Library and 
           CASGEM database", style="display: inline; font-size=14px;")),
    fluidRow(
      column(width = 12, leafletOutput(ns("casgem_map")))
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
             !is.na(wse), wse >= 0)
  })
  
  output$casgem_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(data=casgem_metadata, 
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
              showlegend = FALSE
              #, 
              #fill="tozeroy", 
              #fillcolor = 'rgba(31,120,180, 0.2)'
              ) %>%
      add_markers(y=~wse, marker=list(color="#668ac4"), name="Water Surface Elevation", 
                  showlegend = TRUE) %>%
      add_lines(y= ~fitted(loess(wse ~ as.numeric(MEASUREMENT_DATE))), 
                line = list(color="#b2df8a", width=5), name = "Fitted Trend (loess)", 
                showlegend = TRUE) %>%
      layout(xaxis=list(title=""),
             yaxis=list(title="Water Surface Elevation (ft)"),
             showlegend = TRUE)

  
})}

elevation_changeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h4('Groundwater Elevation Change (ft)')),
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

