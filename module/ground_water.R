elevation_changeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h2('Groundwater Elevation Change (ft)')),
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

