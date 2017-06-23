demandUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h2('Ground Water')),
      column(width = 6,
             withSpinner(leafletOutput(ns('roi_map'), height=750), type = 8, color = '#666666'))
    )
  )
}

demand <- function(input, output, session) {
  
  pal2015 <- colorFactor(palette = 'Dark2', domain = ROIs$AWD2015_AF)
  
  output$roi_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Thunderforest.Outdoors, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>% 
      addPolygons(data = ROIs, color = ~pal2015(AWD2015_AF), 
                  label = ~Name,
                  labelOptions = labelOptions(textOnly = TRUE),
                  popup = ~paste('<b>Total Acres</b> <br>', pretty_num(Acres, 0), '<br><br>',
                                 '<b style = > Applied Water Demand </b><br>',
                                 '<b style = padding-left:10px;>2010</b>', pretty_num(AWD2010_AF, 0), '<em>acre-feet/acre</em>',
                                 '<br><b style = padding-left:10px;>2015</b>', pretty_num(AWD2015_AF, 0), '<em>acre-feet/acre</em>'
                  ), group = 'Regions of Interest') %>% 
      addPolygons(data = sub_basin, group ='Solano Sub Basin') %>% 
      addRasterImage(fall_chg, group = 'Fall') %>% 
      addRasterImage(spring_chg, group = 'Spring') %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'), overlayGroups = c("Regions of Interest", 'Solano Sub Basin')) %>% 
      hideGroup('Solano Sub Basin')
  })
  
}

elevation_changeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h2('Groundwater Elevation Change (ft)')),
      column(width = 6,
             withSpinner(leafletOutput(ns('elev_map'), height=750), type = 8, color = '#666666'))
    )
  )
  
}

elevation_change <- function(input, output, session) {
  
  spring_pal <- colorNumeric(palette = 'RdYlBu', domain = values(spring_chg), na.color = "transparent")
  fall_pal <- colorNumeric(palette = 'RdYlBu', domain = values(fall_chg), na.color = "transparent")
  
  output$elev_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Thunderforest.Outdoors, group = 'Map') %>% 
      addRasterImage(fall_chg, group = 'Fall', project = FALSE, colors = fall_pal) %>% 
      addCircles(data = fall_chg_wells, group = 'Fall Wells') %>% 
      addRasterImage(spring_chg, group = 'Spring', project = FALSE, colors = spring_pal) %>% 
      addCircles(data = spring_chg_wells, group = 'Spring Wells') %>% 
      addLayersControl(overlayGroups = c('Fall', 'Fall Wells', 'Spring', 'Spring Wells')) %>% 
      hideGroup('Spring')
  })
}


