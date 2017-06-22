ground_waterUI <- function(id) {
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

ground_water <- function(input, output, session) {
  
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
                  )) %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'))
  })
  
}