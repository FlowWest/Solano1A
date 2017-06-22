agricultureUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6),
      column(width = 6,
             withSpinner(ns('cup'), type = 8, color = '#666666'))
    )
  )
}

agriculture <- function(input, output, session) {
  output$cup <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Thunderforest.Outdoors, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>%
      addRasterImage(CUP_2010, group = '2010') %>% 
      addRasterImage(CUP_2015, group = '2015') %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'), overlayGroups = c('2010', '2015'))
  })
}