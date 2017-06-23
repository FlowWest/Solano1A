model_awUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h2('Modeled Applied Water Dmand (AF/acre)'),
             tags$h6('blah blah blah')),
      column(width = 6,
             fluidRow(
               column(width = 12,
                      withSpinner(leafletOutput(ns('cup'), height = 600), type = 8, color = '#666666'))
             ),
             fluidRow(
               column(width = 12,
                      tags$img(src = 'legend.png', width = '50%'),
                      tags$p('*Pasture is only modeled as irrigated within Solano irrigation district, 
                             Reclamation district 2068, and maine prairie water district service areas')
               )
             )
      )
    )
  )
}

model_aw <- function(input, output, session) {
  pal <- colorNumeric(palette = c('#ffffcc', '#a1dab4', '#41b6c4', '#225ea8'), domain = values(CUP_2010), na.color = "transparent")
  
  output$cup <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>%
      addRasterImage(CUP_2010, group = '2010', colors = pal) %>% 
      addRasterImage(CUP_2015, group = '2015', colors = pal) %>% 
      addLayersControl(baseGroups = c('Map', 'Dark Map', 'Satelite'), overlayGroups = c('2010', '2015'),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup('2015') 
  })
}

cropsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h2('Crops'))
    )
  )
}

crops <- function(input, output, session) {
  
}