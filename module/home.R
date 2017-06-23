homeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h3('Water Resources and Groundwater Basin App')),
      column(width = 6,
             withSpinner(leafletOutput(ns('ROI'), height=750), type = 8, color = '#666666')
             )
    ),
    fluidRow(
      column(width = 12, id = 'credits',
             tags$a(tags$img(src = 'greylogo.png', width = '175px', style = 'display:inline-block;'),
                    href = 'http://www.flowwest.com/', target = '_blank'),
             tags$h5('App created and maintained by:', tags$a(href = 'mailto:erodriguez@flowwest.com', 'Emanuel Rodriguez', target = '_blank'),
                     'and', tags$a(href = 'mailto:sgill@flowwest.com', 'Sadie Gill', target = '_blank'), 
                     style = 'display:inline-block; margin-left:15px;'),
             tags$a(tags$img(src = 'GitHub-Mark-32px.png'), href = 'https://github.com/FlowWest/Solano1A', target = '_blank', style = 'margin-left:15px;')
      ))
  )
}

home <- function(input, output, session) {

  pal <- colorFactor(palette = 'Dark2', domain = ROIs$Name)
  output$ROI <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Thunderforest.Outdoors, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>% 
      addPolygons(data = ROIs, color = ~pal(Name), group = 'Name', label = ~Name, 
                  popup = ~paste('<b>', Name, '</b>', '<br>', pretty_num(Acres, 0), ' Acres')) %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'), overlayGroups = c('Name'))
  })  
}

