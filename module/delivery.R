# UI ----------------------------------------------------------------------------
deliveryUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # top row host map
    fluidRow(
      column(width = 12, 
             # need to figure out spatial component to delivery data
             withSpinner(leafletOutput(ns('delivery_map')), type = 8, color = '#666666'))
    ), 
    
    # bottom row host plots
    fluidRow(
      column(width = 12, 
             tabsetPanel(
               tabPanel(title = "Delivery", 
                        withSpinner(plotlyOutput(ns("deliver_plot")), 
                        type = 8, color = '#666666')), 
               tabPanel(title = "Demand")
             ))  
    )
  )
}

# Server ----------------------------------------------------------------------
delivery <- function(input, output, session) {
  
  
  pal <- colorFactor(palette = 'Dark2', domain = ROIs$Name)
  # need to figure out spatial component to delivery data
  output$delivery_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Thunderforest.Outdoors, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>% 
      # basic polygon added needs work!
      addPolygons(data = deliv_entities, fillColor=~Name, 
                  layerId=~Name, weight = 2, 
                  popup = ~Name) %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'), overlayGroups = c('Name'))
  })
  
  output$deliver_plot <- renderPlotly({
    solano_deliveries %>% 
      mutate(entity_labels = abbreviate(`Water Resources Management Entity`, minlength = 15)) %>% 
      plot_ly(x=~entity_labels, y=~value, color=~year, 
              type='bar', colors = "Set2") %>% 
      layout(xaxis = list(title="", tickangle = -45, ticklen = 1, tickfont = 5),
             margin = list(pad = 0, b = 90))
  })
}