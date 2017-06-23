# UI ----------------------------------------------------------------------------
deliveryUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # top row host map
    fluidRow(
      column(width = 6, 
             # need to figure out spatial component to delivery data
             withSpinner(leafletOutput(ns('delivery_map'), height = 800), 
                         type = 8, color = '#666666')), 
      column(width = 6, 
             fluidRow(
               tabsetPanel(
                 tabPanel(title = "Delivery", 
                          withSpinner(plotlyOutput(ns("deliver_plot")), 
                                      type = 8, color = '#666666'))
               )
             ), 
             fluidRow(
               tabPanel(title = "Demand")
             )
             )
    )

  )
}

# Server ----------------------------------------------------------------------
delivery <- function(input, output, session) {
  
  map_events <- reactiveValues(clikced_shape = NULL)
  
  # listen for a click on a shape file
  observeEvent(input$delivery_map_shape_click, {
    map_events$clicked_shape <- input$delivery_map_shape_click$id
  })
  
  # click on background causes reset
  observeEvent(input$delivery_map_click, {
    map_events$clicked_shape <- NULL
  })
  
  deliveries <- reactive({
    if (is.null(map_events$clicked_shape)) {
      return(solano_deliveries)
    }
    solano_deliveries %>% 
      filter(shape_ref_attr == map_events$clicked_shape)
  })
  
  selected_ents <- reactive({
    d <- event_data("plotly_hover", source = 'source')
    
    entity <- as.character(d[['key']])
    
    return(deliv_entities[deliv_entities$Name == entity, ])
  })
  
  # need to figure out spatial component to delivery data
  output$delivery_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Thunderforest.Outdoors, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>% 
      # basic polygon added needs work!
      addPolygons(data = deliv_entities, fillColor=~Name, 
                  layerId=~Name, weight = 2, 
                  label = ~Name) %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'))
  })
  
  output$deliver_plot <- renderPlotly({
    deliveries() %>% 
      mutate(entity_labels = abbreviate(`Water Resources Management Entity`, minlength = 15)) %>% 
      plot_ly(x=~entity_labels, y=~value, color=~year, 
              type='bar', colors = "Set2", source = 'source', key = ~shape_ref_attr) %>% 
      layout(xaxis = list(title="", tickangle = -45, ticklen = 1, tickfont = 5),
             margin = list(pad = 0, b = 90), 
             dragmode = "zoom")
  })
  
  observe({
    leafletProxy("delivery_map", data = selected_ents()) %>% 
      clearGroup("hl_layer") %>% 
      addPolygons(fill = FALSE, color = '#FFFF00',
                  opacity = 1, group = "hl_layer") 
  })
  
}