deliv_pal <- colorFactor(palette = 'Accent', domain = deliv_entities$Name)

entity_summary <- function(.d) {
  year_totals <- .d %>% 
    group_by(`Water Resources Management Entity`, year) %>% 
    summarise(
      year_total = sum(value)
    ) %>% ungroup() %>% dplyr::select(`Water Resources Management Entity`,year, year_total)
  
  percent_of_total <- left_join(.d, year_totals) %>% 
    mutate(percent_of_total = value / year_total)
  
  return(percent_of_total)
}

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
               tabsetPanel(
                 tabPanel(title = "Demand", 
                          DT::dataTableOutput(ns("delivery_summary")))
               ))))
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
      return(solano_deliveries %>% 
               unite(reporting_type, water_type, year, remove = FALSE, sep = " "))
      
    }
    solano_deliveries %>% 
      filter(shape_ref_attr == map_events$clicked_shape) %>% 
      unite(reporting_type, water_type, year, remove = FALSE, sep = " ")
  })
  
  summary_data <- reactive({
    if (is.null(map_events$clicked_shape)) {
      return(solano_deliveries %>% 
               entity_summary())
      
    }
    solano_deliveries %>% 
      filter(shape_ref_attr == map_events$clicked_shape) %>% 
      entity_summary()
  })
  
  selected_ents <- reactive({
    d <- event_data("plotly_hover", source = 'source')
    
    entity <- as.character(d[['key']])
    
    return(deliv_entities[deliv_entities$Name == entity, ])
  })
  
  # need to figure out spatial component to delivery data
  output$delivery_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>% 
      # basic polygon added needs work!
      addPolygons(data = deliv_entities, 
                  color = ~deliv_pal(Name), 
                  fill = TRUE, 
                  layerId=~Name, weight = 2, 
                  label = ~Name, fillOpacity = .6) %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'))
  })
  
  output$deliver_plot <- renderPlotly({
    deliveries() %>% 
      mutate(entity_labels = abbreviate(`Water Resources Management Entity`, minlength = 15)) %>% 
      plot_ly(x=~entity_labels, y=~value, color=~reporting_type, 
              type='bar', colors = "Dark2", source = 'source', key = ~shape_ref_attr) %>% 
      layout(xaxis = list(title="", tickangle = -45, ticklen = 1, tickfont = 5),
             margin = list(pad = 0, b = 90), 
             dragmode = "zoom")
  })
  
  output$delivery_summary <- DT::renderDataTable({
    validate(
      need(!is.null(map_events$clicked_shape), "Select a delivery entity")
    )
    summ <- summary_data() %>% dplyr::select(-c(`Water Resources Management Entity`, 
                                               shape_ref_attr))
    return(summ)
  })
  
  # leaflet proxy redraws hover piece from plotly delivery plot
  observe({
    leafletProxy("delivery_map", data = selected_ents()) %>% 
      clearGroup("hl_layer") %>% 
      addPolygons(fill = FALSE, color = '#666666',
                  opacity = 1, group = "hl_layer") 
  })
  
}