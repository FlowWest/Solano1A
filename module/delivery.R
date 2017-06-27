
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
      column(width = 3, 
             tags$h3("Solano County Water Delivery"),
             tags$p("Delivered water is defined here as the volume of water provided 
                    by a water resources management entity to meet a water demand. 
                    In this evaluation, delivered water is expressed annually for 2010
                    and 2015. This page shows 2010 and 2015 annual total water 
                    deliveries for the water resources management entities that 
                    provided data, and the breakdown between surface water and 
                    groundwater within those deliveries. In 2010, approximately 
                    295,000 acre-feet were delivered to users in Solano County, 
                    and approximately 10% of this was derived from groundwater. 
                    Total reported delivery increased in 2015 to approximately 328,000 
                    acre-feet with approximately 9% derived from groundwater sources. 
                    Agricultural water providers (Solano Irrigation District, 
                    Reclamation District 2068, and Maine Prairie Water District) 
                    deliver the most water in the County, and nearly all of their 
                    delivered water is from surface sources. The City of Dixon 
                    (including Cal Water Service area) is the largest groundwater delivery 
                    entity in the County; SID, Vacaville, RNVWD, Travis AFB, and Rio Vista 
                    also deliver groundwater."), 
             tags$br(),
             tags$p("The City of Davis is outside of Solano County, 
                    but was included in this summary (but not in County water balance totals 
                    in the following sections) as it is immediately adjacent to the County and 
                    has historically delivered a significant volume of groundwater. 
                    Davis will be transitioning from groundwater to Sacramento River 
                    surface water deliveries within the next few years, which could 
                    change groundwater level conditions in northeastern Solano County. ")),
      column(width = 4, 
             # need to figure out spatial component to delivery data
             withSpinner(leafletOutput(ns('delivery_map'), height = 800), 
                         type = 8, color = '#666666')),
      column(width = 5, 
             fluidRow(
               tabsetPanel(
                 tabPanel(title = "Delivery", 
                          withSpinner(plotlyOutput(ns("deliver_plot")), 
                                      type = 8, color = '#666666'))
               )
             ), 
             fluidRow(
               tabsetPanel(
                 tabPanel(title = "Summary", 
                          DT::dataTableOutput(ns("delivery_summary")))
               ))))
    )
}

# Server ----------------------------------------------------------------------
delivery <- function(input, output, session) {
  
  deliv_pal <- colorFactor(palette = 'Accent', domain = deliv_entities$Name)
  
  
  map_events <- reactiveValues(clicked_shape = NULL)
  
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
                  label = ~Name, fillOpacity = .6, 
                  group =~entty_t) %>% 
      addLayersControl(baseGroups = c('Map', 'Satellite'), 
                      overlayGroups = paste(deliv_entities$entty_t))
  })
  
  output$deliver_plot <- renderPlotly({
    
    # TODO: logic for where to show entity label, needs more work
    # currently it just checks to see if a click is not null indicating 
    # that user selected an entity
    if (is.null(map_events$clicked_shape)) {
      deliveries() %>% 
        mutate(entity_labels = abbreviate(`Water Resources Management Entity`, minlength = 15)) %>% 
        plot_ly(x=~entity_labels, y=~value, color=~reporting_type, 
                type='bar', colors = "Dark2", source = 'source', key = ~shape_ref_attr) %>% 
        layout(xaxis = list(title="", tickangle = -45, ticklen = 1, tickfont = 5),
               yaxis = list(title="Delivered (ac-ft)"),
               margin = list(pad = 0, b = 90), 
               dragmode = "zoom")
    } else {
      deliveries() %>% 
        mutate(entity_labels = abbreviate(`Water Resources Management Entity`, minlength = 15)) %>% 
        plot_ly(x=~entity_labels, y=~value, color=~reporting_type, 
                type='bar', colors = "Dark2", source = 'source', key = ~shape_ref_attr) %>% 
        layout(title=paste(deliveries()$`Water Resources Management Entity`[1]), 
               xaxis = list(title="", tickangle = -45, ticklen = 1, 
                            tickfont = 5, showticklabels = FALSE),
               yaxis = list(title="Delivered (ac-ft)"),
               margin = list(pad = 0, b = 90), 
               dragmode = "zoom")
    }
    
  })
  
  output$delivery_summary <- DT::renderDataTable({
    validate(
      need(!is.null(map_events$clicked_shape), "Select a delivery entity")
    )
    summ <- summary_data() %>% dplyr::select(-c(`Water Resources Management Entity`, 
                                               shape_ref_attr))
    summ$percent_of_total <- pretty_num(summ$percent_of_total * 100)
    colnames(summ) <- c("Year", "Delivery Type", "Delivered Amount", "Year Total", "Percent of Total")
    return(DT::datatable(summ, options = list(dom = 't', 
                                              columnDefs = list(list(className = 'dt-center', targets = 4)))))
  })
  
  # leaflet proxy redraws hover piece from plotly delivery plot
  observe({
    leafletProxy("delivery_map", data = selected_ents()) %>% 
      clearGroup("hl_layer") %>% 
      addPolygons(fill = FALSE, color = '#666666',
                  opacity = 1, group = "hl_layer") 
  })
  
}