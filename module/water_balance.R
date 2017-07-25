water_balanceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 4, 
             tags$h3("Water Balance Summary"), 
             tags$br(),
             tags$p("These charts summarize the estimated annual water balance for 
                    Solano County in 2010 and 2015. We assume that all of the 
                    agricultural applied water demand outside of the service 
                    areas of water management entities is derived from groundwater. 
                    In 2010, of the 340,530 acre-feet of total water 
                    demand (agricultural + urban), up to 115,991 acre-feet (34.1%) 
                    could be supplied from groundwater sources. Of the 252,744 acre 
                    feet of agricultural applied water demand, up to 99,806 
                    acre-feet (39.5%) could be supplied from groundwater sources. 
                    Again, it is important to note that nearly all of the groundwater 
                    supply for agriculture can be attributed to assumed groundwater 
                    use in the Dixon Ridge and Southeast regions that are outside of 
                    water resources management entity service areas. Finally, of the 
                    87,786 acre-feet of urban water demand, 16,185 acre-feet (18.4%) 
                    is supplied from groundwater sources. We also show how the water 
                    balance changed between 2010 and 2015. While urban water demand 
                    decreased by 11% between 2010 and 2015, countywide and agricultural 
                    demand increased, as did the proportion of groundwater supply for 
                    both urban and agricultural demands.  "), 
             DT::dataTableOutput(ns("percent_change_table"))), 
      column(width = 8,
             plotlyOutput(ns("water_balance_plot_2010"), 
                          height = 600)
             )
    )
  )
}

water_balance <- function(input, output, session) {
  
  percent_change_summary <- reactive({
    balance_data %>% distinct(Entity, `Percent Change`) %>% 
      dplyr::select(Entity, "Percent Change from 2010 to 2015" = `Percent Change`)
  })
  
  output$percent_change_table <- DT::renderDataTable({
    percent_change_summary()
  }, options = list(dom = 't', columnDefs = list(list(className = 'dt-center')), 
                    rownames = FALSE))
  
  output$water_balance_plot_2010 <- renderPlotly({
    balance_data %>% 
      plot_ly(x=~fct_inorder(display_label), y=~volume, 
              color=~as.character(year), type='bar', colors="Accent", 
              text=~paste("<b>Volume (acre-feet)</b>:", round(volume)), 
              hoverinfo = "text") %>% 
      layout(title="2010 and 2015 Water Balance", 
             xaxis = list(title =""), 
             yaxis = list(title = "Volume (acre-feet)"),
             margin = list(b=80)) %>% 
      config(displayModeBar = FALSE)
  })
  
  # output$water_balance_plot_2015 <- renderPlotly({
  #   balance_data %>% 
  #     filter(year == "2015") %>% 
  #     plot_ly(x=~fct_inorder(display_label), y=~volume, color=~entity_type, type='bar', colors="Accent") %>% 
  #     layout(title="2015 Water Balance", 
  #            xaxis = list(title =""), 
  #            yaxis = list(title = "Volume  (acre-feet)",
  #                         range = c(0, 400000)),
  #            margin = list(b=80)) %>% 
  #     config(displayModeBar = FALSE)
  # })
}