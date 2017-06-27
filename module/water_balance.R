water_balanceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6, 
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
                    both urban and agricultural demands.  ")), 
      column(width = 6, 
             tabsetPanel(
               tabPanel(title="2010", 
                        fluidRow(plotlyOutput(ns("water_balance_plot_2010"), 
                                              height = 600))), 
               tabPanel(title="2015", 
                        fluidRow(plotlyOutput(ns("water_balance_plot_2015"), 
                                              height = 600)))
             ))
    )
  )
}

water_balance <- function(input, output, session) {
  
  output$water_balance_plot_2010 <- renderPlotly({
    balance_data %>% 
      filter(year == "2010") %>% 
      plot_ly(x=~display_label, y=~volume, color=~entity_type, type='bar', colors="Accent") %>% 
      layout(title="2010 Water Balance", 
             xaxis = list(title =""), 
             margin = list(b=80))
  })
  
  output$water_balance_plot_2015 <- renderPlotly({
    balance_data %>% 
      filter(year == "2015") %>% 
      plot_ly(x=~display_label, y=~volume, color=~entity_type, type='bar', colors="Accent") %>% 
      layout(title="2015 Water Balance", 
             xaxis = list(title =""), 
             margin = list(b=80))
  })
}