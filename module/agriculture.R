cropsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h2('Crops')),
      column(width = 6,
             tabsetPanel(
               tabPanel('Area',
                        withSpinner(plotlyOutput(ns('crop_areas'), height = 725), type = 8, color = '#666666')),
               tabPanel('Demand',
                        withSpinner(plotlyOutput(ns('crop_demands'), height = 725), type = 8, color = '#666666'))
             ))
    )
  )
}

crops <- function(input, output, session) {
  
  output$crop_areas <- renderPlotly({
    crop_acres %>% 
      plot_ly(y = ~`2015 Summary Crop Type`, x = ~`Total Area (acres) in Solano County for 2010`, type = 'bar', name = '2010',
              orientation = 'h', hoverinfo = 'text', marker = list(color = 'rgba(136,65,157, 1)'),
              text = ~paste('2010', `2015 Summary Crop Type`, '<br>', 
                            'Total Acres', pretty_num(`Total Area (acres) in Solano County for 2010`, 0))) %>% 
      add_bars(x = ~`Total Area (acres) in Solano County for 2015`, name = '2015', marker = list(color = 'rgba(140,150,198, 1)'),
               hoverinfo = 'text', 
               text = ~paste('2015', `2015 Summary Crop Type`, '<br>', 
                             'Total Acres', pretty_num(`Total Area (acres) in Solano County for 2015`, 0), '<br>', 
                             'Change in Acres', pretty_num(`Change in Summary Crop Acreage (2015--2010)`, 0))) %>% 
      layout(yaxis = list(title = ''), xaxis = list(title = 'Solano County Total Area (acres)'),
             margin = list(l = 175)) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$crop_demands <- renderPlotly({
    crop_demand %>% 
      plot_ly(y = ~`2015 Summary Crop Type`, x = ~`2010 Total Agricultural Applied Water Demand (acre-feet / year)`, type = 'bar', name = '2010',
              orientation = 'h', hoverinfo = 'text', marker = list(color = 'rgba(136,65,157, 1)'),
              text = ~paste('2010', `2015 Summary Crop Type`, '<br>',
                            'Demand', pretty_num(`2010 Total Agricultural Applied Water Demand (acre-feet / year)`, 0), 'AF/year')) %>% 
      add_bars(x = ~`2015 Total Agricultural Applied Water Demand (acre-feet / year)`, name = '2015', 
               marker = list(color = 'rgba(140,150,198, 1)'),
               hoverinfor = 'text',
               text = ~paste('2015', `2015 Summary Crop Type`, '<br>',
                             'Demand', pretty_num(`2015 Total Agricultural Applied Water Demand (acre-feet / year)`, 0), 'AF/year', '<br>',
                             'Change in Demand', pretty_num(`Change in Applied Water Demand  (2015--2010)`, 0), 'AF/year')) %>% 
      layout(yaxis = list(title = ''), xaxis = list(title = 'Solano County Total Agricultural Applied Water Demand (acre-feet / year)'),
             margin = list(l = 175)) %>% 
      config(displayModeBar = FALSE)
  })
  
}

model_awUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h2('Modeled Agricultural Applied Water Demand (AF/acre)'),
             tags$h6('blah blah blah')),
      column(width = 6,
             fluidRow(
               column(width = 12,
                      tabsetPanel(
                        tabPanel('2010',
                                 withSpinner(leafletOutput(ns('cup10'), height = 600), type = 8, color = '#666666')),
                        tabPanel('2015',
                                 withSpinner(leafletOutput(ns('cup15'), height = 600), type = 8, color = '#666666'))
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      tags$img(src = 'legend.png', width = '50%'),
                      tags$p('*Pasture is only modeled as irrigated within Solano Irrigation District, 
                             Reclamation District 2068, and Maine Prairie Water District service areas')
               )
             )
      )
    ))
}

model_aw <- function(input, output, session) {
  pal <- colorNumeric(palette = c('#ffffcc', '#a1dab4', '#41b6c4', '#225ea8'), domain = values(CUP_2010), na.color = "transparent")
  
  output$cup10 <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>%
      addRasterImage(CUP_2010, group = '2010', colors = pal) %>% 
      addPolygons(data = county, group = 'Solano County', color = '#666666', fill = FALSE) %>% 
      addLayersControl(baseGroups = c('Map', 'Dark Map', 'Satelite'), overlayGroups = c('2010', 'Solano County')) 
  })
  
  output$cup15 <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>%
      addRasterImage(CUP_2015, group = '2015', colors = pal) %>% 
      addPolygons(data = county, group = 'Solano County', color = '#666666', fill = FALSE) %>% 
      addLayersControl(baseGroups = c('Map', 'Dark Map', 'Satelite'), overlayGroups = c('2015', 'Solano County')) 
  })
}

demandUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h2('Modeled Agricultural Applied Water Demand (AF/acre)'),
             tags$h6('blah blah blah')),
      column(width = 6,
             withSpinner(leafletOutput(ns('roi_map'), height=750), type = 8, color = '#666666'))
    )
  )
}

demand <- function(input, output, session) {
  
  pal2015 <- colorFactor(palette = 'Dark2', domain = ROIs$AWD2015_AF)
  
  output$roi_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>% 
      addPolygons(data = ROIs, color = ~pal2015(AWD2015_AF), 
                  label = ~Name,
                  popup = ~paste('<b>Total Acres</b> <br>', pretty_num(Acres, 0), '<br><br>',
                                 '<b> Applied Water Demand </b><br>',
                                 '<b style = padding-left:10px;>2010</b>', pretty_num(AWD2010_AF, 0), '<em>AF/acre</em>',
                                 '<br><b style = padding-left:10px;>2015</b>', pretty_num(AWD2015_AF, 0),'<em>AF/acre</em>'),
                  group = 'Regions') %>% 
      addPolygons(data = sub_basin, group ='Solano Sub Basin') %>% 
      addPolygons(data = county, group = 'Solano County', color = '#666666', fill = FALSE) %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'), overlayGroups = c("Regions", 'Solano County', 'Solano Sub Basin')) %>% 
      hideGroup('Solano Sub Basin')
  })
  
}


