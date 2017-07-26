cropsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h4('Crops'),
             tags$br(),
             tags$p('We used the California Department of Water Resources Consumptive Use Program PLUS (CUP+ Version 6.81; Orang, M. et al., 2016) to estimate applied water for agricultural crops in Solano County for 2010 and 2015 (as mapped by the U.S Department of Agriculture’s CropScape program). The inputs to the CUP+ model are climate, crop, and soil data, and the model calculates annual applied water per unique combination of crop and soil type for a given year of climate data, which can vary depending on climatic factors such as solar radiation, temperature, humidity, wind, and precipitation. Results are given as evotranspiration of applied water (ETaw), which is an estimate of the net applied water required to produce a given crop under the defined soil and climatic conditions. '),
             tags$p('This tab shows crop acreages and model results for the most significant crop types in terms of acreage in the County. It is important to note the potential changes within applied water demand for a single crop type. For example, the evapotranspirative applied water demand for almonds increased from 2.02 acre-feet/acre in 2010 to 2.49 acre-feet/acre in 2015. This highlights how changes in climate can impact the required water for a given crop. Certain crops, however, did not demonstrate as much sensitivity to climate. For example, grain crop evapotranspirative applied water demand changed very slightly from 2010 to 2015 in spite of different climate conditions.'),
             tags$p('Other crops types for which agricultural applied water demand was calculated, but are not presented in the table due to smaller acreages are:', 
                    tags$br(),'sod / grass seed, asparagus, oranges, olives, onion, truck crops, potatoes, dry beans, lettuce, and “other crops”—as categorized by the USDA.'),
             tags$p('Crop types with a “+” include the following crops per type.', tags$br(),
                    'Alfalfa+ category includes: Alfalfa, Rice', tags$br(),
                    'VetchCorn+ category includes: Corn, Sorghum, Sweet Corn, Double Crop Oats/Corn', tags$br(),
                    'Sunflower+ category includes: Sunflower, Cotton')),
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
      dplyr::filter(`2015 Summary Crop Type` %in% 
                      c('Alfalfa+','Almonds','Corn+','Deciduous fruits and nuts','Grains','Safflower',
                        'Sunflower+','Tomatoes','Wine grapes', 'Winter wheat')) %>% 
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
      dplyr::filter(`2015 Summary Crop Type` %in% 
                      c('Alfalfa+','Almonds','Corn+','Deciduous fruits and nuts','Grains','Safflower',
                        'Sunflower+','Tomatoes','Wine grapes', 'Winter wheat')) %>% 
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
             tags$h4('Modeled Agricultural Applied Water Demand (AF/acre)'),
             tags$br(),
             tags$p('We used the California Department of Water Resources Consumptive Use Program PLUS 
                    (CUP+ Version 6.81; Orang, M. et al., 2016) to estimate applied water for agricultural 
                    crops in Solano County for 2010 and 2015 (as mapped by the U.S Department of Agriculture’s 
                    CropScape program). The inputs to the CUP+ model are climate, crop, and', 
                    tags$a(href = "https://websoilsurvey.sc.egov.usda.gov/App/HomePage.htm", target="_blank", 'soil data'), 
                    'and the 
                    model calculates annual applied water per unique combination of crop and soil type for a given
                    year of climate data, which can vary depending on climatic factors such as solar radiation, 
                    temperature, humidity, wind, and precipitation. Results are given as evotranspiration of 
                    applied water (ETaw), which is an estimate of the net applied water required to produce a given 
                    crop under the defined soil and climatic conditions.'),
             tags$p('We integrated the CUP+ applied water results in a Geographic Information System (GIS) to 
                    facilitate comparisons between agricultural water demand and water deliveries (surface and groundwater) 
                    within water resources management entity service areas in the County.')),
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
             fluidRow(
               column(width = 12, 
                      tags$h4('Modeled Agricultural Applied Water Demand (AF/acre)'),
                      tags$br(),
                      tags$p('We used the California Department of Water Resources Consumptive Use Program PLUS 
                    (CUP+ Version 6.81; Orang, M. et al., 2016) to estimate applied water for agricultural 
                    crops in Solano County for 2010 and 2015 (as mapped by the U.S Department of Agriculture’s 
                    CropScape program). The inputs to the CUP+ model are climate, crop, and soil data, and the 
                    model calculates annual applied water per unique combination of crop and soil type for a given
                    year of climate data, which can vary depending on climatic factors such as solar radiation, 
                    temperature, humidity, wind, and precipitation. Results are given as evotranspiration of 
                    applied water (ETaw), which is an estimate of the net applied water required to produce a given 
                    crop under the defined soil and climatic conditions.'),
                      tags$p('We integrated the CUP+ applied water results in a Geographic Information System (GIS) to 
                    facilitate comparisons between agricultural water demand and water deliveries (surface and groundwater) 
                    within water resources management entity service areas in the County.'))), 
             fluidRow(
               column(width = 12, 
                      tabsetPanel(
                        tabPanel(title="2010", plotlyOutput(ns("demand_bars_2010"))), 
                        tabPanel(title="2015", plotlyOutput(ns("demand_bars_2015")))
                      )))),
      column(width = 6,
             withSpinner(leafletOutput(ns('roi_map'), height=750), type = 8, color = '#666666'))
    )
  )
}

# 
# County
# 
# Solano Sub-basin within the County
# SID
# MPWD
# RD 2068
# 
# 
# 2010 and 2015 Demand volumes are here:
#   U:\Active Projects\Solano_County_Water_Resources\Dash\solano-flat-files_revised\AppliedWaterDemand_updated.csv

demand <- function(input, output, session) {
  
  pal2015 <- colorFactor(palette = 'Dark2', domain = ROIs$AWD2015_AF)
  palent <- colorFactor(palette = 'Dark2', domain = sub_deliv_ent$Acronym)
  
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
      addPolygons(data = sub_basin, group ='Solano Sub Basin', label = ~Boundary,
                  popup = ~paste('<b>Total Acres</b> <br>', pretty_num(Acres, 0), '<br><br>',
                                 '<b> Applied Water Demand </b><br>',
                                 '<b style = padding-left:10px;>2010</b>', pretty_num(`2010 (Acre-ft)`, 0), '<em>AF/acre</em>',
                                 '<br><b style = padding-left:10px;>2015</b>', pretty_num(`2015 (Acre-ft)`, 0),'<em>AF/acre</em>')) %>% 
      addPolygons(data = sub_deliv_ent, group = 'Delivery Entities', color = ~palent(Acronym),
                  label = ~Acronym,
                  popup = ~paste('<b>Total Acres</b> <br>', pretty_num(Acres, 0), '<br><br>',
                                 '<b> Applied Water Demand </b><br>',
                                 '<b style = padding-left:10px;>2010</b>', pretty_num(`2010 (Acre-ft)`, 0), '<em>AF/acre</em>',
                                 '<br><b style = padding-left:10px;>2015</b>', pretty_num(`2015 (Acre-ft)`, 0),'<em>AF/acre</em>')) %>% 
      addPolygons(data = county, group = 'Solano County', color = '#666666', fill = FALSE) %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite'), overlayGroups = c("Regions", 'Solano County', 'Solano Sub Basin', 'Delivery Entities'))
  })
  
  output$demand_bars_2010 <- renderPlotly({
    # demand plots 
    applied_demand %>% 
      filter(year== "2010") %>% 
      plot_ly(x=~fct_inorder(display_name), y=~demand_acre_ft, type='bar', color=~Boundary, colors="Accent") %>% 
      layout(margin = list(b=80), 
             xaxis = list(title=""), 
             yaxis = list(title="Volume (acre-feet)",
                          range = c(0, 300000)),
             showlegend=FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$demand_bars_2015 <- renderPlotly({
    # demand plots 
    applied_demand %>% 
      filter(year== "2015") %>% 
      plot_ly(x=~fct_inorder(display_name), y=~demand_acre_ft, type='bar', color=~Boundary, colors="Accent") %>% 
      layout(margin = list(b=80), 
             xaxis = list(title=""), 
             yaxis = list(title="Volume (acre-feet)",
                          range = c(0, 300000)),
             showlegend=FALSE) %>% 
      config(displayModeBar = FALSE)
  })
  
}


