homeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             tags$h3('Solano County Water Resources App'),
             tags$br(),
             tags$p('This app intends to improve understanding of the status and trends of 
                    groundwater basins and surface water systems in the County, and support the development of data-driven 
                    strategies for ongoing Solano County engagement in SGMA compliance. 
                    This app was developed to share the findings of the', 
                    tags$a(href="https://s3-us-west-2.amazonaws.com/solano1a-project/SolanoWaterResourcesStudy_FINAL_updatedAppLink.pdf",
                           target="_blank",
                           'Solano County Water Resources and Groundwater Basin Study'), 
                    'which involved:'),
             tags$ol(
               tags$li('A review of existing surface, groundwater, and agricultural information and data '),
               tags$li('Integration of this data to develop a high-level picture of Solano County water resources'),
               tags$li('Completion of agricultural consumptive use modeling to quantify water demand throughout the County based on crop type'),
               tags$li('Comparison of water demands and deliveries in a high-level water balance'),
               tags$li('Historical groundwater level trend analysis')),
             tags$br(),
             tags$p('Each tab delves into a different aspect of the water resources picture in the County:'),
             tags$ul(
               tags$li(tags$b('Deliveries'), ' - Provides stakeholders with delivery volumes and sources from water management 
                       entities across the County, and how those values changed between 2010 and 2015.'),
               tags$li(tags$b("Agriculture"), ' - Users can view agricultural applied water demand results from 
                       the CUP+ model and compare changes in acreage and demand by crop type between 2010 and 2015.'),
               tags$li(tags$b("Water Balance"), ' - By comparing water demand and deliveries, we developed 
                    an initial picture of the water balance in areas with and without water delivery systems in 2010 and
                       2015. The water balance indicates that county-wide water demand increased 6% from 2010 to 2015, 
                       while county-wide water supply from groundwater increased by 9% over the same period'),
               tags$li(tags$b("Groundwater"), ' - Shows how well records indicate greater decreases in groundwater 
                       elevations along the northern extent of the County.')),
             tags$br(),
             tags$a(tags$img(src = 'TransLogoTreb.png', width = '150px', style = 'display:inline-block;'),
                    href = 'http://www.flowwest.com/', target = '_blank'),
             tags$h6('App created and maintained by:', tags$a(href = 'mailto:erodriguez@flowwest.com', 'Emanuel Rodriguez', target = '_blank'),
                     'and', tags$a(href = 'mailto:sgill@flowwest.com', 'Sadie Gill', target = '_blank'), 
                     style = 'display:inline-block; margin-left:15px;'),
             tags$a(tags$img(src = 'GitHub-Mark-32px.png'), href = 'https://github.com/FlowWest/Solano1A', target = '_blank', style = 'margin-left:15px;')
             ),
      column(width = 6,
             withSpinner(leafletOutput(ns('ROI'), height=750), type = 8, color = '#666666')
             )
    )
  )
}

home <- function(input, output, session) {

  pal <- colorFactor(palette = 'Dark2', domain = deliv_entities$Name)
  pal2 <- colorFactor(palette = 'Dark2', domain = groundwater_basins$Basin_Name)
  
  output$ROI <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = 'Topo') %>% 
      addProviderTiles(providers$CartoDB.Positron, group = 'Map') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Satelite') %>% 
      setView(lng = -121.933136, lat = 38.301334, zoom = 10) %>%
      addPolygons(data = county, group = 'Solano County', color = '#666666', fill = FALSE, weight = 3) %>% 
      addPolygons(data = groundwater_basins, label = ~Basin_Name, color = ~pal2(Basin_Name),  weight = 2,
                  group = 'Groundwater Basins') %>% 
      addPolygons(data = deliv_entities, color = ~pal(Name), weight = 2, 
                  label = ~Name, group = 'Delivery Entity') %>% 
      addPolygons(data = gsa, fillOpacity = .1, weight = 5, label = ~ GSA.Name, color = '#e7298a',
                  group = 'Groundwater Sustainability Agencies') %>% 
      addPolygons(data = RD2068_2017, group = 'Updated RD 2068 (2017)', weight = 2, 
                  color = '#1b9e77', label = 'Updated RD 2068 (2017)') %>% 
      addPolygons(data = RD999, group = 'RD 999', label =  'RD 999', weight = 2, 
                  color = '#d95f02') %>% 
      addPolygons(data = ryer, group = 'Ryer Island (RD 501)', label = 'Ryer Island (RD 501)',
                  weight = 2, color = '#7570b3') %>% 
      addLayersControl(baseGroups = c('Map', 'Satelite', 'Topo'), 
                       overlayGroups = c('Delivery Entity', 'Solano County', 'Groundwater Basins',  
                                         'Updated RD 2068 (2017)', 'Ryer Island (RD 501)',
                                         'RD 999','Groundwater Sustainability Agencies')) %>% 
      hideGroup(c('Groundwater Sustainability Agencies', 'Groundwater Basins', 'Updated RD 2068 (2017)'))
  })  
}

