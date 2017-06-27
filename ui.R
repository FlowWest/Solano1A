navbarPage(
  title = div(tags$img(src = 'thick_elder.png', width = 95), 'Solano County'),
  collapsible = TRUE,
  windowTitle = 'Solano County',
  theme = shinytheme(theme = 'paper'),
  header = includeCSS('styles.css'),
  tabPanel(title = 'Home',
           homeUI('one')),
  tabPanel(title = 'Deliveries',
           deliveryUI('one')),
  navbarMenu(title = 'Agriculture',
             tabPanel(title = 'Crops',
                      cropsUI('one')),
             tabPanel(title = 'Water Demand - By Crop Type',
                      model_awUI('one')),
             tabPanel(title = 'Water Demand - By Entity or Region',
                      demandUI('one'))),
  tabPanel(title = 'Water Balance',
           water_balanceUI('one')),
  navbarMenu(title = 'Groundwater',
             tabPanel(title = "Elevation Change", elevation_changeUI('one')), 
             tabPanel(title = "Well Data", casgem_dataUI('one'))
           )
)
