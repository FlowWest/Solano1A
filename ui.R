navbarPage(
  title = 'Solano County',
  theme = shinytheme(theme = 'paper'),
  header = includeCSS('styles.css'),
  tabPanel(title = 'Home',
           homeUI('one')),
  tabPanel(title = 'Deliveries',
           deliveryUI('one')),
  navbarMenu(title = 'Agriculture',
             tabPanel(title = 'Modeled Applied Water Demand',
                      model_awUI('one')),
             tabPanel(title = 'Crops',
                      cropsUI('one'))),
  navbarMenu(title = 'Ground Water',
             tabPanel(title = 'Elevation Change',
                      elevation_changeUI('one')),
             tabPanel(title = 'Demand',
                      demandUI('one'))
                      ),
  tabPanel(title = 'Water Balance',
           water_balanceUI('one'))
)
