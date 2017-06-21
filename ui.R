navbarPage(
  title = 'Solano 1A',
  theme = shinytheme(theme = 'paper'),
  header = includeCSS('styles.css'),
  tabPanel(title = 'About Solano 1A',
           homeUI('one')),
  tabPanel(title = 'Deliveries',
           deliveryUI('one')),
  tabPanel(title = 'Agriculture',
           agricultureUI('one')),
  tabPanel(title = 'Ground Water',
           ground_waterUI('one')),
  tabPanel(title = 'Water Balance',
           water_balanceUI('one'))
)
