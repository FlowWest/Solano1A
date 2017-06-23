shinyServer(function(input, output) {
  callModule(home, 'one')
  callModule(delivery, 'one')
  callModule(demand, 'one')
  callModule(elevation_change, 'one')
  callModule(model_aw, 'one')
})
