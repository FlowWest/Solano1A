# Applied Water Demand
applied_demand <- read_csv("data/delivery/AppliedWaterDemand_updated.csv")

View(applied_demand)

applied_demand <- applied_demand %>% 
  gather(year, demand_acre_ft, `2010 (Acre-ft)`:`2015 (Acre-ft)`) %>% 
  separate(year, into = c("year", "dummy")) %>% 
  select(-dummy)

applied_demand %>% 
  ggplot(aes(Boundary, demand_acre_ft, fill=year)) + geom_col(position = "dodge")+ 
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_brewer(palette = "Set2")

applied_demand %>% 
  ggplot(aes(Boundary, demand_acre_ft, fill=year)) + geom_col(position = "fill")

write_rds(applied_demand, "data/delivery/applied_water_demand.rds")

# Solano Deliveries 
solano_deliveries <- read_csv("data/delivery/Solano_Delivery_updated.csv")

View(solano_deliveries)

solano_deliveries <- solano_deliveries %>% 
  gather(water_action, value, `2010 Groundwater`:`2015 Surface Water`) %>% 
  separate(water_action, into = c("year", "water_type")) %>% 
  mutate(water_type = ifelse(water_type == "Groundwater", "Groundwater", "Surface Water"))

p <- solano_deliveries %>% 
  ggplot(aes(`Water Resources Management Entity`, value, fill=year)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous(name = "", labels = scales::comma, 
                     breaks = c(0, 25000, 50000, 75000, 
                                100000)) + 
  scale_fill_brewer(palette = "Set2") + 
  scale_x_discrete(label=function(x) abbreviate(x, minlength = 10)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# reproduce the plot with plotly 
solano_deliveries %>% 
  plot_ly(x=~`Water Resources Management Entity`, y=~value, color=~year, 
          type='bar') %>% 
  layout(xaxis = list(title="", tickangle = -45, ticklen = 1, tickfont = 5))

write_rds(solano_deliveries, "data/delivery/solano_county_deliveries.rds")

# Interested Demand  
interested_demand <- read_csv("data/delivery/interested_demand_delivery_updated.csv")

locs <- interested_demand$water_resource_entity

applied_demand %>% 
  filter(Boundary %in% locs)

solano_deliveries %>% 
  filter(`Water Resources Management Entity` %in% locs, water_type == "Surface Water")

solano_deliveries %>% 
  filter(`Water Resources Management Entity` %in% locs, water_type == "Groundwater")



  
