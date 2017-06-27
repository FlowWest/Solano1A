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

# plotly version 
applied_demand %>% 
  plot_ly(x=~Boundary, y=~demand_acre_ft, color = ~year, type='bar', colors = "Set2") %>% 
  layout(title = "Applied Water Demand", 
         xaxis = list(title = "", tickangle = -45), 
         margin = list(pad = 0, b = 90))

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
  mutate(entity_labels = abbreviate(`Water Resources Management Entity`, minlength = 15)) %>% 
  plot_ly(x=~entity_labels, y=~value, color=~year, 
          type='bar', colors = "Set2") %>% 
  layout(xaxis = list(title="", tickangle = -45, ticklen = 1, tickfont = 5), 
         margin = list(pad = 0, b = 90))

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


# The names in the rds for delivs do not match those in the shape file, this 
# fixes this 


# named vector will map the names from rds to those in the shape file 
name_conv_lookup <- c("SID" = "Solano Irrigation District", 
                      "RD 2068" = "Reclamation District 2068", 
                      "City of Fairfield" = "Fairfield", 
                      "City of Vallejo" = "Vallejo", 
                      "MPWD" = "Maine Prairie Water District", 
                      "City of Vacaville" = "Vacaville", 
                      "City of Benicia" = "Benicia", 
                      "City of Davis" = "Davis", 
                      "City of Dixon (includes Cal Water Service District)" = "Dixon", 
                      "Suisun City" = "Suisun City", 
                      "City of Rio Vista" = "Rio Vista", 
                      "UC Davis" = "University of California - Davis", 
                      "Cal State Prison- Solano" = "Cal State Prison- Solano", 
                      "Rural North Vacaville Water District" = "Rural North Vacaville Water District", 
                      "Travis Airforce Base" = "Travis Airforce Base")


shape_attribute <- name_conv_lookup[solano_deliveries$`Water Resources Management Entity`]

solano_deliveries$shape_ref_attr <- shape_attribute

# write out this new dataset 
write_rds(solano_deliveries, "data/delivery/solano_county_deliveries.rds")

# linking the demand with the delivery 

# the names in the deliveries, and demand
solano_deliveries$`Water Resources Management Entity` %>% unique()
applied_demand$Boundary %>% unique()

# summary statistics 
# total water delivered 
solano_deliveries %>% 
  filter(`Water Resources Management Entity` == 'SID') %>% 
  group_by(year) %>% 
  summarise(
    year_total = sum(value)
  ) %>% ungroup() 

entity_summary <- function(.d) {
  year_totals <- .d %>% 
    group_by(`Water Resources Management Entity`, year) %>% 
    summarise(
      year_total = sum(value)
    ) %>% ungroup() %>% dplyr::select(`Water Resources Management Entity`,year, year_total)
    
  percent_of_total <- left_join(.d, year_totals) %>% 
    mutate(percent_of_total = value / year_total)
  
  return(percent_of_total)
}



View(water_balance)

colnames(water_balance) <- c("Entity", "2010", "2015", "Percent Change")

water_balance <- water_balance %>% 
  gather(year, volume, `2010`:`2015`) %>% 
  mutate("Percent Change" = parse_number(`Percent Change`))

water_balance %>%
  filter(year == "2010") %>% 
  plot_ly(x=~Entity, y=~volume, type='bar', color=~year)

water_balance %>%
  filter(year == "2015") %>% 
  plot_ly(x=~Entity, y=~volume, type='bar', color=~year)

View(balance_data)

urban_water <- stringr::str_detect(balance_data$Entity, "Urban Water")  
ag_water <- stringr::str_detect(balance_data$Entity, "Agricultural Water")  
all_water <- !urban_water & !ag_water 
  
balance_data$entity_type[urban_water] <- "Urban Water"
balance_data$entity_type[ag_water] <- "Agricultural Water"
balance_data$entity_type[all_water] <- "Countywide Water"

  
balance_data$display_label <- balance_data$Entity %>% 
  map_chr(function(x) {paste(strwrap(x, width = 16), collapse = "<br>")})
  







  
