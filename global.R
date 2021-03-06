library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(plotly)
library(leaflet)
library(rgdal)
library(raster)
library(sp)
library(readxl)
library(forcats)
library(lubridate)

source('module/home.R')
source('module/delivery.R')
source('module/agriculture.R')
source('module/ground_water.R')
source('module/water_balance.R')


applied_demand <- read_rds("data/delivery/applied_water_demand2.rds")
applied_demand$display_name <- factor(applied_demand$display_name)
solano_deliveries <- read_rds("data/delivery/solano_county_deliveries_2.rds")
percent_delivered <- read_csv("raw-data/percent_deliveries_updated.csv")
casgem_metadata <- read_rds('data/casgem/gst_in_solano_2017.rds')
casgem <- read_rds('data/casgem/gwl_in_solano_2017.rds')
balance_data <- read_rds("data/water_balance/water_balance_fixed.rds")
balance_data$display_label <- factor(balance_data$display_label)

sub_basin <- rgdal::readOGR('data/solano_subbasin/solano_subasin2016.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

sub_basin_county <- rgdal::readOGR('data/subbasin_county/subbasin_cty.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

gsa <- rgdal::readOGR('data/CA_GSAs/GSA_Master.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

RD2068_2017 <- rgdal::readOGR('data/RD2068/RD2068.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

RD999 <- rgdal::readOGR('data/rd_999/rd999.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

ryer <- rgdal::readOGR('data/ryer_island/ryerisland501.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

county <- rgdal::readOGR('data/county_boundaries/countyboundarypoly.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

ROIs <- rgdal::readOGR('data/ROI/solano_ROI.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

deliv_entities <- rgdal::readOGR('data/mgmt_entities_delivery2/entity_deliveries2.shp',
                                 stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

groundwater_basins <- rgdal::readOGR('data/B118_CA_GroundwaterBasins_Revised2016/i08_B118_CA_GroundwaterBasins.shp', stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
  subset(Basin_Subb %in% c('2-002.03', '2-003', '5-021.66'))

groundwater_basins@data$Subbasin_N <- c('Napa-Sonoma Lowlands Subbasin', 'Suisun-Fairfield Vally Basin', 'Solano Subbasin')

# crop data
crop_acres <- read_excel('raw-data/crops_2010_2015.xlsx', sheet = "2015 v 2010 Acres")
crop_demand <- read_excel('raw-data/crops_2010_2015.xlsx', sheet = "2015 v 2010 Demand")

awd_deliv_ent <- read_csv('raw-data/AppliedWaterDemand_updated.csv')
sub_deliv_ent <- subset(deliv_entities, Acronym %in% c('MPWD', 'SID', 'RD 2068'))

temp <- awd_deliv_ent %>% 
  dplyr::rename(Acronym = Boundary) %>% 
  dplyr::filter(Acronym %in% c('MPWD', 'SID', 'RD 2068'))

sub_deliv_ent@data <- sub_deliv_ent@data %>% 
  dplyr::left_join(temp)

sub_basin_county@data <- bind_cols(sub_basin_county@data, awd_deliv_ent[2,])



# modeled applied water demand
CUP_2010 <- raster::raster('data/cup2010/2010_CUP.tif') 
CUP_2015 <- raster::raster('data/cup2015/2015_CUP.tif')

# groundwater
# fall_chg <- raster::raster('data/GW_change/fall_chng_surface/Co_f15to10_KS.tif') %>% 
#   projectRasterForLeaflet()
# writeRaster(fall_chg, 'data/GW_change/fall_chng_surface/fall_change_surface.grd')

fall_chg <- raster::raster('data/GW_change/fall_chng_surface/fall_change_surface.grd')
fall_chg_wells <- rgdal::readOGR('data/GW_change/fall_chng_at_wells/f2015_to_f2010.shp', stringsAsFactors = FALSE) %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

# spring_chg <- raster::raster('data/GW_change/spring_chng_surface/Co_s15to10_KS.tif') %>% 
#   projectRasterForLeaflet()
# writeRaster(spring_chg, 'data/GW_change/spring_chng_surface/spring_change_surface.grd')

spring_chg <- raster::raster('data/GW_change/spring_chng_surface/spring_change_surface.grd')
spring_chg_wells <- rgdal::readOGR('data/GW_change/spring_chng_at_wells/s2015_to_s2010.shp', stringsAsFactors = FALSE) %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))


pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}



