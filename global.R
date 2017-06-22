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

source('module/home.R')
source('module/delivery.R')
source('module/agriculture.R')
source('module/ground_water.R')
source('module/water_balance.R')

ROIs <- rgdal::readOGR('data/solano_ROI.shp', stringsAsFactors = FALSE) %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}
