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

ROIs <- rgdal::readOGR('data/ROI/solano_ROI.shp', stringsAsFactors = FALSE) %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

# modeled evapotransport applied water 
CUP_2010 <- raster::raster('data/cup2010/2010_CUP.tif')
CUP_2015 <- raster::raster('data/cup2015/2015_CUP.tif')

pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}



