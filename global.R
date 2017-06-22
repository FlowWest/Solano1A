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

# data imports 

applied_demand <- read_rds("data/delivery/applied_water_demand.rds")
solano_deliveries <- read_rds("data/delivery/solano_county_deliveries.rds")