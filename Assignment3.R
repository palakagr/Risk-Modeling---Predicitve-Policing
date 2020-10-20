library(tidyverse)
library(sf)
library(viridis)
library(spatstat)
library(RSocrata)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
# functions
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


## Read in Data from SF

policeDistricts_sf <- 
  st_read("./Data/Current Police Districts.geojson") %>% 
  st_transform('ESRI:102241')

policeDistricts_sf <- 
  read.socrata("https://data.sfgov.org/resource/q52f-skbd.json") %>% 
  mutate(geometry = the_geom.coordinates) %>%
  st_as_sf() %>%
  st_transform('ESRI:102241')

assualt <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(category == "ASSAULT" & date > as.POSIXct("2016-01-01") & date < as.POSIXct("2017-01-01") ) %>%  
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102241') %>% 
  distinct()

sfCounty <-
  read.socrata("https://data.sfgov.org/resource/wamw-vt4s.json") %>%
  filter(county == "San Francisco") %>%
  mutate(geometry = the_geom.coordinates) %>%
  st_sf() %>%
  st_transform('ESRI:102241')

sfCounty <-
  st_read("./Data/Bay Area Counties.geojson") %>% 
  st_transform('ESRI:102241')
  

stat_density2d



