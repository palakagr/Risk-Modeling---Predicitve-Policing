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
library(mapview)
library(osmdata)
library(fastDummies)
# functions
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


## Read in Data from SF
sfCounty <-
  st_read("https://data.sfgov.org/api/geospatial/p5b7-5n3h?method=export&format=GeoJSON") %>% 
  st_union() %>%
  st_transform('ESRI:102241')

policeDistricts_sf <- 
  st_read("https://data.sfgov.org/api/geospatial/wkhw-cjsf?method=export&format=GeoJSON") %>% 
  st_transform('ESRI:102241')

assualt <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(category == "ASSAULT" & date > as.POSIXct("2016-01-01") & date < as.POSIXct("2017-01-01") ) %>%  
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102241') %>% 
  distinct()

# uses grid.arrange to organize independent plots
grid.arrange(ncol=2,
             ggplot() + 
               geom_sf(data = sfCounty) +
               geom_sf(data = assualt, colour="red", size=0.1, show.legend = "point") +
               labs(title= "Assualts, San Francisco - 2017") +
               mapTheme(title_size = 14),
             
             ggplot() + 
               geom_sf(data = sfCounty, fill = "grey40") +
               stat_density2d(data = data.frame(st_coordinates(assualt)), 
                              aes(X, Y, fill = ..level.., alpha = ..level..),
                              size = 0.01, bins = 40, geom = 'polygon') +
               scale_fill_viridis() +
               scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
               labs(title = "Density of Assualts") +
               mapTheme(title_size = 14) + theme(legend.position = "none"))

## Creating a fishnet grid
## using {sf} to create the grid
fishnet <- 
  st_make_grid(sfCounty,
               cellsize = 300, 
               square = TRUE) %>%
  .[sfCounty] %>% 
  st_sf() %>%
  mutate(uniqueID = rownames(.))

### Aggregate points to the fishnet
## add a value of 1 to each crime, sum them with aggregate
crime_net <- 
  dplyr::select(assualt) %>% 
  mutate(countAssualt = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countAssualt = replace_na(countAssualt, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = crime_net, aes(fill = countAssualt), color = NA) +
  scale_fill_viridis() +
  labs(title = "Count of Assualts for the fishnet") +
  mapTheme()

# Visualising crime data and fishnet
xx <- mapview::mapview(crime_net, zcol = "countAssualt")
yy <- mapview::mapview(mutate(assualt, ID = seq(1:n())))
xx + yy

## Modeling Spatial Features
j <- read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json?service_name=Encampments")
j <- j %>% filter(closed_date > as.POSIXct("2016-01-01") & closed_date < as.POSIXct("2017-01-01"))
j <- j %>% filter(service_subtype == "Encampment Reports")

Encampments <- 
  read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json?service_name=Encampments") %>%
  filter(service_subtype == "Encampment Reports" & closed_date > as.POSIXct("2016-01-01") & closed_date < as.POSIXct("2017-01-01")) %>%
  dplyr::select(Y = lat, X = long) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Encampments")

Streetlights <- 
  read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json?service_name=Streetlights") %>%
  filter(service_subtype == "Streetlight - Light_Burnt_Out" | service_subtype == "Streetlight - Light_Flickering_On_Off" |
         service_subtype == "Streetlight - Light_Dim" | closed_date > as.POSIXct("2016-01-01") & closed_date < as.POSIXct("2017-01-01")) %>%
  dplyr::select(Y = lat, X = long) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "StreetLights")

Graffiti <-
  read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json?service_name=Graffiti") %>%
  filter(closed_date > as.POSIXct("2016-01-01") & closed_date < as.POSIXct("2017-01-01")) %>%
  dplyr::select(Y = lat, X = long) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Graffiti")

NoiseReport <-
  read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json?service_name=Noise Report") %>%
  filter(closed_date > as.POSIXct("2016-01-01") & closed_date < as.POSIXct("2017-01-01")) %>%
  dplyr::select(Y = lat, X = long) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Noise")

AbandonedCar <-
  read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json?service_name=Abandoned Vehicle") %>%
  filter(closed_date > as.POSIXct("2016-01-01") & closed_date < as.POSIXct("2017-01-01")) %>%
  dplyr::select(Y = lat, X = long) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "AbandonedCar")

Homeless <-
  read.socrata("https://data.sfgov.org/resource/vw6y-z8j6.json?service_name=Homeless Concerns") %>%
  filter(closed_date > as.POSIXct("2016-01-01") & closed_date < as.POSIXct("2017-01-01")) %>%
  dplyr::select(Y = lat, X = long) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Homeless")

#Bar
sfbase <-
  st_read("https://data.sfgov.org/api/geospatial/p5b7-5n3h?method=export&format=GeoJSON") %>% 
  st_union()

xmin = st_bbox(sfbase)[[1]]
ymin = st_bbox(sfbase)[[2]]
xmax = st_bbox(sfbase)[[3]]  
ymax = st_bbox(sfbase)[[4]]

bars <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("bar", "pub", "restaurant")) %>%
  osmdata_sf()

bars <- 
  bars$osm_points %>%
  .[sfbase,]

bars <- bars %>% st_transform(st_crs(fishnet)) %>% mutate(Legend = 'Bar')

bars <- data.frame(bars$geometry, bars$Legend)

drug <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(category == "DRUG/NARCOTIC" & date > as.POSIXct("2016-01-01") & date < as.POSIXct("2017-01-01") ) %>%  
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102241') %>% 
  distinct()

ggplot() +
  geom_sf(data=sfbase, fill="black") +
  geom_sf(data=assualt, colour="red", size=.75)+
  geom_sf(data=drug, colour="blue", size=.75)

DomesticViolence <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(descript == "DOMESTIC VIOLENCE" & date > as.POSIXct("2016-01-01") & date < as.POSIXct("2017-01-01") ) %>%  
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102241') %>% 
  distinct()



liquorRetail <- 
  read.socrata("https://data.cityofchicago.org/resource/nrmj-3kcf.json") %>%  
  filter(business_activity == "Retail Sales of Packaged Liquor") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Liquor_Retail")

Liquor <-
  st_read("C:/Users/agarw/Documents/MUSA508/MUSA508-Assignment3/Data/Registered_Business_Locations_-_San_Francisco.csv")


Liquor <- Liquor %>% filter(str_detect(Liquor$DBA.Name, "Liquor"))

Liquor <- Liquor %>% mutate(geometry = Business.Location)

Liquor <- Liquor %>% mutate(Ge=paste(Business.Location, sep=" ")) %>%
  dummy_cols(select_columns="Ge", split=" ")

spli <- strsplit(Liquor$geometry, " ")
