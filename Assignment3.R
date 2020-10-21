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

ggplot() +
  geom_sf(data=sfbase, fill="black") +
  geom_sf(data=assualt, colour="red", size=.75)+
  geom_sf(data=DomesticViolence, colour="blue", size=.75)

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

domestic_net <-
  dplyr::select(DomesticViolence) %>% 
  mutate(countDom = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countDom = replace_na(countDom, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

drug_net <-
  dplyr::select(DomesticViolence) %>% 
  mutate(countDrug = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countDom = replace_na(countDrug, 0),
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

bars <- bars %>% st_transform(st_crs(fishnet)) %>% 
  mutate(Legend = 'Bar') %>% 
  dplyr::select(geometry, Legend)



parks <-
  st_read("C:/Users/agarw/Documents/MUSA508/MUSA508-Assignment3/Data/Recreation_and_Parks_Properties.csv") %>%
  dplyr::select(Y = Latitude, X = Longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Park")

Liquor <-
  st_read("C:/Users/agarw/Documents/MUSA508/MUSA508-Assignment3/Data/Registered_Business_Locations_-_San_Francisco.csv") %>% 
  filter(str_detect(Liquor$DBA.Name, "Liquor")) %>%
  mutate(geometry = Business.Location) %>%
  spli <- strsplit(Liquor$geometry, " ")

## Neighborhoods to use in LOOCV in a bit
neighborhoods <- 
  st_read("https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=GeoJSON") %>%
  st_transform(st_crs(fishnet)) %>%
  dplyr::select(-link)

#### Aggregate a feature to our fishnet

vars_net <- rbind(Encampments, Streetlights, Graffiti, NoiseReport, AbandonedCar, bars, parks) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet, by = "uniqueID") %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

## Plot the risks
vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol =3, top = "Risk Factors by Fishnet"))

## Nearest neighbor
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

# convinience to reduce length of function names.
st_c<- st_coordinates
st_coid <- st_centroid

## create NN from abandoned cars
vars_net <- vars_net %>%
  mutate(n = nn_function(st_c(st_coid(vars_net)), st_c(Encampments),3))

## try one with 3
vars_net <-
  vars_net %>%
  mutate(
    Encampments.nn3 =
      nn_function(st_c(st_coid(vars_net)), st_c(Encampments),3),
    Abandoned_Cars.nn3 =
      nn_function(st_c(st_coid(vars_net)), st_c(AbandonCars),3),
    Graffiti.nn3 =
      nn_function(st_c(st_coid(vars_net)), st_c(Graffiti),3),
    Bars.nn3 =
      nn_function(st_c(st_coid(vars_net)), st_c(bars),3),
    Streetlights.nn3 = 
      nn_function(st_c(st_coid(vars_net)), st_c(Streetlights),3),
    NoiseReport.nn3 =
      nn_function(st_c(st_coid(vars_net)), st_c(NoiseReport),3),
    Parks.nn3 =
      nn_function(st_c(st_coid(vars_net)), st_c(parks),3))

## Plot nearest neighbor
vars_net.long.nn <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

vars <- unique(vars_net.long.nn$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long.nn, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol = 3, top = "Nearest Neighbor risk Factors by Fishnet"))

## Hotspot
Financial_District <-
  filter(neighborhoods, name == "Financial District") %>%
  st_centroid()

vars_net$loopDistance =
  st_distance(st_centroid(vars_net),Financial_District) %>%
  as.numeric() 

ggplot() + 
  geom_sf(data=vars_net, aes(fill=loopDistance), colour=NA) + 
  scale_fill_viridis()

## Final net
final_net <-
  left_join(crime_net, st_drop_geometry(vars_net), by="uniqueID") 

final_net <-
  st_centroid(final_net) %>%
  st_join(dplyr::select(neighborhoods, name)) %>%
  st_join(dplyr::select(policeDistricts_sf, district)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit()

mapview::mapview(final_net, zcol = "district")
mapview::mapview(final_net, zcol = "name")

ggplot() + 
  geom_sf(data=final_net, aes(fill=factor(district)), colour=NA) 

ggplot() + 
  geom_sf(data=final_net, aes(fill=factor(name)), colour=NA) 

## Local Moran I's
final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

local_morans <- localmoran(final_net$Abandoned_Cars, final_net.weights) %>% 
  as.data.frame()

final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(final_net$countAssualt, final_net.weights)),
    as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(Assualt_Count = countAssualt, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z > 0)`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.05, 1, 0)) %>%
  gather(Variable, Value, -geometry)

vars <- unique(final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(final_net.localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme() + theme(legend.position="bottom")}

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Assualt"))

final_net <-
  final_net %>% 
  mutate(Assualt.isSig = 
           ifelse(localmoran(final_net$countAssualt, 
                             final_net.weights)[,5] <= 0.0000001, 1, 0)) %>%
  mutate(Assualt.isSig.dist = 
           nn_function(st_coordinates(st_centroid(final_net)),
                       st_coordinates(st_centroid(
                         filter(final_net, Assualt.isSig == 1))), 1))

ggplot() +
  geom_sf(data = final_net, aes(fill=Assualt.isSig.dist), colour=NA) +
  scale_fill_viridis(name="NN Distance") +
  labs(title="Abandoned Car NN Distance") +
  mapTheme()

## Correlation

correlation.long <-
  st_drop_geometry(final_net) %>%
  dplyr::select(-uniqueID, -cvID, -loopDistance, -name, -district) %>%
  gather(Variable, Value, -countAssualt)

correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, countAssualt, use = "complete.obs"))

ggplot(correlation.long, aes(Value, countAssualt)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Assualt count as a function of risk factors") +
  plotTheme()


correlation_dom = cor(domestic_net$countDom, crime_net$countAssualt, use = "complete.obs")
correlation_dom
correlation_drug = cor(drug_net$countDom, crime_net$countAssualt, use = "complete.obs")
correlation_drug


## Regression
reg.vars <- c("Abandoned_Buildings.nn", "Abandoned_Cars.nn", "Graffiti.nn", 
              "Liquor_Retail.nn", "Street_Lights_Out.nn", "Sanitation.nn", 
              "loopDistance")

reg.ss.vars <- c("Abandoned_Buildings.nn", "Abandoned_Cars.nn", "Graffiti.nn", 
                 "Liquor_Retail.nn", "Street_Lights_Out.nn", "Sanitation.nn", 
                 "loopDistance", "burglary.isSig", "burglary.isSig.dist")

crossValidate <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset[[id]])
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, dataset[[id]] != thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, dataset[[id]] == thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countAssualt ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -id))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}

reg.cv <- crossValidate(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countAssualt",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = cvID, countAssualt, Prediction, geometry)

reg.ss.cv <- crossValidate(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countAssualt",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = cvID, countAssualt, Prediction, geometry)

reg.spatialCV <- crossValidate(
  dataset = final_net,
  id = "name",
  dependentVariable = "countAssualt",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = name, countAssualt, Prediction, geometry)

reg.ss.spatialCV <- crossValidate(
  dataset = final_net,
  id = "name",
  dependentVariable = "countAssualt",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = name, countAssualt, Prediction, geometry)

reg.summary <- 
  rbind(
    mutate(reg.cv,           Error = Prediction - countAssualt,
           Regression = "Random k-fold CV: Just Risk Factors"),
    
    mutate(reg.ss.cv,        Error = Prediction - countAssualt,
           Regression = "Random k-fold CV: Spatial Process"),
    
    mutate(reg.spatialCV,    Error = Prediction - countAssualt,
           Regression = "Spatial LOGO-CV: Just Risk Factors"),
    
    mutate(reg.ss.spatialCV, Error = Prediction - countAssualt,
           Regression = "Spatial LOGO-CV: Spatial Process")) %>%
  st_sf() 

error_by_reg_and_fold <- 
  reg.summary %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - countBurglaries, na.rm = T),
            MAE = mean(abs(Mean_Error), na.rm = T),
            SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
  ungroup()

error_by_reg_and_fold %>%
  ggplot(aes(MAE)) + 
  geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
  facet_wrap(~Regression) +  
  geom_vline(xintercept = 0) + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Distribution of MAE", subtitle = "k-fold cross validation vs. LOGO-CV",
       x="Mean Absolute Error", y="Count") 