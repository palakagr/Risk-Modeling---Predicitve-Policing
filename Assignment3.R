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

ggplot()+
  geom_sf(data = policeDistricts_sf)+
  labs(title="Police Districts") +
  mapTheme()
  

assualt <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(category == "ASSAULT" & date > as.POSIXct("2017-01-01") & date < as.POSIXct("2018-01-01") ) %>%  
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

ggplot()+
  geom_sf(data = fishnet)+
  labs(title="Fishnet for SF") +
  mapTheme()

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
  mutate(g = str_remove_all(Business.Location, "[POINT()]")) %>%
  separate(., col = g,into = c("g","X","Y"), sep = " ")%>% 
  dplyr::select(Y, X) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Liquor")

drug <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(category == "DRUG/NARCOTIC" & date > as.POSIXct("2017-01-01") & date < as.POSIXct("2018-01-01") ) %>%  
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102241') %>% 
  distinct()

drug_net <-
  dplyr::select(drug) %>% 
  mutate(countDrug = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countDrug = replace_na(countDrug, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE)) 

ggplot() +
  geom_sf(data=sfbase, fill="black") +
  geom_sf(data=assualt, colour="red", size=.75)+
  geom_sf(data=drug, colour="blue", size=.75)+
  labs(title="Assualt and Drug cases", subtitle = "Assualts in red and Drug in Blue", caption = "Figure 3.1")

DomesticViolence <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(descript == "DOMESTIC VIOLENCE" & date > as.POSIXct("2017-01-01") & date < as.POSIXct("2018-01-01") ) %>%  
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102241') %>% 
  distinct()

domestic_net <-
  dplyr::select(DomesticViolence) %>% 
  mutate(countDom = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countDom = replace_na(countDom, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data=sfbase, fill="black") +
  geom_sf(data=assualt, colour="red", size=.75)+
  geom_sf(data=DomesticViolence, colour="blue", size=.75)+
  labs(title="Assualt and Domestic violence cases", subtitle = "Assualts in red and Domestic Violence in Blue", caption = "Figure 3.2")

correlation_dom = cor(domestic_net$countDom, crime_net$countAssualt, use = "complete.obs")
correlation_dom
correlation_drug = cor(drug_net$countDrug, crime_net$countAssualt, use = "complete.obs")
correlation_drug

plot(drug_net$countDrug, crime_net$countAssualt, main= "Correlation between Drug and Assualt")
plot(domestic_net$countDom, crime_net$countAssualt, main= "Correlation between Domestic Violence and Assualt")

## Neighborhoods to use in LOOCV in a bit
neighborhoods <- 
  st_read("https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=GeoJSON") %>%
  st_transform(st_crs(fishnet)) %>%
  dplyr::select(-link)

#### Aggregate a feature to our fishnet

vars_net <- rbind(Encampments, Streetlights, Graffiti, NoiseReport, AbandonedCar, bars, parks, Liquor) %>%
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

Encampments_coord <- data.frame(st_c(Encampments)) %>% na.omit()
AbandonedCar_coord <- data.frame(st_c(AbandonedCar)) %>% na.omit()
Graffiti_coord <- data.frame(st_c(Graffiti)) %>% na.omit()
Streetlights_coord <- data.frame(st_c(Streetlights)) %>% na.omit()
NoiseReport_coord <- data.frame(st_c(NoiseReport)) %>% na.omit()
Bars_coord <- data.frame(st_c(bars)) %>% na.omit()
Parks_coord <- data.frame(st_c(parks)) %>% na.omit()
Liquor_coord <- data.frame(st_c(Liquor)) %>% na.omit()

## create NN from abandoned cars

## Nearest neighbor function
vars_net <-
  vars_net %>%
  mutate(
    Encampments.nn =
      nn_function(st_c(st_coid(vars_net)), Encampments_coord,2),
    Abandoned_Cars.nn =
      nn_function(st_c(st_coid(vars_net)), AbandonedCar_coord,5),
    Graffiti.nn =
      nn_function(st_c(st_coid(vars_net)), Graffiti_coord,5),
    Bars.nn =
      nn_function(st_c(st_coid(vars_net)), Bars_coord,3),
    Streetlights.nn = 
      nn_function(st_c(st_coid(vars_net)), Streetlights_coord,5),
    NoiseReport.nn =
      nn_function(st_c(st_coid(vars_net)), NoiseReport_coord,2),
    Parks.nn =
      nn_function(st_c(st_coid(vars_net)), Parks_coord,1),
    Liquor.nn =
      nn_function(st_c(st_coid(vars_net)), Liquor_coord,1))

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
  labs(title="Assualt NN Distance") +
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

## Regression
reg.vars <- c("Encampments.nn", "Abandoned_Cars.nn", "Graffiti.nn", 
              "Bars.nn", "Streetlights.nn", "NoiseReport.nn", "Parks.nn",
              "loopDistance")

reg.ss.vars <- c("Encampments.nn", "Abandoned_Cars.nn", "Graffiti.nn", 
                 "Bars.nn", "Streetlights.nn", "NoiseReport.nn", "Parks.nn",
                 "loopDistance", "Assualt.isSig", "Assualt.isSig.dist")

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

## Predicted assualts
reg.summary %>%
  filter(str_detect(Regression, "k-fold")) %>%
  ggplot() +
  geom_sf(aes(fill = Prediction)) +
  facet_wrap(~Regression) +
  scale_fill_viridis() +
  labs(title = "Prediction assualts by Regression") +
  mapTheme() + theme(legend.position="bottom")

##Oberserved Assualts
reg.summary %>%
  filter(str_detect(Regression, "k-fold")) %>%
  ggplot() +
  geom_sf(aes(fill = countAssualt)) +
  scale_fill_viridis() +
  labs(title = "Observed count of Assualts") +
  mapTheme()+ theme(legend.position="bottom")

error_by_reg_and_fold <- 
  reg.summary %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - countAssualt, na.rm = T),
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

st_drop_geometry(error_by_reg_and_fold) %>%
  group_by(Regression) %>% 
  summarize(Mean_MAE = round(mean(MAE), 2),
            SD_MAE = round(sd(MAE), 2)) %>%
  kable(caption = "MAE by regression") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(2, color = "black", background = "#FDE725FF") %>%
  row_spec(4, color = "black", background = "#FDE725FF") 

error_by_reg_and_fold %>%
  filter(str_detect(Regression, "LOGO")) %>%
  ggplot() +
  geom_sf(aes(fill = MAE)) +
  facet_wrap(~Regression) +
  scale_fill_viridis() +
  labs(title = "Assualt errors by LOGO-CV Regression") +
  mapTheme() + theme(legend.position="bottom")

neighborhood.weights <-
  filter(error_by_reg_and_fold, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  group_by(cvID) %>%
  poly2nb(as_Spatial(.), queen=TRUE) %>%
  nb2listw(., style="W", zero.policy=TRUE)

filter(error_by_reg_and_fold, str_detect(Regression, "LOGO"))  %>% 
  st_drop_geometry() %>%
  group_by(Regression) %>%
  summarize(Morans_I = moran.mc(abs(Mean_Error), neighborhood.weights, 
                                nsim = 999, zero.policy = TRUE, 
                                na.action=na.omit)[[1]],
            p_value = moran.mc(abs(Mean_Error), neighborhood.weights, 
                               nsim = 999, zero.policy = TRUE, 
                               na.action=na.omit)[[3]]) %>%
  kable(caption = "Moran's I on Errors by Regression") %>%
  kable_styling("striped", full_width = F)%>%
  row_spec(2, color = "black", background = "#FDE725FF") 

st_drop_geometry(reg.summary) %>%
  group_by(Regression) %>%
  mutate(Assualt_Decile = ntile(countAssualt, 10)) %>%
  group_by(Regression, Assualt_Decile) %>%
  summarize(meanObserved = mean(countAssualt, na.rm=T),
            meanPrediction = mean(Prediction, na.rm=T)) %>%
  gather(Variable, Value, -Regression, -Assualt_Decile) %>%          
  ggplot(aes(Assualt_Decile, Value, shape = Variable)) +
  geom_point(size = 2) + geom_path(aes(group = Assualt_Decile), colour = "black") +
  scale_shape_manual(values = c(2, 17)) +
  facet_wrap(~Regression) + xlim(0,10) +
  labs(title = "Predicted and observed assualt by observed assualt decile")

## Generalization by race
tracts18 <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E"), 
          year = 2018, state=06, county=075, geometry=T) %>%
  st_transform('ESRI:102241')  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01001_001,
         NumberWhites = B01001A_001) %>%
  mutate(percentWhite = NumberWhites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority_White", "Majority_Non_White")) %>%
  .[neighborhoods,]

ggplot()+
  geom_sf(data = tracts18, aes(fill = raceContext)) +
  scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Race Context") +
  labs(title = "Income Context") +
  mapTheme() + theme(legend.position="bottom")

reg.summary %>% 
  filter(str_detect(Regression, "LOGO")) %>%
  st_centroid() %>%
  st_join(tracts18) %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(Regression, raceContext) %>%
  summarize(mean.Error = mean(Error, na.rm = T)) %>%
  spread(raceContext, mean.Error) %>%
  kable(caption = "Mean Error by neighborhood racial context") %>%
  kable_styling("striped", full_width = F) 

## Density vs predictions
assualt_ppp <- as.ppp(st_coordinates(assualt), W = st_bbox(final_net))
assualt_KD.1000 <- spatstat::density.ppp(assualt_ppp, 1000)
assualt_KD.1500 <- spatstat::density.ppp(assualt_ppp, 1500)
assualt_KD.2000 <- spatstat::density.ppp(assualt_ppp, 2000)
assualt_KD.df <- rbind(
  mutate(data.frame(rasterToPoints(mask(raster(assualt_KD.1000), as(neighborhoods, 'Spatial')))), Legend = "1000 Ft."),
  mutate(data.frame(rasterToPoints(mask(raster(assualt_KD.1500), as(neighborhoods, 'Spatial')))), Legend = "1500 Ft."),
  mutate(data.frame(rasterToPoints(mask(raster(assualt_KD.2000), as(neighborhoods, 'Spatial')))), Legend = "2000 Ft.")) 

assualt_KD.df$Legend <- factor(assualt_KD.df$Legend, levels = c("1000 Ft.", "1500 Ft.", "2000 Ft."))

ggplot(data=assualt_KD.df, aes(x=x, y=y)) +
  geom_raster(aes(fill=layer)) + 
  facet_wrap(~Legend) +
  coord_sf(crs=st_crs(final_net)) + 
  scale_fill_viridis(name="Density") +
  labs(title = "Kernel density with 3 different search radii") +
  mapTheme(title_size = 14)

as.data.frame(assualt_KD.1000) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  ggplot() +
  geom_sf(aes(fill=value)) +
  geom_sf(data = sample_n(assualt, 1500), size = .5) +
  scale_fill_viridis(name = "Density") +
  labs(title = "Kernel density of 2017 Assualt") +
  mapTheme(title_size = 14)

assualt18 <- 
  read.socrata("https://data.sfgov.org/resource/tmnf-yvry.json") %>% 
  filter(category == "ASSAULT" & date > as.POSIXct("2018-01-01") & date < as.POSIXct("2019-01-01") ) %>%  
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102241') %>% 
  distinct()%>%
  .[fishnet,]

assualt_ppp <- as.ppp(st_coordinates(assualt), W = st_bbox(final_net))
assualt_KD <- spatstat::density.ppp(assualt_ppp, 1000)

assualt_KDE_sf <- as.data.frame(assualt_KD) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  mutate(label = "Kernel Density",
         Risk_Category = ntile(value, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(assualt18) %>% mutate(assualtCount = 1), ., sum) %>%
      mutate(assualtCount = replace_na(assualtCount, 0))) %>%
  dplyr::select(label, Risk_Category, assualtCount)

assualt_risk_sf <-
  filter(reg.summary, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  mutate(label = "Risk Predictions",
         Risk_Category = ntile(Prediction, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(assualt18) %>% mutate(assualtCount = 1), ., sum) %>%
      mutate(assualtCount = replace_na(assualtCount, 0))) %>%
  dplyr::select(label,Risk_Category, assualtCount)


rbind(assualt_KDE_sf, assualt_risk_sf) %>%
  na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
  ggplot() +
  geom_sf(aes(fill = Risk_Category), colour = NA) +
  geom_sf(data = sample_n(assualt18, 2000), size = .5, colour = "black") +
  facet_wrap(~label, ) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Comparison of Kernel Density and Risk Predictions",
       subtitle="2017 assualt risk predictions; 2018 assualts") +
  mapTheme()

rbind(assualt_KDE_sf, assualt_risk_sf) %>%
  st_set_geometry(NULL) %>% na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category) %>%
  group_by(label, Risk_Category) %>%
  summarize(countAssualt = sum(Value)) %>%
  ungroup() %>%
  group_by(label) %>%
  mutate(Rate_of_test_set_crimes = countAssualt / sum(countAssualt)) %>%
  ggplot(aes(Risk_Category,Rate_of_test_set_crimes)) +
  geom_bar(aes(fill=label), position="dodge", stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Risk prediction vs. Kernel density, 2018 assualts") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

