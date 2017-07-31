setwd("C:")

library(shapefiles)
library(maptools)
library(rgdal)
library(stringdist)
library(leaflet)
library(dplyr)
library(rgeos) #### intersect() function, careful because it has conflicts with dplyr

## Import dynamic zones previously created

crswgs4326=CRS("+init=EPSG:4326")
dynamic_zones <- readShapePoly("C:\\Governance_test\\result_poly\\bf_05_2003.shp", proj4string=crswgs4326,verbose=TRUE) 
zones.laea <- spTransform(dynamic_zones,CRS("+init=EPSG:3035"))

# split multipolygon in separate polygons

split.poly <- disaggregate(zones.laea)

split.poly$area.1 <- area(split.poly)
n <- nrow(split.poly@data)
split.poly@data$SP_ID <- c(0:(n-1))
# Import the jurisdictional waters limits

JW <- readShapePoly("C:\\Governance_test\\original_data\\eez_clip.shp",proj4string=crswgs4326,verbose=TRUE)
jw.laea <- spTransform(JW,CRS("+init=EPSG:3035"))

## clip 
# westmed <- readOGR(dsn="C:\\Governance_test\\original_data",layer="WestMed_area")
# westmed.laea <- spTransform(westmed,CRS("+init=EPSG:3035"))

# clip <- gIntersection(jw.laea, westmed.laea)

### plot(zones.laea, axes=T, col='red'); plot(jw.laea, add=T)


## intersect dynamic polys with jurisdictional water layer

Results <- gIntersects(split.poly,jw.laea,byid=TRUE)

Results <- as.data.frame(Results)
colnames(Results)<-"Overlap"

### join the dataframe of the jw with the results of the intersection

Results_info <- cbind(jw.laea@data, Results) 

## select only jw areas which overlap with the dynamic poly (Overlap = TRUE)

selected <- Results_info %>% 
  filter(Overlap == TRUE) 


View(selected)


#### 
#################
###### calculate intersection ######################################################
library(PBSmapping)
library(raster)

detach("package:dplyr") ## dplyr in conflict with raster for intersect function

############## repair the zones.laea that sometimes is invalid for some reason (https://gis.stackexchange.com/questions/163445/r-solution-for-topologyexception-input-geom-1-is-invalid-self-intersection-er)
zones.b <- gBuffer(split.poly, byid=TRUE, width=0)
## it works!!





#####°°°°°°°°°°°°°°°°
## I should find a way to repeat the following caculations for each single polygon
#####°°°°°°°°°°°°°°°°






# intersect from raster package (it does not work if you have dplyr library)
pi <- intersect(jw.laea,zones.b)
plot(zones.laea, axes=T); plot(jw.laea, add=T); plot(pi, add=T, col='red')


## calculate area of intersection and add as a column

## http://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
areas <- data.frame(area=sapply(pi@polygons, FUN=function(x) {slot(x, 'area')}))

row.names(areas) <- sapply(pi@polygons, FUN=function(x) {slot(x, 'ID')})

# Combine attributes info and areas (area is in m2)

attArea <- spCbind(pi, areas)

### create a column with the proportion of area intersection for country
library(dplyr)

intersection <- attArea@data %>% mutate(intersect = area.1 / sum(area.1))


###################################
#### obtain the ponderate value of the indicator
#####################################

############ add indicator data 

index <- read.csv("C:\\Users\\fede\\Desktop\\MSP Master Tesis\\Indicators\\components_2016.csv", sep=",",header = TRUE)
area.ind <- intersection %>% inner_join(index, by = c("Sovereign1"="country")) %>%
  filter(ohi_component_id == c("fp_mora", "wgi_all"))

##### consider to use unique (there are repetions intrinsic of the indicator from OHI original data)
##### distinct works fine

area.ind.unique <- area.ind %>% 
  group_by(Sovereign1) %>% #### if a jw layer with also TS and IW is considered it can be reasonable to group also by jurisdiction
  distinct(ohi_component_id, .keep_all = TRUE) %>% 
  ungroup(Sovereign1) %>% 
  mutate(weighted = value * intersect)


###########################################################
###################### create the score for each indicator
##########################################################


##### fishery management

fm <- area.ind.unique %>% filter(ohi_component_id == "fp_mora")

score_fm <- sum(fm$weighted)



##### wgi

wgi <- area.ind.unique %>% filter(ohi_component_id == "wgi_all")

score_wgi <- sum(wgi$weighted)
