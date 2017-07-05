


library(ncdf4)
library(raster)
library(rgeos)
library(maptools)

###bluefin <- nc_open("C:/Users/fede/Desktop/MSP Master Tesis/data/EMIS_BFT500_SH_08_2014.nc")
###print(bluefin)

bluefin <- raster("data/EMIS_BFT500_SH_08_2014.nc")
summary(bluefin)
plot(bluefin)

hist(bluefin)

# 1) create extent with a bounding box

westmed <- readOGR(dsn="C:\\Users\\fede\\Desktop\\MSP Master Tesis\\data",layer="WestMed_area")

# 2) use crop() to subset raster for our study area.

# wm_bluefin <- crop(bluefin, westmed)

## for some reason this clip only for the whole Mediterranean sea not just the West Med. 
## use gIntersection (below) to do it

###reclassify

v.rcl <- c(0, 10, 0, 10, 100, 1)
m.rcl <- matrix(v.rcl, ncol=3, byrow=TRUE)

bluefin.rec <- reclassify(bluefin, m.rcl)
bluefin.rec[bluefin.rec==0]<-NA
plot(bluefin.rec, axes=T, col='red'); plot(westmed, add=T)

## convert raster into polygon
poly.bluefin <- rasterToPolygons(bluefin.rec, na.rm=TRUE, digits=12, dissolve=TRUE)

# split multipolygon in separate polygons

split.poly <- disaggregate(poly.bluefin)

# 3) calculate area for each polygon

bluefin.laea <- spTransform(split.poly,CRS("+init=EPSG:3035"))

bluefin.laea$area <- area(bluefin.laea)

# 4) select polygon with area higher than our specified threshold

## threshold 100 km

bluefin_over100 <- subset(bluefin.laea, area > 100000000)

# dissolve the polygons in one multypoligon feature

bluefin.uni <- gUnaryUnion(bluefin_over100)

## make the clip again for WestMed, this way works fine

westmed.laea <- spTransform(westmed,CRS("+init=EPSG:3035"))

clip <- gIntersection(bluefin.uni, westmed.laea)

plot(clip, axes=T); plot(westmed.laea, add=T);

# writePolyShape(poly.bluefin,"C:\\Users\\fede\\Desktop\\MSP Master Tesis\\data2")
