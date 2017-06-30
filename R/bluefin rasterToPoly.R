


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
# 2) use crop() to subset raster for our study area.


###reclassify

v.rcl <- c(0, 0.1, 0, 0.1, 100, 1)
m.rcl <- matrix(v.rcl, ncol=3, byrow=TRUE)

bluefin.rec <- reclassify(bluefin, m.rcl)
bluefin.rec[bluefin.rec==0]<-NA
plot(bluefin.rec)

## TRY TO CHANGE THE DISSOLVE OPTION FROM FALSE TO TRUE (still takes 1 hour time)
poly.bluefin <- rasterToPolygons(bluefin.rec, na.rm=TRUE, digits=12, dissolve=TRUE)

# 3) calculate area for each polygon
# 4) select polygon with area higher than our specified threshold


writePolyShape(poly.bluefin,"C:\\Users\\fede\\Desktop\\MSP Master Tesis\\data2")

