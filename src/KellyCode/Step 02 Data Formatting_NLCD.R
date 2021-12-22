#A script to re-format the NLCD data

# Last edited KR McCaffrey Jan 2021

#NLCD category definitions can be found here:
#https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(progress)

#set the base working directory
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/")

#set the county
co<-"Stanislaus"

#load in CDL raster as a template
temp<-raster(paste0("./", co, "/CDL_Acc/", co, "_2013_1_stack.tif"))

#Import the county shapefile
wd<-paste0(getwd(), "/Spatial Data/CA_Counties/")
county<-shapefile(paste0(wd, "CA_Counties_Tiger2016.shp"))
county<-subset(county, county$NAME==co)

#Import the CA border
wd<-paste0(getwd(), "/Spatial Data/CA_Border/")
CA<-shapefile(paste0(wd, "CA_border.shp"))

#project both
x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
county<-spTransform(county, x)
CA<-spTransform(CA, x)

#Load the NLCD
NLCD<-raster("E:/EPA Work/Prob Crop Proj Jan 2021/NLCD_Land_Cover_L48_20190424_full_zip/NLCD_2013_Land_Cover_L48_20190424.img")

#make smaller before reprojecting
e<-extent(-2356114, -1646660, 1242357, 2452711 )
NLCD<-crop(NLCD, e)

#fix the raster values
NLCD<-setValues(raster(NLCD), NLCD[])

#project the raster to our system
NLCD<-projectRaster(NLCD, crs=x, method="ngb")

#mask and save
NLCD<-mask(NLCD, CA)
plot(NLCD)

# writeRaster(NLCD, "NLCDCA2011.tif", format="GTiff", overwrite=T)


#### If NLCD has already been clipped to CA, start from here ####
NLCD<-raster("NLCDCA2011.tif")
county<-spTransform(county, crs(NLCD))
NLCD<-crop(NLCD, county)
NLCD<-mask(NLCD, county)

#resample to a blank raster to match the origins, resolution, etc
r<-raster(ext=extent(temp), crs=crs(temp),
          resolution=c(30,30))
NLCD<-resample(NLCD, r, method="ngb")
NLCD; plot(NLCD)

#pixels of class 82 are the "cultivated crop" category
NLCD82<-NLCD
values(NLCD82)[values(NLCD82)!=82]<-0
values(NLCD82)[values(NLCD82)==82]<-1

#pixels of class 81 are the "Pasture/Hay" category
NLCD81<-NLCD
values(NLCD81)[values(NLCD81)!=81]<-0
values(NLCD81)[values(NLCD81)==81]<-1

#save the NLCD rasters
writeRaster(NLCD81, paste0("./",co,"/NLCD_81_", co, ".tif"), format="GTiff", overwrite=T)
writeRaster(NLCD82, paste0("./",co,"/NLCD_82_", co, ".tif"), format="GTiff", overwrite=T)

#### Reclassify the rasters with the accuracy matrix values for Cultivated Crops and Pasture/Hay 
crop<-matrix(c(0, 11, 0.0008, 
               11, 12, 0, 
               12, 21, 0.0205, 
               21, 22, 0.0017, 
               22, 23, 0, 
               23, 24, 0, 
               24, 31, 0.0005, 
               31, 41, 0.0118, 
               41, 42, 0.0009, 
               42, 43, 0, 
               43, 52, 0.0055,
               52, 71, 0.0390, 
               71, 81, 0.0865, 
               81, 82, 0.8300, 
               82, 90, 0.0021,
               90, 95, 0.0008), ncol=3, byrow=T)
notcrop<-matrix(c(0, 11, 0.0149, 
                  11, 12, 0, 
                  12, 21, 0.1301, 
                  21, 22, 0.0149, 
                  22, 23, 0.0074,
                  23, 24, 0.0076, 
                  24, 31, 0.0002, 
                  31, 41, 0.1060, 
                  41, 42, 0.0149, 
                  42, 43, 0.0074, 
                  43, 52, 0.0480, 
                  52, 71, 0.0929, 
                  71, 81, 0.5037,
                  81, 82, 0, 
                  82, 90, 0.0177, 
                  90, 95, 0.0344), ncol=3, byrow=T)
past<-matrix(c(0, 11, 0.0011, 
               11, 12, 0, 
               12, 21, 0.0411, 
               21, 22, 0.0033, 
               22, 23, 0, 
               23, 24, 0.0003,
               24, 31, 0.0006, 
               31, 41, 0.0058, 
               41, 42, 0.0032, 
               42, 43, 0.0014, 
               43, 52, 0.0367,
               52, 71, 0.2121, 
               71, 81, 0.4848, 
               81, 82, 0.2013, 
               82, 90, 0.0016, 
               90, 95, 0.0077),
             ncol=3, byrow=T)
notpast<-matrix(c(0, 11, 0.0056, 
                  11, 12, 0, 
                  12, 21, 0.1354, 
                  21, 22, 0.0257, 
                  22, 23, 0, 
                  23, 24, 0,
                  24, 31, 0.0056, 
                  31, 41, 0.1549, 
                  41, 42, 0.0270, 
                  42, 43, 0.0056, 
                  43, 52, 0.0526,
                  52, 71, 0.1010, 
                  71, 81, 0, 
                  81, 82, 0.4459, 
                  82, 90, 0.0111, 
                  90, 95, 0.0297),
                ncol=3, byrow=T)

#create conditional probability rasters
NLCD_crop<-reclassify(NLCD, crop, right=T)
NLCD_notcrop<-reclassify(NLCD, notcrop, right=T)
NLCD_past<-reclassify(NLCD, past, right=T)
NLCD_notpast<-reclassify(NLCD, notpast, right=T)
NLCD_crop_acc<-stack(NLCD_crop, NLCD_notcrop)
NLCD_past_acc<-stack(NLCD_past, NLCD_notpast)

#save the rasters
writeRaster(NLCD_crop_acc, paste0("./",co,"/NLCD_crop_acc_stack_", co,".tif"), format="GTiff", overwrite=T)
writeRaster(NLCD_past_acc, paste0("./",co,"/NLCD_past_acc_stack_", co, ".tif"), format="GTiff", overwrite=T)

###Find the area of 81 and 82 by county
co<-"Madera"

NLCD81<-raster(paste0("./", co, "/NLCD_81_", co, ".tif"))
NLCD82<-raster(paste0("./", co, "/NLCD_82_", co, ".tif"))

area81<-NLCD81*900
area81<-cellStats(area81, sum, na.rm=T)
area81<-area81*0.000247105

area82<-NLCD82*900
area82<-cellStats(area82, sum, na.rm=T)
area82<-area82*0.000247105

total<-area81+area82
print(total)

#### End ####