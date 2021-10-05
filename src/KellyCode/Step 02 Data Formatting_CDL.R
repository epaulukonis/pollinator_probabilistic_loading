#A script to re-format the CDL data and assign CDL accuracy information

# Last edited KR McCaffrey Jan 2021

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(progress)

#set the base working directory
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/")

#tests

#set the county
co<-"Stanislaus"

#Import the county shapefile
wd<-paste0(getwd(), "/Spatial Data/CA_Counties/")
county<-shapefile(paste0(wd, "CA_Counties_Tiger2016.shp"))
county<-subset(county, county$NAME==co)

#Import the CDL data for CA
CDL_2013<-raster("./Spatial Data/CDL_CA/CDL_2013_06.tif")
CDL_2014<-raster("./Spatial Data/CDL_CA/CDL_2014_06.tif")
CDL_2015<-raster("./Spatial Data/CDL_CA/CDL_2015_06.tif")
CDL_2016<-raster("./Spatial Data/CDL_CA/CDL_2016_06.tif")
CDL_2017<-raster("./Spatial Data/CDL_CA/CDL_2017_06.tif")

#make the CRS match
x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
CDL_2013<-projectRaster(CDL_2013, crs=x)
CDL_2014<-projectRaster(CDL_2014, crs=x)
CDL_2015<-projectRaster(CDL_2015, crs=x)
CDL_2016<-projectRaster(CDL_2016, crs=x)
CDL_2017<-projectRaster(CDL_2017, crs=x)

county<-spTransform(county, x)

#import tables of CDL Accuracy information
#Accuracy information is available by state, for each year of the CDL
#https://www.nass.usda.gov/Research_and_Science/Cropland/metadata/meta.php
Acc_df<-read.csv("CDL_Acc.csv") #CDL User's Accuracy
Err_df<-read.csv("CDL_Err.csv") #CDL Commission Error

#mask the CDL layers to the county
for(k in 2013:2017){
  CDL<-eval(parse(text=paste("CDL_",k,sep="")))
  CDL<-crop(CDL, county)
  CDL<-mask(CDL, county)
  assign(paste0("CDL_",k),CDL)
}

#separate the CDL crops
wd<-paste0("./", co, "/CDL_Acc/")
pb<-progress_bar$new(total=560) #set up a progress bar
pb$tick(0) #start the progress bar at 0

for(k in 2013:2017){
  for(i in c(1:6, 10:14, 21:39, 41:61, 66:69, 71, 72, 74:77, 92, 176, 181, 204:250, 254)){ 
    crop<-eval(parse(text=paste("CDL_", k, sep=""))) #load the CDL raster for year k
    values(crop)[values(crop)!=i]<-0 #set any values that are not crop i to 0
    acc<-Acc_df%>%select("Attribute.Code",paste0("X",k)) #get the accuracy data for year k, crop i
    acc<-as.matrix(acc) #format
    colnames(acc)<-NULL #format
    err<-Err_df%>%select("Attribute.Code", paste0("X", k)) #get the error data for year k, crop i
    err<-as.matrix(err) #format
    colnames(err)<-NULL #format
    file_a<-reclassify(crop, acc, right=NA) #reclassify the crop raster, so the cells are set to the crop's accuracy value
    file_e<-reclassify(crop, err, right=NA) #reclassify the crop raster, so the cells are set to the crop's error value
    acc_stack<-stack(crop, file_a, file_e) #make a stack
    fl<-paste0(co, "_",k,"_",i,"_stack.tif") #set up the new file name, based on k and i
    writeRaster(acc_stack,  paste(wd,fl, sep=""), format="GTiff", overwrite=T) #save the raster stack
    pb$tick() #update the progress bar
  }
}

#### Reclassify the CDL crops ####
#Based on crop reclassification key

#set up reclassification keys
rastlist<-c(3, 6, 10:14, 23, 25, 27, 29:36, 38, 39, 41:61, 66:69, 71, 72, 74:77, 204, 206:208,
            210:224, 229, 242:250) 
old<-c(3, 6, 10:14, 23, 25, 27, 29:36, 38, 39, 41:61, 66:69, 71, 72, 74:77, 204, 206:208,
       210:224, 229, 242:250)
new<-c(3, 6:11, 14, 16, 17, 19:26, 28:61, 63:65, 67:81, 83:92)
df<-as.data.frame(cbind(old, new))

#First, reclassify the rasters that do not need to be recombined
wd<-paste0("E:/EPA Work/Prob Crop Proj Jan 2021/", co, "/CDL_Comb")

for(k in 2013:2017){
  for(i in rastlist){
    rast<-stack(paste0("./",co,"/CDL_Acc/", co, "_", k, "_", i,"_stack.tif"))
    val<-df$new[df$old==i]
    fl<-paste0(co,"_",k,"_",val,"_stack.tif")
    writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
  }
}

#Re-classify the rasters which needed recombination
#Dbl crop rasters are NOT mutually exclusive (e.g. dbl crop lettuce/durum wht is included in both lettuce and durum wheat)

#Corn: New=1, Old=1, 225, 226, 228, 237, 241
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_1_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_225_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_226_stack.tif"))
  layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_228_stack.tif"))
  layer5<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_237_stack.tif"))
  layer6<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_241_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], layer5[[1]], layer6[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], layer5[[2]], layer6[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], layer5[[3]], layer6[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_1_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Cotton: New=2, Old= 2, 232, 238, 239
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_2_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_232_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_238_stack.tif"))
  layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_239_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co, "_", k, "_2_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Sorghum: New=4, Old=4, 234, 235, 236
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_4_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_234_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_235_stack.tif"))
  layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_236_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co, "_", k, "_4_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}
  
#Soybeans: New=5, Old=5, 26, 239, 240, 254
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_5_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_26_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_239_stack.tif"))
  layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_240_stack.tif"))
  layer5<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_254_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], layer5[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], layer5[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], layer5[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_5_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Barley: New = 12, Old=21, 233, 235, 237, 254
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_21_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_233_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_235_stack.tif"))
  layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_237_stack.tif"))
  layer5<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_254_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], layer5[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], layer5[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], layer5[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_12_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Durum Wheat: New=13, Old=22, 230, 234
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_22_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_230_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_234_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_13_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Winter Wheat: New=15, Old=24, 26, 225, 236, 238
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_24_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_26_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_225_stack.tif"))
  layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_236_stack.tif"))
  layer5<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_238_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], layer5[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], layer5[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], layer5[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_15_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Oats: New = 18, Old=28, 226, 240
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_28_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_226_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_240_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_18_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Other Hay/Non Alfalfa: New=27, Old=37, 176, 181
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_37_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_176_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_181_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_27_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Triticale: New = 62, Old=205, 228
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_205_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_228_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_62_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Cantaloupes: New = 66, Old= 209, 231
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_209_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_231_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_66_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#Lettuce: New = 82, Old = 227, 230, 231, 232, 233
for(k in 2013:2017){
  layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_227_stack.tif"))
  layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_230_stack.tif"))
  layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_231_stack.tif"))
  layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_232_stack.tif"))
  layer5<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_233_stack.tif"))
  pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], layer5[[1]], na.rm=T)
  acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], layer5[[2]], na.rm=T)
  err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], layer5[[3]], na.rm=T)
  rast<-stack(pres, acc, err)
  fl<-paste0(co,"_",k,"_82_stack.tif")
  writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
}

#### End ####
