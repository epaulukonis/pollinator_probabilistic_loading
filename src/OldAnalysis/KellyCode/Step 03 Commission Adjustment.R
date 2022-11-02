#A script to perform the commission adjustment
# Last edited KR McCaffrey Jan 2021

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(progress)

#set the county
co<-"Stanislaus"

#load the county shapefile
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/Spatial Data/CA_Counties/")
counties<-readOGR(".","CA_Counties_TIGER2016")
crs(counties)
county<-subset(counties, counties$NAME==co)
rm(counties)

x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
county<-spTransform(county, x)

#set the base working directory
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/")

#calculate the area of the CDL rasters, categories 1-92
wd<-paste0("E:/EPA Work/Prob Crop Proj Jan 2021/", co, "/CDL_Comb")
area_df<-data.frame(matrix(ncol=4, nrow=460))#set up an empty data frame with the correct number of rows and columns
colnames(area_df)<-c("Year","Crop","CDL_m2","Comm_m2")#give the columns names
area_df$Year<-c(rep(2013, 92), rep(2014, 92), rep(2015, 92), rep(2016, 92), rep(2017, 92)) #input years
area_df$Crop<-c(rep(1:92, 5)) #input crop IDs

pb<-progress_bar$new(total=460)
pb$tick(0)
for(k in 2013:2017){
  for(i in 1:92){
    rast<-stack(paste(wd, paste0(co, "_", k, "_", i, "_stack.tif"), sep="/"))
    pres<-rast[[1]] #just get presence/absence
    pres[pres>0]<-900 #set any values where a crop is preesent to 900 (m2)
    areaval<-cellStats(pres, sum, na.rm=T) #find the total area of the CDL crop
    area_df$CDL_m2[area_df$Year==k & area_df$Crop==i]<-areaval
    pb$tick()
  }
}

#save the table so far
write.csv(area_df, paste0("./", co, "/", co,"_Crop_Area.csv"))

#read in NLCD accuracy data
NLCD_crop_acc<-stack(paste0("./", co, "/NLCD_crop_acc_stack_", co, ".tif"))
NLCD_past_acc<-stack(paste0("./", co, "/NLCD_past_acc_stack_", co, ".tif"))

# #read the crop data frame in
area_df<-read.csv(paste0("./",co,"/",co,"_Crop_Area.csv"))

wd<-paste("./",co,"/CDL_Comb/", sep="")
wd2<-paste("./",co,"/Comm_Prob/", sep="")

#26=Alfalfa, 27=Other Hay/Non Alfalfa, 49=Switchgrass, 81=Vetch
pvals<-c(26, 27, 49, 81) #pasture crop codes
county<-spTransform(county, crs(NLCD_crop_acc))
pb<-progress_bar$new(total=460)
pb$tick(0)
for(k in c(2013:2017)){
  for(i in c(1:92)){
    #load in CDL raster stack
    r<-stack(paste(wd, paste0(co,"_",k,"_",i,"_stack.tif"), sep=""))
    #add NLCD accuracy info to this stack
    if(i %in% pvals){
      r<-stack(r, NLCD_past_acc)
    }else{
      r<-stack(r, NLCD_crop_acc)
    }
    #do commission adjustment
    r2<-r[[-1]] #get a raster without the presence/absence layer
    r2<-overlay(r2, fun=function(w, x, y, z){(w*y)/((w*y)+(x*z))}, na.rm=T)
    #we will get NA values because of math - replace NA values with 0
    r2[is.na(r2[])]<-0
    #clip to the area of our county
    r2<-mask(r2, county)
    #save the commission-adjusted raster
    fl<-paste0(co, "_Comm_",k,"_",i,".tif")
    writeRaster(r2, paste(wd2, fl, sep=""), format="GTiff", overwrite=T)
    #find the area of commission adjustment, save to the data frame
    areacomm<-r2
    areacomm[areacomm>=0]<-900 #set all sqaures to 900 m2
    areacomm<-areacomm*r2 #adjust by new probability values
    areacommval<-cellStats(areacomm, sum, na.rm=T)
    #shove the value into our data frame
    area_df$Comm_m2[area_df$Year==k & area_df$Crop==i]<-areacommval
    pb$tick()
    
  }
}

#save the area calculations
write.csv(area_df, paste0("./", co, "/", co,"_Crop_Area.csv"))

