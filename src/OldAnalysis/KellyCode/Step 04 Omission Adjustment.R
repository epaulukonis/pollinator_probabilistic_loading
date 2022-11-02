#A script to perform the omission adjustment
# Last edited KR McCaffrey Jan 2021

###OUTSIDE OF R: Do prep work for omission adjustment. Compare the commission-adjusted area
##estimates to the Ag Census values for the corresponding crop(s).
##If Ag Census acreage values reported = (D) [data withheld], considered NA, no omission adjustment performed
##If Ag Census acreage values reported = (Z) [<0.5], considered NA, no omission adjustment performed
##If Ag Census CV value reported =(H) [CV >99.95], record considered NA
##These procedures were done to avoid setting arbitrary values that may introduce error

##Find the state adjustment ratio and determine the necessary omission adjustment
##File is saved as [County]_Crop_Area_Comparison.csv and .xlsx

##Omission adjustment code:
#1 = within confidence interval, no ommission adjustment needed
#2 = CoA was not NA, commission-adjusted area was not within 90% CI, the state adjustment ratio was not >1. Multiply the probability values by the state adjustment ratio
#3 = CoA was not NA, commission-adjusted area was not within 90% CI, the state adjustment ratio was >1. Create omisison-adjustment raster from NLCD
#4 = Original CDL values were 0, no omission adjustment.
#5 = CDL is >0, commission-adjusted area is 0. No omission adjustment
#6 = The commission-adjusted area is >0, but the CoA is NA. No omission adjustment.

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

Om_Key<-read.csv(paste0("./", co,"/",co,"_Crop_Area_Comparison.csv"))
NLCD82<-raster(paste0("./", co, "/NLCD_82_",co,".tif"), format="GTiff", overwrite=T)
NLCD81<-raster(paste0("./", co, "/NLCD_81_", co, ".tif"), format="GTiff", overwrite=T)
pvals<-c(26, 27, 49, 81)

#make sure numeric values are classed correctly
#these lines of code may need to be changed
Om_Key$CoA_Acres<-as.numeric(as.character(Om_Key$CoA_Acres))

pb<-progress_bar$new(total=460)
pb$tick(0)
wd<-paste0("./",co,"/Comm_Prob/")
wd2<-paste0("./",co,"/Crop_Prob/")
wd3<-paste0("./",co,"/CDL_Comb/")

# if(key==1 | key==4 | key==5 | key==6){
#   print("1")
# }else if(key==2){
#   print("2")
# }else{
#   print("3")
# }

for(k in c(2013:2017)){ #for each of 5 years
  for(i in c(1:92)){ #each crop category
    comm<-raster(paste0(wd, co,"_Comm_",k,"_",i,".tif"))
    Comm_Acres<-Om_Key$Comm_Acres[Om_Key$Year==k & Om_Key$Crop==i] #get the comission adjusted CDL acreage
    Census_Acres<-Om_Key$CoA_Acres[Om_Key$Year==k & Om_Key$Crop==i] #get the Ag census acreage
    key<-Om_Key$Adjustment_Code[Om_Key$Year==k &Om_Key$Crop==i] #get the key for the correct year and crop
    if(key==1 | key==4 | key==5 | key==6){ #if the adjustment code is 1, 4, 5, or 6, save the comission probability raster as the final probability raster
      writeRaster(comm, paste0(wd2, co, "_",k,"_", i, "_prob.tif"), format="GTiff", overwrite=T)
    }else if(key==2){
      ratio<-Om_Key$State_Adj_Ratio[Om_Key$Year==k & Om_Key$Crop==i]
      prob<-comm*ratio
      writeRaster(prob, paste0(wd2, co, "_", k, "_", i, "_prob.tif"), format="GTiff", overwrite=T)
    }else{ #key==3
      #first, will need to decide if crop is a row crop or pasture crop
      if(i %in% pvals){ #for pasture crops, we used NCLD 81 for this step
        s_pres<-stack() #create an empty stack
        s<-seq(2013,2017,1) #for each CDL year
        s<-s[s!=k] #get a sequence WITHOUT the year we're working on
        for(z in s){ #for each number in this sequence,
          pres<-stack(paste0(wd3, co, "_",z,"_",i,"_stack.tif"))
          pres<-pres[[1]] #get only the presence layer
          pres[pres>=1]<-1 #set presence values =1
          s_pres<-stack(s_pres, pres) #this makes a stack of all of presence pixels through all of the "other" years
        }
        s_rast<-sum(s_pres, na.rm=T) #this should keep all presence pixels
        s_rast[s_rast>=1]<-1 #set back to 1
        pix<-NLCD81*s_rast #keep all NLCD81 pixels where s_rast!=0
        area_pix<-pix
        area_pix[area_pix>0]<-900 #set the area of each presence pixel to 900 m2
        area_pix_val<-cellStats(area_pix, sum, na.rm=T)#find the area of all NLCD pixels that were not cat in CDL year, but were in any other
        #covert this area value to acres for calculation
        area_pix_val<-area_pix_val*0.000247105 #one square meter is 0.000247105 acres
        #prob value of these pixels is:
        #((Ag Census Acreage)-(Commission Adjusted CDL Acreage))/NLCD pixel acreage
        om_prob<-(Census_Acres-Comm_Acres)/area_pix_val
        #assign this value to all the pixels
        area_pix[area_pix!=0]<-(om_prob/100)
        #join with commission probability raster - keep the maximum values. THis should keep comm prob if already filled in. If comm prob =0, will assign om prob value 
        prob<-mosaic(comm, area_pix, fun=max, na.rm=T)
        writeRaster(prob, paste0(wd2, co,"_",k,"_",i,"_prob.tif"), format="GTiff", overwrite=T)
      }else{ #if it's not a pasture crop, we use NLCD 82
        s_pres<-stack()#create an empty stack
        s<-seq(2013, 2017, 1) #for each CDL year
        s<-s[s!=k] #get a sequence WITHOUT the year we're working on
        for(z in s){
          pres<-stack(paste0(wd3, co, "_",z,"_",i,"_stack.tif"))
          pres<-pres[[1]] #get only the presence layer
          pres[pres>=1]<-1 #set presence values = 1
          s_pres<-stack(s_pres, pres) #this makes a stack of all presence pixels though all of the "other" years
        }
        s_rast<-sum(s_pres, na.rm=T) #this should keep all presence pixels
        s_rast[s_rast>=1]<-1 #set back to 1
        pix<-NLCD82*s_rast # keep all NLCD82 pixels where s_rast!=0
        area_pix<-pix
        area_pix[area_pix>0]<-900 #set the area of each presence pixel to 900 m2
        area_pix_val<-cellStats(area_pix, sum, na.rm=T)#find the area of all NLCD pixels that were not cat in CDL year, but were in any other
        #covert this area value to acres for calculation
        area_pix_val<-area_pix_val*0.000247105 #one square meter is 0.000247105 acres
        #prob value of these pixels is:
        #((Ag Census Acreage)-(Commission Adjusted CDL Acreage))/NLCD pixel acreage
        om_prob<-(Census_Acres-Comm_Acres)/area_pix_val 
        #assign this value to all the pixels
        area_pix[area_pix!=0]<-(om_prob/100)
        #join with the commission probability raster - keep the maximum values
        prob<-mosaic(comm, area_pix, fun=max, na.rm=T)
        writeRaster(prob, paste0(wd2, co,"_",k,"_",i,"_prob.tif"), format="GTiff", overwrite=T)
      }
    }
    pb$tick()
  }
} 

##### Average for each crop over 5 years (2013-2017), conserve all (+) CDL pixels #####
#2013-2017
wd<-paste0("./",co, "/Crop_Prob_Final/")
wd2<-paste0("./",co, "/CDL_Comb/")
wd3<-paste0("./",co, "/Crop_Prob/")
wd4<-paste0("./",co,"/Crop_Prob_Final_no0001/")

temp<-raster(paste0("./", co, "/CDL_Comb/", co, "_2013_1_stack.tif"))

rast<-raster(ext=extent(temp), crs=x,
             resolution=c(30,30))

pb<-progress_bar$new(total=92)
pb$tick(0)
for(i in 1:92){
  crop_stack<-stack()
  crop_stack<-stack(rast)
  #get each year of the crop
  crop_stack<-stack(crop_stack, paste0(wd3, co, "_2013_",i,"_prob.tif"))
  crop_stack<-stack(crop_stack, paste0(wd3, co, "_2014_",i,"_prob.tif"))
  crop_stack<-stack(crop_stack, paste0(wd3, co, "_2015_",i,"_prob.tif"))
  crop_stack<-stack(crop_stack, paste0(wd3, co, "_2016_",i,"_prob.tif"))
  crop_stack<-stack(crop_stack, paste0(wd3, co, "_2017_",i,"_prob.tif"))
  #find the 5 year average probability
  #find the weighted average, placing more weight on more recent data
  wt<-c(1, 2, 3, 4, 5)
  crop<-weighted.mean(crop_stack, w=wt, na.rm=T)
  #crop<-mean(crop_stack, na.rm=T)
  #save without adding pixels back in
  writeRaster(crop, paste0(wd4, co, "_",i,"prob_w.tif"), overwrite=T)
  #ID pixels that were the crop in ANY year of the CDL
  s_pres<-stack()
  for(k in 2013:2017){
    pres<-stack(paste0(wd2, co, "_",k,"_",i,"_stack.tif")) #get file for current crop and years
    pres<-pres[[1]] #only the presence layer
    pres[pres>=1]<-1 #set presence values to 1
    s_pres<-stack(s_pres, pres) #make a stack of all of the presence pixels through time
  }
  pres<-max(s_pres, na.rm=T) #this should keep all of the presence pixels
  pres[pres==1]<-0.0001 #set the value of these pixels to 0.0001
  crop_prob<-max(crop, pres, na.rm=T) #join the two rasters. 0.0001 is a small enough value that any pixels that are now 0 will be change to 0.0001 if they were CDL at all. Otherwise, calculated probabilityvalues will be maintained
  #save the results
  writeRaster(crop_prob, paste0(wd, co, "_",i,"prob_w.tif"), format="GTiff", overwrite=T)
  pb$tick()
}

####End####