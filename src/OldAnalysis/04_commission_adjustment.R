### Probabilistic Crop Loading 

### 04 commission error adjustment

# Edited by E. Paulukonis Jan 2022

import_start_time <- Sys.time()
print("stepping into 04_commission_adjustment.R")

cdl_crop_area_file<-paste0(cdl_dir,"/crop_area.csv")
if(file.exists(cdl_crop_area_file)){
# get names of layers, pull out presence, and create empty data frame

  commission_df<-read.csv(paste0(cdl_dir,"/crop_area.csv"))
  
}else{
  names_l<-c()
  names_cdl<-for(n in 1:length(cdl_data_rec)){
    names_l[n]<-names(cdl_data_rec[[n]][[1]])
  }
  # get area of each layer
  Crop_m2<-c()
  for (cdl in 1:length(cdl_data_rec)){
    pres<-cdl_data_rec[[cdl]][[1]]
    pres[pres>0]<-900 # set any values where a crop is present to 900 (m2)
    areaval<-cellStats(pres, sum, na.rm=T) # calculate the area value
    Crop_m2[cdl]<-areaval
  }
  
  # create df to hold area and data info; note that names need to match substr index numbers
  commission_df<-data.frame(Crop_m2)
  commission_df$Layer<-names_l # add layer name
  commission_df$Year<-substr(commission_df$Layer, 5, 8) # get year from layer name
  commission_df$Crop<-substr(commission_df$Layer, 10, 11) # get crop ids from layer name
  commission_df$Crop<-unlist(strsplit(commission_df$Crop, split='_', fixed=TRUE)) # remove unwanted underscore in layer name
  write.csv(commission_df,paste0(cdl_dir,"/crop_area.csv")) 
}

#the

nlcd_crop_acc<-nlcd_data_acc[c(1:8)] #partition into
nlcd_past_acc<-nlcd_data_acc[c(9:16)]

## pasture crop:
#36 = alfalfa

vals<-unique(commission_df$Crop)
pvals<-c(36) #pasture crop codes
pb<-progress_bar$new(total=460)
pb$tick(0)

### NLCD and CDL assignments
# for this, we use a recent nearest neighbor assignment; when a CDL is between years for NLCD (i.e., 2012 from 2011-2013), choose closer NLCD
# NLCD: 2001 - CDL: 1999, 2000, 2001, 2002 
NLCD01<-c("1999","2000","2001","2002")
# NLCD: 2004 - CDL: 2003, 2004
NLCD04<-c("2003","2004")
# NLCD: 2006 - CDL: 2005, 2006
NLCD06<-c("2005","2006")
# NLCD: 2008 - CDL: 2007, 2008, 2009
NLCD08<-c("2007","2008","2009")
# NLCD: 2011 - CDL: 2010, 2011
NLCD11<-c("2010","2011")
# NLCD: 2013 - CDL: 2012, 2013, 2014
NLCD13<-c("2012","2013",'2014')
# NLCD: 2016 - CDL: 2015, 2016, 2017
NLCD16<-c("2015","2016","2017")
# NLCD: 2019 - CDL: 2018, 2019, 2020
NLCD19<-c("2018","2019","2020")

NLCD_list<-list(NLCD01,NLCD04,NLCD06,NLCD08,NLCD11,NLCD13,NLCD16,NLCD19) #put all years in a list

#challenge; in the entire CDL dataset, pull out the stacks with names that are included in the NLCD set of interest

#create function where x goes where NLCDX goes

#pull out names for nested cdl_data_rec layers and rename each item by first layer name
for (x in 1:length(cdl_data_rec)){
names(cdl_data_rec)[[x]]<-names(cdl_data_rec[[x]][[1]]) #got close, need to get names to get str_subset
}

get_cdl_names<-function(cdl){
  set<-str_subset(names(cdl_data_rec),pattern=paste(NLCD19, collapse="|")) # this works
  
  f <- function(x) which(x == set)[1] 
  output<-lapply(cdl_data_rec, f)
  
  which(names(cdl_data_rec[[1]]) == set)[1]
  
  
  cdl_data_rec_u<-unlist(cdl_data_rec)
  output<-cdl_data_rec[names(cdl_data_rec) == set]
}



result <- filter(your_df, grepl(paste(patterns, collapse="|"), Letter))

for(k in c(2013:2017)){
  
  for(c in vals){
   
    #add NLCD accuracy info to this stack
    if(c %in% pvals){
      r<-stack(r, nlcd_past_acc)
    }else{
      r<-stack(r, nlcd_crop_acc)
    }
    
    #do commission adjustment
    r2<-r[[-1]] #get a raster without the presence/absence layer
    r2<-overlay(r2, fun=function(w, x, y, z){(w*y)/((w*y)+(x*z))}, na.rm=T) # this is the formula for the commission adjusted probability of a crop being present in that year and pixel
    
   # w = p(cdl): cdl user's accuracy
   # y = p(nlcd|cdl): conditional probability of correct NLCD class, given CDL was correctly ID'ed
   # x = p(cdl'): cdl commission error
   # z = p(nlcd|cdl): conditional probability of correct NLCD class, given CDL was incorrectly ID'ed
    
    #w,x are from the cdl 2, 3 raster layers (acc, comm)
    #y,z are from the nlcd layers by year, 1 and 2
    
    #what I need to do is for each year that corresponds with the set of cdl data, those need to be lumped together
    
    
    cdl_data_rec[[1]]
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



### for the original probabilistic analysis, we aggregated 5 years of CDL data; in the Richardson paper, they did 6 years

#options: weigh NLCD year, and if 3, uneven (1999, 2001 more than 2000)

# two year crop rotation of soy and corn indicates that within a given field, we rotate soy 1 year, and corn the next






#two things to discuss:

# 1. field delineation
# 2. rotation, and assignment of NLCD/CDL


#IDs
# 1 = corn  
# 5 = soybean 
# 24 = winter wheat
# 25 = other small grains
# 36 = alfalfa * 
# 44 = other crops
# 61 = fallow/idle cropland 

# *pasture
