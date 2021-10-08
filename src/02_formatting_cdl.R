### Probabilistic Crop Loading 

###01 Formatting CDL data

#Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 02_formatting_cdl.R")

print(list.files(path=cdl_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(cdl_dir)
cdl_data <- file.path(cdl_dir, list.files(path=cdl_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))

cdl_data<-lapply(cdl_data, raster) #create list of cdl rasters 

#there are 3 different extents; 2020-2008, 2007-2006, and 2005-1999
# first expand extent 
ext<-extent(cdl_data[[22]])
cdl_data<-lapply(cdl_data, function(x) setExtent(x, ext))

#sget 2005-1999
# set_1<-cdl_data[c(1:7)]

#get 2006-2007
# set_2<-cdl_data[c(8:9)]

#get rest
cdl_base<-cdl_data[c(10:22)]

# r1<-cdl_data[[22]]
# out_1<-lapply(set_1, function(file){
#   projectRaster(file, r1, method='ngb',crs(r1))
# })
# for (i in 1:length(out_1)){
#   writeRaster(out_2[[i]], filename = file.path(cdl_dir, "cdl_final1.tif"), by_layer=T, format="GTiff")
# }
# 
# out_2<-lapply(set_2, function(file){
#   projectRaster(file, r1, method='ngb',crs(r1))
# })
# for (i in 1:length(out_2)){
#   writeRaster(out_2[[i]], filename = file.path(cdl_dir, "cdl_final2.tif"), by_layer=T, format="GTiff")
# }


cdl_f<-file.path(cdl_dir_fin,
                  list.files(path=cdl_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_f<-lapply(cdl_f, raster) #list new projected/fixed rasters
cdl_f<-list(cdl_f, cdl_base)
 

#Get accuracy and error data
#https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section3_22.0

#note: users will need to format and organize CDL accuracy and error as specified in the demo data provided
#if using code below
# cdl_acc<-read.csv(cdl_acc_dir, "CDL_Acc_il.csv")
# 
# #make function mask and crop to county
# co<-"PEORIA" #set county
# county<-state[state$COUNTY_NAM == co,]
# mask_cdl<-function(c){
#   county_m<-crop(c, county)
#   cdl_f<-mask(county_m, county)
# }

#when you get to the point where we'll do it for all counties, follow this:
# county_names<-state$COUNTY_NAM
# cdl_f<-list()
# mask_cdl<-function(y){
#   for (c in 1:length(county_names)){
#     county<-state[state$COUNTY_NAM == county_names[c],]
#   county_m<-crop(y, county)
#   cdl_f[c]<-mask(county_m, county)
# }}
# 




print("CDL formatted, reconstructed, and corrected for accuracy/error")

