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

# now we'll resample to fix the number of rows and columns in several of the later years
#get 1999-2005
set_1<-cdl_data[c(1:7)]
#get 2006-2007
set_2<-cdl_data[c(8:9)]
#get rest
cdl_base<-cdl_data[c(10:22)]

# r1<-cdl_data[[22]]
# out_1<-lapply(set_1, function(file){
#   projectRaster(file, r1, method='ngb',crs(r1))
# })
# for (i in 1:length(out_1)){
#   writeRaster(out_1[[i]], filename = file.path(cdl_dir, "cdl_final1.tif"), by_layer=T, format="GTiff")
# }
# 
# out_2<-lapply(set_2, function(file){
#   projectRaster(file, r1, method='ngb',crs(r1))
# })
# for (i in 1:length(out_2)){
#   writeRaster(out_2[[i]], filename = file.path(cdl_dir, "cdl_final2.tif"), by_layer=T, format="GTiff")
# }



cdl_f<-file.path(cdl_dir_fin,
                  list.files(path=cdl_dir_fin, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_f<-lapply(cdl_f, raster) #list new projected/fixed rasters
#assign original year names 
list_names<-vector()
for (n in 1:22){
  list_names[n]<-names(cdl_data[[n]])
}
cdl_fin<-c(cdl_f, cdl_base)
names(cdl_fin)<-list_names


#make function mask and crop to county
co<-"PEORIA" #set county
county<-state[state$COUNTY_NAM == co,]
mask_crop_cdl<-function(cdl){
  cdl_list<-crop(cdl, county)
  mask(cdl_list, county)
}
cdl_fin_co<-lapply(cdl_fin, mask_crop_cdl)


# when you get to the point where we'll do it for all counties, follow this:
# county_names<-state$COUNTY_NAM
# cdl_fin_co<-list()
# mask_crop_cdl<-function(cdl){
#   for (c in 1:length(county_names)){
#   county<-state[state$COUNTY_NAM == county_names[c],]
#   county_c<-crop(cdl, county)
#   cdl_fin_co[c]<-mask(county_c, county)
# }}
# you'll have a nested list of stacked rasters by county

 

#Get accuracy and error data
#https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section3_22.0

#note: users will need to format and organize CDL accuracy and error as specified in the demo data provided
#if using code below
cdl_acc<-read.csv(cdl_acc_dir, "CDL_Acc_il.csv")





print("CDL formatted, reconstructed, and corrected for accuracy/error")

