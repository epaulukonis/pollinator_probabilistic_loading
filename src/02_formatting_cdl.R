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

#set parameters for memory to lower value
rasterOptions(memfrac=.3)

#sget 2005-1999
set_1<-cdl_data[c(1:7)]
#get 2006-2007
set_2<-cdl_data[c(8:9)]
#get rest
cdl_base<-cdl_data[c(10:22)]

r1<-cdl_data[[22]]
out_1<-lapply(set_1, function(file){
  projectRaster(file, r1, method='ngb',crs(r1))
})

out_2<-lapply(set_2, function(file){
  projectRaster(file, r1, method='ngb',crs(r1))
})

cdl_out<-stack(out_1, out_2, cdl_base)


writeRaster(cdl_out, filename = file.path(cdl_dir, "cdl_final.tif"), bylayer=TRUE, format="GTiff")
cdl_f<-file.path(cdl_dir,
                   list.files(path=cdl_dir, pattern='_final.tif$', all.files=TRUE, full.names=FALSE))
cdl_f<-lapply(cdl_f, raster) #list new projected/fixed rasters


#Get accuracy and error data
#https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section3_22.0

#note: users will need to format and organize CDL accuracy and error as specified in the demo data provided
#if using code below
cdl_acc<-file.path(cdl_acc_dir,
                 list.files(path=cdl_acc_dir, pattern='.csv', all.files=TRUE, full.names=FALSE))
cdl_acc<-list(cdl_acc)

#make function mask and crop to county
mask_cdl<-function(y){
  CDL<-crop(y, county)
  CDL<-mask(y, county)
}

county_names<- state$COUNTY_NAM
for (county in county_names){

}


co<-"PEORIA" #set county
county<-state[state$COUNTY_NAM == co,]

#think about farming out the core processes to each state?
#parrellel package

print("CDL formatted, reconstructed, and corrected for accuracy/error")

