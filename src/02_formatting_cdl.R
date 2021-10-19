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
cdl_acc<-read.csv(paste0(cdl_acc_dir,"/CDL_Acc_il.csv"))
cdl_err<-read.csv(paste0(cdl_acc_dir,"/CDL_Err_il.csv"))
nnames<-2008:2020
names(cdl_acc)[3:15]<-nnames
names(cdl_err)[3:15]<-nnames

#instead of setting all NAs to 0.5, as in Budreski et al., we'll use the average of years to backfill missing values
correct_backfill<-function(data){
#impute years of missing data
# data[,16:26]<-NA
# colnames(data)[16:26]<-1997:2007
# data<-data[,c(1:2,16:26,3:15)]
  #take average of rows by crop and year to fill in
o<-data[,1:2]
m<-as.matrix(data[,3:15])
k <- which(is.na(m), arr.ind=TRUE)
m[k] <- rowMeans(m, na.rm=TRUE)[k[,1]]
m<-as.data.frame(m)
m<-round(m, 2)
out<-cbind(o,m)
}

cdl_acc<-correct_backfill(cdl_acc) 
cdl_err<-correct_backfill(cdl_err) 

codes<-cdl_acc[,2] #pull out CDL crop codes
pb<-progress_bar$new(total=1000) #set up a progress bar
pb$tick(0) #start the progress bar at 0
numCores <- detectCores()
print(numCores)

reclassify_cdl<-function(cdl_data){
  for(c in codes){ 
    for(y in 2008:2020){
   cdl <- cdl_data #get the CDL raster by year
    values(cdl)[values(cdl)!=c]<-0 #set any values that are not crop to 0
    acc<-as.matrix(cdl_acc%>%select("Attribute_Code",paste0(y))) #get the accuracy data for year y
    err<-as.matrix(cdl_err%>%select("Attribute_Code",paste0(y))) #get the error data for year y
    file_a<-reclassify(cdl, acc, right=NA) #reclassify the crop raster, so the cells are set to the crop's accuracy value
    file_e<-reclassify(cdl, err, right=NA) #reclassify the crop raster, so the cells are set to the crop's error value
    acc_stack<-stack(cdl, file_a, file_e) #make a stack
    fl<-paste0(co, "_",y,"_",c,"_stack.tif") #set up the new file name, based on k and i
    writeRaster(acc_stack,  paste(cdl_dir, fl, sep=""), format="GTiff", overwrite=T) #save the raster stack
}}}


mcapply(cdl_fin_co, reclassify_cdl, mc.cores=numCores)


print("CDL formatted, reconstructed, and corrected for accuracy/error")

