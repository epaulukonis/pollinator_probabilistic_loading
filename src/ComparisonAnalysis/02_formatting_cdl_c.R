### Probabilistic Crop Loading 

### 02 Formatting CDL data

# Edited by E. Paulukonis Sept 2021
import_start_time <- Sys.time()
print("stepping into 02_formatting_cdl_c.R")

#### Multistate-Specific----

### Michigan-Specific 
cdl_rec_filename<-paste0(cdl_mi_dir, "/rec_cdl/CDL_2021_26.tif")
if(file.exists(cdl_rec_filename)){
  
  print(list.files(path=paste0(cdl_mi_dir,"/rec_cdl"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_mi_rec <- file.path(paste0(cdl_mi_dir,"/rec_cdl"), list.files(path=paste0(cdl_mi_dir,"/rec_cdl"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_mi_rec<-lapply(cdl_data_mi_rec, raster) #create list of reclassed and stacked cdl rasters 
  
  print('the Michigan CDL has already been processed, it can be read in')
  
}else{

#### Import and modify files ####
print(list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(cdl_mi_dir)
cdl_data <- file.path(cdl_mi_dir, list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_data<-lapply(cdl_data, raster) #create list of cdl rasters 


# first expand extent 
ext<-extent(cdl_data[[14]])
cdl_data<-lapply(cdl_data, function(x) setExtent(x, ext)) #make sure all have the same extent


study<-mi
#mask to the study area
mask_crop<-function(x){
  r_list<-crop(x, study)
  mask(r_list, study)
}
cdl_mi_co<-lapply(cdl_data, mask_crop)

plot(cdl_mi_co[[1]][[1]])

f<-paste0(cdl_mi_dir, "/rec_cdl")

m <- cbind(from = c(-Inf, 80), to = c(0, 200), becomes = c(NA)) #non-crop reclass tables; i.e., reclassify crop and non-crop
cdl_mi_rec<-list()
for(layer in 1:14){
cdl_mi_rec[[layer]] <- reclassify(cdl_mi_co[[layer]], m)
writeRaster(cdl_mi_rec[[layer]], file.path(f, names(cdl_mi_rec[[layer]])), format="GTiff", overwrite = TRUE)
}
}




### Wisconsin-Specific 
cdl_rec_filename<-paste0(cdl_wi_dir, "/rec_cdl/CDL_2021_55.tif")
if(file.exists(cdl_rec_filename)){
  
  print(list.files(path=paste0(cdl_wi_dir,"/rec_cdl"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_wi_rec <- file.path(paste0(cdl_wi_dir,"/rec_cdl"), list.files(path=paste0(cdl_wi_dir,"/rec_cdl"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_wi_rec<-lapply(cdl_data_wi_rec, raster) #create list of reclassed and stacked cdl rasters 
  
  print('the Wisconsin CDL has already been processed, it can be read in')
  
}else{
  
  #### Import and modify files ####
  print(list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  print(cdl_wi_dir)
  cdl_data <- file.path(cdl_wi_dir, list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data<-lapply(cdl_data, raster) #create list of cdl rasters 
  
  
  # first expand extent 
  ext<-extent(cdl_data[[14]])
  cdl_data<-lapply(cdl_data, function(x) setExtent(x, ext)) #make sure all have the same extent
  
  
  study<-wi
  #mask to the study area
  mask_crop<-function(x){
    r_list<-crop(x, study)
    mask(r_list, study)
  }
  cdl_wi_co<-lapply(cdl_data, mask_crop)
  
  plot(cdl_wi_co[[1]][[1]])
  
  f<-paste0(cdl_wi_dir, "/rec_cdl")
  
  m <- cbind(from = c(-Inf, 80), to = c(0, 200), becomes = c(NA)) #non-crop reclass tables; i.e., reclassify crop and non-crop
  cdl_wi_rec<-list()
  for(layer in 1:14){
    cdl_wi_rec[[layer]] <- reclassify(cdl_wi_co[[layer]], m)
    writeRaster(cdl_wi_rec[[layer]], file.path(f, names(cdl_wi_rec[[layer]])), format="GTiff", overwrite = TRUE)
  }
}






#for additional years that may need to be added: add in the index number in the CDL_data. 

# x=23
# 
# CDL_2021_17<-crop(cdl_data[[x]], study)
# CDL_2021_17<-mask(CDL_2021_17, study)
# f<-cdl_dir_adj
# m <- cbind(from = c(-Inf, 80), to = c(0, 200), becomes = c(NA)) #non-crop reclass tables
# CDL_2021_17 <- reclassify(CDL_2021_17, m)
# writeRaster(CDL_2021_17, file.path(f, names(CDL_2021_17)), format="GTiff", overwrite = TRUE)
