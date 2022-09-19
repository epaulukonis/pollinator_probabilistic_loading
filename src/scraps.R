#####Get NLCD mask for non-crop areas (roadS)
nlcd<-raster(paste0(nlcd_dir,"/nlcd2019.tif"))
#nlcd<-raster(paste0(nlcd_dir,"/NLCD_2008_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))

list_of_nlcd_masks_wi<-list()
for (nlcd_layer in 1:3){
  cpaa<-county_list[[nlcd_layer]][[1]] ##get associated counties
  ext<-extent(cpaa)
  #plot(cpaa)
  
  #use 2008 NLCD to mask out non-crop
  
  nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
  nlcdc<-crop(nlcdc, cpaa)
  nlcdc<-mask(nlcdc, cpaa)
  
  nlcdc[nlcdc==11]<-NA
  nlcdc[nlcdc==21]<-NA
  nlcdc[nlcdc==22]<-NA
  nlcdc[nlcdc==23]<-NA
  nlcdc[nlcdc==24]<-NA
  nlcdc[nlcdc==31]<-NA
  
  names(nlcdc)<-names(county_list)[[nlcd_layer]][[1]]
  list_of_nlcd_masks_wi[[nlcd_layer]]<-nlcdc
  
}

names(list_of_nlcd_masks_wi)<-names_cc
f<-paste0(root_data_out, "/all_NLCD/Wisconsin")
for(layer in 1:length(list_of_nlcd_masks_wi)){
  writeRaster(list_of_nlcd_masks_wi[[layer]], file.path(f, names(list_of_nlcd_masks_wi[[layer]])), format="GTiff", overwrite = TRUE)
}