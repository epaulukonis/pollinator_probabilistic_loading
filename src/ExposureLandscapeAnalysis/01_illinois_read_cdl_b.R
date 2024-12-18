### B. affinis analysis

### 01 Read CDL data

# Edited by E. Paulukonis March 2022


cdl_rec_filename<-paste0(cdl_dir_adj, "/CDL_2021_17.tif")
if(file.exists(cdl_rec_filename)){
  
  print(list.files(path=cdl_dir_adj, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_ill_rec <- file.path(cdl_dir_adj, list.files(path=cdl_dir_adj, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_ill_rec<-setNames(lapply(cdl_data_ill_rec, raster), tools::file_path_sans_ext(basename(cdl_data_ill_rec)))
  print('the Illinois CDL has already been processed, it can be read in')

  
  
}else{
  
  #### Import and modify files ####
  print(list.files(path=cdl_ill_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  print(cdl_ill_dir)
  cdl_data <- file.path(cdl_ill_dir, list.files(path=cdl_ill_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
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
  cdl_base<-cdl_data[c(10:23)]
  
  #this section is for writing the new rasters out with a new projection and extent; it is commented out because we've already done this section
  #and it takes a while 
  
  # r1<-cdl_data[[22]]
  # out_1<-lapply(set_1, function(file){
  #   projectRaster(file, r1, method='ngb',crs(r1))
  # })
  # for (i in 1:length(out_1)){
  #   writeRaster(out_1[[i]], filename = file.path(cdl_dir, "cdl_final9905.tif"), by_layer=T, format="GTiff")
  # }
  # out_2<-lapply(set_2, function(file){
  #   projectRaster(file, r1, method='ngb',crs(r1))
  # })
  # for (i in 1:length(out_2)){
  #   writeRaster(out_2[[i]], filename = file.path(cdl_dir, "cdl_final0607.tif"), by_layer=T, format="GTiff")
  # }
  
  print(list.files(path=cdl_dir_fin, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_f<-file.path(cdl_dir_fin,
                   list.files(path=cdl_dir_fin, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_f<-lapply(cdl_f, raster) #list new projected/fixed rasters
  
  #get original names 
  list_names<-vector()
  for (n in 1:22){
    list_names[n]<-names(cdl_data[[n]])
  }
  
  #fix names in the fixed extent 1999-2007 set
  names_o<-list_names[1:9]
  for(x in 1:9){
    names(cdl_f[[x]])<-names_o[[x]]
  }
  
  #join fixed and original extent rasters
  cdl_fin<-c(cdl_f, cdl_base) 
  
  
  #mask to the study area
  mask_crop<-function(x){
    r_list<-crop(x, ill)
    mask(r_list, ill)
  }
  cdl_fin_co<-lapply(cdl_fin, mask_crop)
  
  f<-cdl_dir_adj
  #m <- cbind(from = c(-Inf, 80), to = c(0, 200), becomes = c(NA)) #non-crop reclass tables; i.e., reclassify crop and non-crop
  cdl_fin_co_rec<-list()
  for(layer in 1:22){
    cdl_fin_co_rec[[layer]] <- reclassify(cdl_fin_co[[layer]], m)
    writeRaster(cdl_fin_co_rec[[layer]], file.path(f, names(cdl_fin_co_rec[[layer]])), format="GTiff", overwrite = TRUE)
  }
}
