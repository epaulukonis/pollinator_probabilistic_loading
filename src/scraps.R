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



# 
# #code for trying to get intersection to work...another way?
# cpaa<-cpaa_list_ill_f[[1]]
# final_layer<-st_intersection(sf::st_as_sf(output_of_cleaning[[1]]),sf::st_as_sf(cpaa[[1]])) #let's get the areas in Illinois where they have high probability of distribution
# 
# area_thresh <- units::set_units(8100 , m^2) #2 acres for dropping polygons
# drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
# 
# setwd(root_data_out)
# final_layer<-st_collection_extract(final_layer, "POLYGON")
# st_write(final_layer,"test2.shp")
# 
# #writeRaster(fw_s, file.path(cdl_dir, "/fw_6_test.tif"), format="GTiff", overwrite = TRUE)
# #writeOGR(output_of_cleaning[[3]], cdl_dir,  "output_sd_high_newn", driver = "ESRI Shapefile")
# #we'll use the new vectors to look at focal window of sequences in each field above a certain size
# 
# 
# 
# writeOGR(output_of_cleaning[[1]], root_data_out,  "test_champaignesub3", driver = "ESRI Shapefile")
# 
# st_write(final_layer, paste0(root_data_out,"/test_inst.shp"))
# 
# 
# 
# 
# writeOGR(output_of_cleaning[[3]], field_dir,  "high_sd_77", driver = "ESRI Shapefile")
