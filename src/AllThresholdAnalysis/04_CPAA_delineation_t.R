### Probabilistic Crop Loading 

### 04 Delineation of Fields

# Edited by E. Paulukonis August 2022

import_start_time <- Sys.time()
print("stepping into 04_CPAA_delineation.R")
options(scipen = 999) #remove exponent options, throws R off

ill_cpaa<-paste0(root_data_out, "/all_tif/ILLINOIS/CPAA/thresh1.shp")
mi_cpaa<-paste0(root_data_out, "/all_tif/MICHIGAN/CPAA/thresh1.shp")
wi_cpaa<-paste0(root_data_out, "/all_tif/WISCONSIN/CPAA/thresh1.shp")

if(file.exists(mi_cpaa) && file.exists(wi_cpaa) && file.exists(ill_cpaa)){
  mi_fields<-list() 
  wi_fields<-list()
  ill_fields<-list()
  
  mi_fields[1]<- readOGR(paste0(cdl_mi_dir,"/CPAA"), layer = "huron75")
  mi_fields[2]<- readOGR(paste0(cdl_mi_dir,"/CPAA"), layer = "oceana25")
  mi_fields[3]<- readOGR(paste0(cdl_mi_dir,"/CPAA"), layer = "vanburen50")
  
  names(mi_fields)<-c("HURON","OCEANA","VAN BUREN")
  
  wi_fields[1]<- readOGR(paste0(cdl_wi_dir,"/CPAA"), layer = "lang15")
  wi_fields[2]<- readOGR(paste0(cdl_wi_dir,"/CPAA"), layer = "rock65")
  wi_fields[3]<- readOGR(paste0(cdl_wi_dir,"/CPAA"), layer = "wau35")
  
  names(wi_fields)<-c("LANG","ROCK","WAU")
  
  #source(file.path(root_src, "05_CPAA_analysis_c.R"))

}else{
  
  
#### Illinois Delineation ----
county_fw_sets<-list()
  thresh_list<-thresh_list_ill
  list_of_nlcd_masks<-nlcd_ill
  
  for (i in 1:length(thresh_list)){
    thresh_layers<-thresh_list[[i]]
    df_n<-thresh_layers[,c(1:2,8)]
    
    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_mi_rec[[1]])
    #plot(df_n)
    
    
    # focal window of 3x3 pixels, or 7x7 (13)
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
      terra::mask(mask = r) 
    fw<-raster(fw) #convert back to raster object
    
    
    #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
    
    # mask out NA areas here using NLCD
    nlcd<-list_of_nlcd_masks[[i]]
    ext<-extent(nlcd)
    fw<-setExtent(fw, ext)
    fw<-crop(fw, nlcd)
    fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
    fw<-mask(fw, nlcd) #mask out by NLCD NA here
    
    
    # this evaluates and removes clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw, directions = 8) 
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this was visual inspection based)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw<-mask(fw, formaskSieve)
    
    # convert to a vector
    fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                          as_points = FALSE, merge = TRUE)) 
    # fill holes 
    area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
    fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
    
    county_fw_sets[[i]]<-fw_fill
    
  }
  
  #add separate function for buffering and cleaning 
  area_thresh <- units::set_units(40460, m^2) #drop crumbs below 10 acres
  expand_shrink_clean<-function(x){
    expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
    shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
    drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
  }
  
  #dropped crumbs < 10 acres, did buffer to smooth of 2 acres
  
  
  #buffered_layers<-lapply(county_fw_sets,expand_shrink_clean
  
  michigan_cpaa<-lapply(county_fw_sets, expand_shrink_clean)
  
  
  
  #writeRaster(fw, file.path(cdl_dir, "/fw_bin.tif"), format="GTiff", overwrite = TRUE)
  # writeOGR(county_fw_sets[[1]], cdl_dir,  "/low_raw", driver = "ESRI Shapefile")
  # writeOGR(county_fw_sets[[2]], cdl_dir,  "/low_med", driver = "ESRI Shapefile")
  # writeOGR(county_fw_sets[[3]], cdl_dir,  "/low_high", driver = "ESRI Shapefile")
  # #writeOGR(delineation_f, cdl_dir,  "/delineation_f", driver = "ESRI Shapefile")
  
  
  writeOGR(michigan_cpaa[[1]], paste0(cdl_mi_dir,"/CPAA"), "huron75", driver = "ESRI Shapefile")
  writeOGR(michigan_cpaa[[2]], paste0(cdl_mi_dir,"/CPAA"), "oceana25", driver = "ESRI Shapefile")
  writeOGR(michigan_cpaa[[3]], paste0(cdl_mi_dir,"/CPAA"),"vanburen50", driver = "ESRI Shapefile")
  
  
  #writeOGR(county_fw_sets[[3]], field_dir,  "high_raw_13", driver = "ESRI Shapefile")
  
  
  
  
#### Michigan Delineation ----
county_fw_sets<-list()
thresh_list<-thresh_list_mi
list_of_nlcd_masks<-nlcd_mi

for (i in 1:length(thresh_list)){
  thresh_layers<-thresh_list[[i]]
  df_n<-thresh_layers[,c(1:2,8)]
  
  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_mi_rec[[1]])
  #plot(df_n)
  
  
  # focal window of 3x3 pixels, or 7x7 (13)
  r<-terra::rast(df_n)
  fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
    terra::mask(mask = r) 
  fw<-raster(fw) #convert back to raster object

  
  #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
  
  # mask out NA areas here using NLCD
  nlcd<-list_of_nlcd_masks[[i]]
  ext<-extent(nlcd)
  fw<-setExtent(fw, ext)
  fw<-crop(fw, nlcd)
  fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
  fw<-mask(fw, nlcd) #mask out by NLCD NA here
  
  
  # this evaluates and removes clumps of pixels (nearest neighbor =  8)
  rc <- clump(fw, directions = 8) 
  f<-freq(rc)
  f<-as.data.frame(f)
  excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this was visual inspection based)
  formaskSieve <- rc
  # assign NA to all clumps whose IDs are found in excludeID
  formaskSieve[rc %in% excludeID] <- NA
  fw<-mask(fw, formaskSieve)
  
  # convert to a vector
  fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                        as_points = FALSE, merge = TRUE)) 
  # fill holes 
  area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
  fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
  
  county_fw_sets[[i]]<-fw_fill
  
}

#add separate function for buffering and cleaning 
area_thresh <- units::set_units(40460, m^2) #drop crumbs below 10 acres
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
  shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
}

#dropped crumbs < 10 acres, did buffer to smooth of 2 acres


#buffered_layers<-lapply(county_fw_sets,expand_shrink_clean

michigan_cpaa<-lapply(county_fw_sets, expand_shrink_clean)



#writeRaster(fw, file.path(cdl_dir, "/fw_bin.tif"), format="GTiff", overwrite = TRUE)
# writeOGR(county_fw_sets[[1]], cdl_dir,  "/low_raw", driver = "ESRI Shapefile")
# writeOGR(county_fw_sets[[2]], cdl_dir,  "/low_med", driver = "ESRI Shapefile")
# writeOGR(county_fw_sets[[3]], cdl_dir,  "/low_high", driver = "ESRI Shapefile")
# #writeOGR(delineation_f, cdl_dir,  "/delineation_f", driver = "ESRI Shapefile")


writeOGR(michigan_cpaa[[1]], paste0(cdl_mi_dir,"/CPAA"), "huron75", driver = "ESRI Shapefile")
writeOGR(michigan_cpaa[[2]], paste0(cdl_mi_dir,"/CPAA"), "oceana25", driver = "ESRI Shapefile")
writeOGR(michigan_cpaa[[3]], paste0(cdl_mi_dir,"/CPAA"),"vanburen50", driver = "ESRI Shapefile")


#writeOGR(county_fw_sets[[3]], field_dir,  "high_raw_13", driver = "ESRI Shapefile")



#### Wisconsin Delineation ----
county_fw_sets<-list()
thresh_list<-thresh_list_wi
list_of_nlcd_masks<-nlcd_wi

for (i in 1:length(thresh_list)){
  thresh_layers<-thresh_list[[i]]
  df_n<-thresh_layers[,c(1:2,8)]
  
  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_wi_rec[[1]])
  #plot(df_n)
  
  
  # focal window of 3x3 pixels, or 7x7 (13)
  r<-terra::rast(df_n)
  fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
    terra::mask(mask = r) 
  fw<-raster(fw) #convert back to raster object

  
  #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
  
  # mask out NA areas here using NLCD
  nlcd<-list_of_nlcd_masks[[i]]
  ext<-extent(nlcd)
  fw<-setExtent(fw, ext)
  fw<-crop(fw, nlcd)
  fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
  fw<-mask(fw, nlcd) #mask out by NLCD NA here
  
  
  # this evaluates and removes clumps of pixels (nearest neighbor =  8)
  rc <- clump(fw, directions = 8) 
  f<-freq(rc)
  f<-as.data.frame(f)
  excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this was visual inspection based)
  formaskSieve <- rc
  # assign NA to all clumps whose IDs are found in excludeID
  formaskSieve[rc %in% excludeID] <- NA
  fw<-mask(fw, formaskSieve)
  
  # convert to a vector
  fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                        as_points = FALSE, merge = TRUE)) 
  # fill holes 
  area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
  fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
  
  county_fw_sets[[i]]<-fw_fill
  
}

#add separate function for buffering and cleaning 
area_thresh <- units::set_units(40460, m^2) #drop crumbs below 10 acres
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
  shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
}

#dropped crumbs < 10 acres, did buffer to smooth of 2 acres


#buffered_layers<-lapply(county_fw_sets,expand_shrink_clean

wisconsin_cpaa<-lapply(county_fw_sets, expand_shrink_clean)



#writeRaster(fw, file.path(cdl_dir, "/fw_bin.tif"), format="GTiff", overwrite = TRUE)
# writeOGR(county_fw_sets[[1]], cdl_dir,  "/low_raw", driver = "ESRI Shapefile")
# writeOGR(county_fw_sets[[2]], cdl_dir,  "/low_med", driver = "ESRI Shapefile")
# writeOGR(county_fw_sets[[3]], cdl_dir,  "/low_high", driver = "ESRI Shapefile")
# #writeOGR(delineation_f, cdl_dir,  "/delineation_f", driver = "ESRI Shapefile")


writeOGR(wisconsin_cpaa[[1]], paste0(cdl_wi_dir,"/CPAA"), "lang15", driver = "ESRI Shapefile")
writeOGR(wisconsin_cpaa[[2]], paste0(cdl_wi_dir,"/CPAA"), "rock65", driver = "ESRI Shapefile")
writeOGR(wisconsin_cpaa[[3]], paste0(cdl_wi_dir,"/CPAA"),"wau35", driver = "ESRI Shapefile")


#writeOGR(county_fw_sets[[3]], field_dir,  "high_raw_13", driver = "ESRI Shapefile")
}
