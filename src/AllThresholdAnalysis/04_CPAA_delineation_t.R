### Probabilistic Crop Loading 

### 04 Delineation of Fields

# Edited by E. Paulukonis September 2022


## NOTE: All counties must assumed to be alphabetical as they are being read in !!!!!

import_start_time <- Sys.time()
print("stepping into 04_CPAA_delineation.R")
options(scipen = 999) #remove exponent options, throws R off

ill_cpaa_t<-paste0(root_data_out, "/all_tif/ILLINOIS/CPAA/thresh1.shp")
mi_cpaa_t<-paste0(root_data_out, "/all_tif/MICHIGAN/CPAA/thresh1.shp")
wi_cpaa_t<-paste0(root_data_out, "/all_tif/WISCONSIN/CPAA/thresh1.shp")

if(file.exists(mi_cpaa_t) && file.exists(wi_cpaa_t) && file.exists(ill_cpaa_t)){

  print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  ill_cpaa<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  ill_cpaa<-setNames(lapply(ill_cpaa, raster), tools::file_path_sans_ext(basename(ill_cpaa)))
  
  print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  mi_cpaa<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  mi_cpaa<-setNames(lapply(mi_cpaa, raster), tools::file_path_sans_ext(basename(mi_cpaa)))
  
  print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  wi_cpaa<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  wii_cpaa<-setNames(lapply(wi_cpaa, raster), tools::file_path_sans_ext(basename(wi_cpaa)))

}else{
  
  
#### Illinois Delineation ----
fw_sets<-list()
cpaa_field_set<-list()

names(thresh_list_ill_f)<-c("Champaign","DuPage","McHenry")

for (c in 1:length(thresh_list_ill_f)){
  thresh_list<-thresh_list_ill_f[[c]]
  
  for (y in 1:length(thresh_list)){
    thresh_layers<-thresh_list[[y]]
    df_n<-thresh_layers[,c(1:2,8)]

    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_ill_rec[[1]])
    #plot(df_n)
    
    df_n
    
    #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
    # mask out NA areas here using NLCD
    # focal window of 3x3 pixels, or 7x7 (13)
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
      terra::mask(mask = r) 
    fw<-raster(fw) #convert back to raster object
    
    
  ##this part is somewhat complicated; here we need to match the nlcd with the year and county
    Ch<-'Champaign'
    Du<-'DuPage'
    Mc<-'McHenry'
      
    if(i <= 2 && grepl(Ch, names(thresh_list_ill_f[[1]]), fixed = TRUE)){nlcd<-nlcd_ill[[1]]} 
    if(i >2 && i <=4 && grepl(Ch, names(thresh_list_ill_f[[1]]), fixed = TRUE)){nlcd<-nlcd_ill[[2]]} 
    if(i >4 && i <=7 && grepl(Ch, names(thresh_list_ill_f[[1]]), fixed = TRUE)){nlcd<-nlcd_ill[[3]]} 
    if(i >7 && i <=10 && grepl(Ch, names(thresh_list_ill_f[[1]]), fixed = TRUE)){nlcd<-nlcd_ill[[4]]} 
    if(i >10 && grepl(Ch, names(thresh_list_ill_f[[1]]), fixed = TRUE)){nlcd<-nlcd_ill[[5]]}
    
    if(i <= 2 && grepl(Du, names(thresh_list_ill_f[[2]]), fixed = TRUE)){nlcd<-nlcd_ill[[6]]} 
    if(i >2 && i <=4 && grepl(Du, names(thresh_list_ill_f[[2]]), fixed = TRUE)){nlcd<-nlcd_ill[[7]]} 
    if(i >4 && i <=7 && grepl(Du, names(thresh_list_ill_f[[2]]), fixed = TRUE)){nlcd<-nlcd_ill[[8]]} 
    if(i >7 && i <=10 && grepl(Du, names(thresh_list_ill_f[[2]]), fixed = TRUE)){nlcd<-nlcd_ill[[9]]} 
    if(i >10 && grepl(Du, names(thresh_list_ill_f[[2]]), fixed = TRUE)){nlcd<-nlcd_ill[[10]]}
    
    
    if(i <= 2 && grepl(Mc, names(thresh_list_ill_f[[3]]), fixed = TRUE)){nlcd<-nlcd_ill[[11]]} 
    if(i >2 && i <=4 && grepl(Mc, names(thresh_list_ill_f[[3]]), fixed = TRUE)){nlcd<-nlcd_ill[[12]]} 
    if(i >4 && i <=7 && grepl(Mc, names(thresh_list_ill_f[[3]]), fixed = TRUE)){nlcd<-nlcd_ill[[13]]} 
    if(i >7 && i <=10 && grepl(Mc, names(thresh_list_ill_f[[3]]), fixed = TRUE)){nlcd<-nlcd_ill[[14]]} 
    if(i >10 && grepl(Mc, names(thresh_list_ill_f[[3]]), fixed = TRUE)){nlcd<-nlcd_ill[[15]]}
    
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
    
    fw
    
    # convert to a vector
    fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                          as_points = FALSE, merge = TRUE)) 
    

    # fill holes 
    area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
    fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
    #plot(fw_fill)
    
    fw_sets[[y]]<-fw_fill
    names(fw_sets)<-names(thresh_list_ill_f[c])
    
  }
  
  cpaa_field_set[[c]]<-fw_sets
  
}
  
names(cpaa_field_set)<-names(thresh_list_ill_f)

  #add separate function for buffering and cleaning, drop crumbs < 10 acres, do buffer to smooth of 8 acres
  area_thresh <- units::set_units(40460, m^2) #drop crumbs below 10 acres
  expand_shrink_clean<-function(x){
    expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
    shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
    drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE) }
    

cpaa_final_batch<-list()
for(layer in 1:length(cpaa_field_set)){
 fw_analysis<-cpaa_field_set[[layer]]
 illinois_cpaa<-lapply(fw_analysis, expand_shrink_clean)
 cpaa_final_batch[[layer]]<-illinois_cpaa
 for(cpaa in 1:length(illinois_cpaa)){
 writeOGR(illinois_cpaa[[cpaa]], paste0(root_data_out, "/all_tif/ILLINOIS/CPAA)"), paste0(names(cpaa_field_set[[layer]]),"_threshold",cpaa), driver = "ESRI Shapefile")

 }
}
  


#### Michigan Delineation ----
# county_fw_sets<-list()
# thresh_list<-thresh_list_mi
# list_of_nlcd_masks<-nlcd_mi
# 
# for (i in 1:length(thresh_list)){
#   thresh_layers<-thresh_list[[i]]
#   df_n<-thresh_layers[,c(1:2,8)]
#   
#   coordinates(df_n)<-~ x + y
#   gridded(df_n)<-TRUE
#   df_n<- raster(df_n)
#   crs(df_n) <- crs(cdl_data_mi_rec[[1]])
#   #plot(df_n)
#   
#   
#   # focal window of 3x3 pixels, or 7x7 (13)
#   r<-terra::rast(df_n)
#   fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
#     terra::mask(mask = r) 
#   fw<-raster(fw) #convert back to raster object
# 
#   
#   #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
#   
#   # mask out NA areas here using NLCD
#   nlcd<-list_of_nlcd_masks[[i]]
#   ext<-extent(nlcd)
#   fw<-setExtent(fw, ext)
#   fw<-crop(fw, nlcd)
#   fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
#   fw<-mask(fw, nlcd) #mask out by NLCD NA here
#   
#   
#   # this evaluates and removes clumps of pixels (nearest neighbor =  8)
#   rc <- clump(fw, directions = 8) 
#   f<-freq(rc)
#   f<-as.data.frame(f)
#   excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this was visual inspection based)
#   formaskSieve <- rc
#   # assign NA to all clumps whose IDs are found in excludeID
#   formaskSieve[rc %in% excludeID] <- NA
#   fw<-mask(fw, formaskSieve)
#   
#   # convert to a vector
#   fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
#                                         as_points = FALSE, merge = TRUE)) 
#   # fill holes 
#   area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
#   fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
#   
#   county_fw_sets[[i]]<-fw_fill
#   
# }
# 
# #add separate function for buffering and cleaning 
# area_thresh <- units::set_units(40460, m^2) #drop crumbs below 10 acres
# expand_shrink_clean<-function(x){
#   expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
#   shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
#   drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
# }
# 
# #dropped crumbs < 10 acres, did buffer to smooth of 2 acres
# 
# 
# #buffered_layers<-lapply(county_fw_sets,expand_shrink_clean
# 
# michigan_cpaa<-lapply(county_fw_sets, expand_shrink_clean)
# 
# 
# 
# #writeRaster(fw, file.path(cdl_dir, "/fw_bin.tif"), format="GTiff", overwrite = TRUE)
# # writeOGR(county_fw_sets[[1]], cdl_dir,  "/low_raw", driver = "ESRI Shapefile")
# # writeOGR(county_fw_sets[[2]], cdl_dir,  "/low_med", driver = "ESRI Shapefile")
# # writeOGR(county_fw_sets[[3]], cdl_dir,  "/low_high", driver = "ESRI Shapefile")
# # #writeOGR(delineation_f, cdl_dir,  "/delineation_f", driver = "ESRI Shapefile")
# 
# 
# writeOGR(michigan_cpaa[[1]], paste0(cdl_mi_dir,"/CPAA"), "huron75", driver = "ESRI Shapefile")
# writeOGR(michigan_cpaa[[2]], paste0(cdl_mi_dir,"/CPAA"), "oceana25", driver = "ESRI Shapefile")
# writeOGR(michigan_cpaa[[3]], paste0(cdl_mi_dir,"/CPAA"),"vanburen50", driver = "ESRI Shapefile")
# 
# 
# #writeOGR(county_fw_sets[[3]], field_dir,  "high_raw_13", driver = "ESRI Shapefile")
# 
# 
# 
# #### Wisconsin Delineation ----
# county_fw_sets<-list()
# thresh_list<-thresh_list_wi
# list_of_nlcd_masks<-nlcd_wi
# 
# for (i in 1:length(thresh_list)){
#   thresh_layers<-thresh_list[[i]]
#   df_n<-thresh_layers[,c(1:2,8)]
#   
#   coordinates(df_n)<-~ x + y
#   gridded(df_n)<-TRUE
#   df_n<- raster(df_n)
#   crs(df_n) <- crs(cdl_data_wi_rec[[1]])
#   #plot(df_n)
#   
#   
#   # focal window of 3x3 pixels, or 7x7 (13)
#   r<-terra::rast(df_n)
#   fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
#     terra::mask(mask = r) 
#   fw<-raster(fw) #convert back to raster object
# 
#   
#   #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
#   
#   # mask out NA areas here using NLCD
#   nlcd<-list_of_nlcd_masks[[i]]
#   ext<-extent(nlcd)
#   fw<-setExtent(fw, ext)
#   fw<-crop(fw, nlcd)
#   fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
#   fw<-mask(fw, nlcd) #mask out by NLCD NA here
#   
#   
#   # this evaluates and removes clumps of pixels (nearest neighbor =  8)
#   rc <- clump(fw, directions = 8) 
#   f<-freq(rc)
#   f<-as.data.frame(f)
#   excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this was visual inspection based)
#   formaskSieve <- rc
#   # assign NA to all clumps whose IDs are found in excludeID
#   formaskSieve[rc %in% excludeID] <- NA
#   fw<-mask(fw, formaskSieve)
#   
#   # convert to a vector
#   fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
#                                         as_points = FALSE, merge = TRUE)) 
#   # fill holes 
#   area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
#   fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
#   
#   county_fw_sets[[i]]<-fw_fill
#   
# }
# 
# #add separate function for buffering and cleaning 
# area_thresh <- units::set_units(40460, m^2) #drop crumbs below 10 acres
# expand_shrink_clean<-function(x){
#   expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
#   shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
#   drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
# }
# 
# #dropped crumbs < 10 acres, did buffer to smooth of 2 acres
# 
# 
# #buffered_layers<-lapply(county_fw_sets,expand_shrink_clean
# 
# wisconsin_cpaa<-lapply(county_fw_sets, expand_shrink_clean)
# 
# 
# 
# #writeRaster(fw, file.path(cdl_dir, "/fw_bin.tif"), format="GTiff", overwrite = TRUE)
# # writeOGR(county_fw_sets[[1]], cdl_dir,  "/low_raw", driver = "ESRI Shapefile")
# # writeOGR(county_fw_sets[[2]], cdl_dir,  "/low_med", driver = "ESRI Shapefile")
# # writeOGR(county_fw_sets[[3]], cdl_dir,  "/low_high", driver = "ESRI Shapefile")
# # #writeOGR(delineation_f, cdl_dir,  "/delineation_f", driver = "ESRI Shapefile")
# 
# 
# writeOGR(wisconsin_cpaa[[1]], paste0(cdl_wi_dir,"/CPAA"), "lang15", driver = "ESRI Shapefile")
# writeOGR(wisconsin_cpaa[[2]], paste0(cdl_wi_dir,"/CPAA"), "rock65", driver = "ESRI Shapefile")
# writeOGR(wisconsin_cpaa[[3]], paste0(cdl_wi_dir,"/CPAA"),"wau35", driver = "ESRI Shapefile")
# 

#writeOGR(county_fw_sets[[3]], field_dir,  "high_raw_13", driver = "ESRI Shapefile")
}
