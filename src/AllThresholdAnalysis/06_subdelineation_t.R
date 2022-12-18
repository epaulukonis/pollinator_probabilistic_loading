### Probabilistic Crop Loading 

### 06 Sub-delineation of fields

# Edited by E. Paulukonis October'22

#### Prepare the CDL layers and the 3 county CPAA layers----


########## NEEDS TO BE CORRECTED FOR NAMING!!! IT STILL WORKS BUT NAMES ARE WRONG!! ######
#### Illinois ----
# #function to mask and crop CDL to the three CPAAs #figure out how to properly stack/list the cdl by the individual thresholds
# cdl_mask_list<-list()
# year_mask_list<-list()
# for (county_layer in 1:length(cpaa_list_ill_f)){
#   cpaa<-cpaa_list_ill_f[[county_layer]]
#   for (year in 1:length(cpaa)){
#     cpaa_y<-cpaa[[year]]
#     mask_crop<-function(x){
#       output<-crop(x, cpaa_y)
#       fin_layer<-mask(output, cpaa_y)
#     }
#     county_set<-lapply(cdl_data_ill_rec, mask_crop)
#    year_mask_list[[year]]<-stack(county_set)
#   }
# 
# 
#   cdl_mask_list[[county_layer]]<-year_mask_list
# }
# 
# #levels/relevel
# 
# 
# #writeRaster(year_mask_list[[1]][[1]], file.path(root_data_out, "/raw_mask_test.tif"), format="GTiff", overwrite = TRUE)
# 
# # is_m <- c(0,1,2:4,5,6:23,24,25:256)
# # becomes <- c(NA,1,rep.int(0,3),2,rep.int(0,18),3,rep.int(0,232))
# # m<-cbind(is_m,becomes)
# # 
# # county_rec_list<-list()
# # layer_list<-list()
# # for (county in 1:length(cdl_mask_list)){
# #   county_r<-cdl_mask_list[[county]]
# #   for(layer in 1:length(county_r)){
# #     layer_list[[layer]] <- reclassify(county_r[[layer]], m)
# #   }
# #   county_rec_list[[county]]<-layer_list
# # }
# # 
# # county_rec_list<-list()
# # layer_list<-list()
# # for (county in 1:length(cdl_mask_list)){
# #   county_r<-cdl_mask_list[[county]]
# #   for(layer in 1:length(county_r)){
# #     layer_list[[layer]] <- reclassify(county_r[[layer]], m)
# #   }
# #   county_rec_list[[county]]<-layer_list
# # }
# # 
# # 
# 
# county_rec_list<-cdl_mask_list
# names(county_rec_list)<-c("Champaign","DuPage","McHenry")
# 
# 
# ####Stacking and getting sequence values within the fields
# sub_by_county_list<-list()
# sub_list<-list()
# for(county_layer in 1:length(county_rec_list)){
#  county<-county_rec_list[[county_layer]]
#  #plot(county[[1]][[1]])
#  for (i in 1:length(county)){
#   sub_layer<-county[[i]]
#   y<-sub_layer
#   coords = coordinates(y)
#   s1 = as.data.frame(getValues(y))
#   layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
#   layer.3 <- data.frame(coords, layer.3)
#   county_binned<-layer.3
#   county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 14 years
#   county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
#   county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
#   county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
#   county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years
# 
#   sub_list[[i]]<-county_binned
# 
#  }
#  sub_by_county_list[[county_layer]]<-sub_list
# }
# 
# names(sub_by_county_list)<-c("Champaign","DuPage","McHenry")
# 
# ####Sub-delineation
# 
# expand_shrink_clean<-function(x){
#   expand<-gBuffer(x, width=90, byid=T) # 3 pixel smooth
#   shrink<-gBuffer(expand, width=-90, byid=T) #3 pixel smooth
#   drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
# }
# 
# area_thresh <- units::set_units(44100 , m^2) #10 acres for dropping polygons
# 
# sub_fw_sets<-list()
# sub_fw_by_county<-list()
# for (county in 1:length(sub_by_county_list)){
#   sub_list<-sub_by_county_list[[county]]
# 
#   for (year in 1:length(sub_list)){
#   output<-sub_list[[year]]
#   df_n<-output[,c(1:2,5)] #5 is the field values themselves
#   df_n<-na.omit(df_n)
# 
#   coordinates(df_n)<-~ x + y
#   gridded(df_n)<-TRUE
#   df_n<- raster(df_n)
#   crs(df_n) <- crs(cdl_data_ill_rec[[1]])
# 
#   #crop to the delineation polygons
#   fw_s<-df_n
#   # fw_s<-setExtent(fw_s, output)
#   # fw_s<-crop(fw_s, output)
#   # fw_s<-projectRaster(fw_s, output, method='ngb',crs(f=output))
#   # fw_s<-mask(fw_s, output) #mask out by first delineation
# 
#   ##12/5/22 note: for b. affinsi work, consider using 3x3 window (or 1x1 smallest possible)
#   #start with 7, then do 3; 7 is 10 acres
#   #focal window: 3 pixels (why? because 8100 m2 = 2 acres, lowest fw possible that allowed for small plots within the function
#   #focal window: 10 pixels (why? bc that's the minimum average field size from the LACIE paper)
#   r<-terra::rast(fw_s)
#   fw_s<- terra::focal(r, w = 7, fun = "modal", na.policy='omit', fillvalue=NA)%>%
#     terra::mask(mask = r) 
#   fw_s<-raster(fw_s) #convert back to raster object
#   #plot(fw_s)
# 
# 
#   #this evaluates clumps of pixels (nearest neighbor =  8)
#   rc <- clump(fw_s, directions = 8)
#   f<-freq(rc)
#   f<-as.data.frame(f)
#   excludeID <- f$value[which(f$count <= 7)] #remove pixel clumps (visual inspection)
#   formaskSieve <- rc
#   # assign NA to all clumps whose IDs are found in excludeID
#   formaskSieve[rc %in% excludeID] <- NA
#   fw_s<-mask(fw_s, formaskSieve)
# 
#   # convert to a vector
#   fw_polys<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw_s),
#                                          as_points = FALSE, merge = TRUE))
#   # fill holes
#   area_thresh <- units::set_units(44100, m^2) #10 acres
#   fw_fills<- fill_holes(fw_polys, threshold = area_thresh)
# 
#   sub_fw_sets[[year]]<-fw_fills
# 
#   }
# 
#   output_of_cleaning<-lapply(sub_fw_sets, expand_shrink_clean)
#   sub_fw_by_county[[county]]<-output_of_cleaning
#   sub_fw_by_county[[county]]<-sub_fw_sets
# }
# 
# 
# names(sub_fw_by_county)<-c("Champaign","DuPage","McHenry")
# #finally, clip the output by the original CPAA layer
# area_thresh <- units::set_units(8100 , m^2) #2 acres for dropping polygons
# for(cpaal in 1:length(cpaa_list_ill_f)){
#   cpaa<-cpaa_list_ill_f[[cpaal]]
#   output<-sub_fw_by_county[[cpaal]]
# 
#   for(i in 1:length(output)){
#     cropped<-st_intersection(sf::st_as_sf(output[[i]]),sf::st_as_sf(cpaa[[i]])) #let's get the areas in Illinois where they have high probability of distribution
#     poly<-st_collection_extract(cropped, "POLYGON")
#     final_layer<-drop_crumbs(poly, area_thresh, drop_empty = TRUE)
#     setwd(paste0(root_data_out, "/all_tif/ILLINOIS/SUB"))
#     st_write(final_layer,paste0(names(sub_fw_by_county[cpaal]),i,"_fin.shp"))
#   }
# 
# }
# 
# # for each in some list 


#### Michigan  ----
#function to mask and crop CDL to the three CPAAs #figure out how to properly stack/list the cdl by the individual thresholds
cdl_mask_list<-list()
year_mask_list<-list()
for (county_layer in 1:length(cpaa_list_mi_f)){
  cpaa<-cpaa_list_mi_f[[county_layer]]
  for (year in 1:length(cpaa)){
    cpaa_y<-cpaa[[year]]
    mask_crop<-function(x){
      output<-crop(x, cpaa_y)
      fin_layer<-mask(output, cpaa_y)
    }
    county_set<-lapply(cdl_data_mi_rec, mask_crop)
    year_mask_list[[year]]<-stack(county_set)
  }
  
  cdl_mask_list[[county_layer]]<-year_mask_list
}


county_rec_list<-cdl_mask_list
names(county_rec_list)<-c("Huron", "Oceana", "VanBuren")


####Stacking and getting sequence values within the fields
sub_by_county_list<-list()
for(county_layer in 1:length(county_rec_list)){
  county<-county_rec_list[[county_layer]]
  
  sub_list<-list()
  for (i in 1:length(county)){
    sub_layer<-county[[i]]
    y<-sub_layer
    coords = coordinates(y)
    s1 = as.data.frame(getValues(y))
    layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
    layer.3 <- data.frame(coords, layer.3)
    county_binned<-layer.3
    county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 14 years
    county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
    county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
    county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
    county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years
    
    sub_list[[i]]<-county_binned
    
  }
  sub_by_county_list[[county_layer]]<-sub_list
}

names(sub_by_county_list)<-c("Huron", "Oceana", "VanBuren")

####Sub-delineation
sub_fw_sets<-list()
sub_fw_by_county<-list()
area_thresh <- units::set_units(8100 , m^2) #2 acres for dropping polygons
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=90, byid=T) # 3 pixel smooth
  shrink<-gBuffer(expand, width=-90, byid=T) #3 pixel smooth
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
}

for (county in 1:length(sub_by_county_list)){
  sub_list<-sub_by_county_list[[county]]
  
  for (year in 1:length(sub_list)){
    output<-sub_list[[year]]
    df_n<-output[,c(1:2,6)] #6 is the field values themselves
    df_n<-na.omit(df_n)
    
    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_mi_rec[[1]])
    
    #crop to the delineation polygons
    fw_s<-df_n
    # fw_s<-setExtent(fw_s, output)
    # fw_s<-crop(fw_s, output)
    # fw_s<-projectRaster(fw_s, output, method='ngb',crs(f=output))
    # fw_s<-mask(fw_s, output) #mask out by first delineation
    
    #start with 7, then do 3; 7 is 10 acres
    #focal window: 3 pixels (why? because 8100 m2 = 2 acres, lowest fw possible that allowed for small plots within the function
    #focal window: 10 pixels (why? bc that's the minimum average field size from the LACIE paper)
    r<-terra::rast(fw_s)
    fw_s<- terra::focal(r, w = 7, fun = "modal", na.policy='omit', fillvalue=NA)%>%
      terra::mask(mask = r)
    fw_s<-raster(fw_s) #convert back to raster object
    #plot(fw_s)
    
    
    #this evaluates clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw_s, directions = 8)
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count <= 3)] #remove pixel clumps (visual inspection)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw_s<-mask(fw_s, formaskSieve)
    
    # convert to a vector
    fw_polys<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw_s),
                                           as_points = FALSE, merge = TRUE))
    # fill holes
    area_threshf <- units::set_units(22050, m^2) #5 acres
    fw_fills<- fill_holes(fw_polys, threshold = area_threshf)
    
    sub_fw_sets[[year]]<-fw_fills
    
  }
  
  output_of_cleaning<-lapply(sub_fw_sets, expand_shrink_clean)
  sub_fw_by_county[[county]]<-output_of_cleaning
  
}


names(sub_fw_by_county)<-c("Huron", "Oceana", "VanBuren")
#finally, clip the output by the original CPAA layer
area_thresh <- units::set_units(8100 , m^2) #2 acres for dropping polygons
for(cpaal in 1:length(cpaa_list_mi_f)){
  cpaa<-cpaa_list_mi_f[[cpaal]]
  output<-sub_fw_by_county[[cpaal]]
  for(i in 1:length(output)){
    cropped<-st_intersection(sf::st_as_sf(output[[i]]),sf::st_as_sf(cpaa[[i]])) #intersect the CPAA and the sub delineation
    poly<-st_collection_extract(cropped, "POLYGON")
    final_layer<-drop_crumbs(poly, area_thresh, drop_empty = TRUE)
    setwd(paste0(root_data_out, "/all_tif/MICHIGAN/SUB"))
    st_write(final_layer,paste0(names(sub_fw_by_county[cpaal]),i,"_fin.shp"))
  }
  
}





#### Wisconsin ----
#function to mask and crop CDL to the three CPAAs #figure out how to properly stack/list the cdl by the individual thresholds
cdl_mask_list<-list()
year_mask_list<-list()
for (county_layer in 1:length(cpaa_list_wi_f)){
  cpaa<-cpaa_list_wi_f[[county_layer]]
  for (year in 1:length(cpaa)){
    cpaa_y<-cpaa[[year]]
    mask_crop<-function(x){
      output<-crop(x, cpaa_y)
      fin_layer<-mask(output, cpaa_y)
    }
    county_set<-lapply(cdl_data_wi_rec, mask_crop)
    year_mask_list[[year]]<-stack(county_set)
  }
  
  
  cdl_mask_list[[county_layer]]<-year_mask_list
}


county_rec_list<-cdl_mask_list
names(county_rec_list)<-c("Langlade","Rock","Waushara")


####Stacking and getting sequence values within the fields
sub_by_county_list<-list()
for(county_layer in 1:length(county_rec_list)){
  county<-county_rec_list[[county_layer]]
  
  sub_list<-list()
  for (i in 1:length(county)){
    sub_layer<-county[[i]]
    y<-sub_layer
    coords = coordinates(y)
    s1 = as.data.frame(getValues(y))
    layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
    layer.3 <- data.frame(coords, layer.3)
    county_binned<-layer.3
    county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 14 years
    county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
    county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
    county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
    county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years
    
    sub_list[[i]]<-county_binned
    
  }
  sub_by_county_list[[county_layer]]<-sub_list
}

names(sub_by_county_list)<-c("Langlade","Rock","Waushara")

####Sub-delineation
sub_fw_sets<-list()
sub_fw_by_county<-list()

area_thresh <- units::set_units(8100 , m^2) #2 acres for dropping polygons
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=90, byid=T) # 3 pixel smooth
  shrink<-gBuffer(expand, width=-90, byid=T) #3 pixel smooth
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
}


for (county in 1:length(sub_by_county_list)){
  sub_list<-sub_by_county_list[[county]]
  
  for (year in 1:length(sub_list)){
    output<-sub_list[[year]]
    df_n<-output[,c(1:2,6)] #6 is the field values themselves
    df_n<-na.omit(df_n)
    
    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_mi_rec[[1]])
    
    #crop to the delineation polygons
    fw_s<-df_n
    # fw_s<-setExtent(fw_s, output)
    # fw_s<-crop(fw_s, output)
    # fw_s<-projectRaster(fw_s, output, method='ngb',crs(f=output))
    # fw_s<-mask(fw_s, output) #mask out by first delineation
    
    #we'll do 7 here, which is 10 acres. hopefully the individual patterns are still reflected here.
    #we could also do 3 here because that's 2 acres, and most acreages in MI/WI are tiny
    r<-terra::rast(fw_s)
    fw_s<- terra::focal(r, w = 7, fun = "modal", na.policy='omit', fillvalue=NA)%>%
      terra::mask(mask = r)
    fw_s<-raster(fw_s) #convert back to raster object
    #plot(fw_s)
    
    
    #this evaluates clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw_s, directions = 8)
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count <= 3)] #remove pixel clumps < 3 (visual inspection)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw_s<-mask(fw_s, formaskSieve)
    
    # convert to a vector
    fw_polys<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw_s),
                                           as_points = FALSE, merge = TRUE))
    # fill holes
    area_threshf <- units::set_units(22050, m^2) #5 acres
    fw_fills<- fill_holes(fw_polys, threshold = area_threshf)
    
    sub_fw_sets[[year]]<-fw_fills
    
  }
  
  output_of_cleaning<-lapply(sub_fw_sets, expand_shrink_clean)
  sub_fw_by_county[[county]]<-output_of_cleaning
  
}


names(sub_fw_by_county)<-c("Langlade","Rock","Waushara")
#finally, clip the output by the original CPAA layer
area_thresh <- units::set_units(8100 , m^2) #2 acres for dropping polygons
for(cpaal in 1:length(cpaa_list_wi_f)){
  cpaa<-cpaa_list_wi_f[[cpaal]]
  output<-sub_fw_by_county[[cpaal]]
  for(i in 1:length(output)){
    cropped<-st_intersection(sf::st_as_sf(output[[i]]),sf::st_as_sf(cpaa[[i]])) #intersect the CPAA and the sub delineation
    poly<-st_collection_extract(cropped, "POLYGON")
    final_layer<-drop_crumbs(poly, area_thresh, drop_empty = TRUE)
    setwd(paste0(root_data_out, "/all_tif/WISCONSIN/SUB"))
    st_write(final_layer,paste0(names(sub_fw_by_county[cpaal]),i,"_fin.shp"))
  }
  
}
