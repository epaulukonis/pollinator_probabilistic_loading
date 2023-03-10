### Probabilistic Crop Loading 

### 04 getting sub-delineations

# Edited by E. Paulukonis March 2023

print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/CPAA/bombus"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_cpaa<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/CPAA/bombus"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/CPAA/bombus"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_cpaa<-setNames(lapply(ill_cpaa, readOGR), tools::file_path_sans_ext(basename(ill_cpaa)))
ill_cpaa<-ill_cpaa[order(mixedsort(names(ill_cpaa)))]


#to match appropriate NLCD...
#1999-2002: 2001
#2003-2006: 2006
#2007-2009: 2008
#2010-2012: 2011
#2013-2014: 2013
#2015-2017: 2016
#2018-2021: 2019


cdl_mask_list<-readRDS(paste0(root_data_out,"/cdl_mask_list.RData"))
#here, we mask the cdl for each CPAA output to mask the crops we want to delineate
#because we're matching the nlcd to the appropriate closest years, we structure the code like so
# cdl_mask_list<-list()
# for(i in 2:length(ill_cpaa)){
#   cpaa<-ill_cpaa[[7]]
#   mask_crop<-function(x){
#     output<-crop(x, cpaa)
#     fin_layer<-mask(output, cpaa)
#   }
#   
#   cdl_data<-cdl_data_ill_rec
#   if(i ==1){cdl_data<-cdl_data_ill_rec[1:4]}
#   if(i ==2){cdl_data<-cdl_data_ill_rec[5:8]}
#   if(i ==3){cdl_data<-cdl_data_ill_rec[9:11]}
#   if(i ==4){cdl_data<-cdl_data_ill_rec[12:14]}
#   if(i ==5){cdl_data<-cdl_data_ill_rec[15:16]}
#   if(i ==6){cdl_data<-cdl_data_ill_rec[17:19]}
#   if(i ==7){cdl_data<-cdl_data_ill_rec[20:23]}
# 
#   annual_set<-lapply(cdl_data, mask_crop)
#   
#   
#   cdl_mask_list[[i]]<-stack(annual_set)
# }

#saveRDS(cdl_mask_list, file=paste0(root_data_out,"/cdl_mask_list.RData"))


#function for area and dropping individual polygons 
area_thresh <- units::set_units(22500 , m^2) #5 acres for dropping polygons
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=90, byid=T) # 3 pixel smooth
  shrink<-gBuffer(expand, width=-90, byid=T) #3 pixel smooth
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
}


names(cdl_mask_list)<-c(1:7)

#here, we stack the individual sequence numbers and run the process
for(set in 1:length(cdl_mask_list)){
  cpaa<-ill_cpaa[[set]]
  annual_set<-cdl_mask_list[[set]]
    sub_layer<-stack(annual_set)
    y<-sub_layer
    coords = coordinates(y)
    s1 = as.data.frame(getValues(y))
    layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
    layer.3 <- data.frame(coords, layer.3)
    county_binned<-layer.3
    #county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in x years
    county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
    county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
    county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
    county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years
    #sub_list[[year]]<-county_binned
  
    output<-county_binned
    df_n<-output[,c(1:2,6)] #6 is the sequence values themselves
   # df_n<-na.omit(df_n)
    
    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_ill_rec[[1]])
    
   # terra::writeRaster(df_n, paste0(root_data_out, "/all_tif/ILLINOIS/bombus/test_rawseq_20032006.tif"), filetype = "GTiff", overwrite = TRUE)
    #visualize data first
 
    #here, we'll do a 10 acre window; we omit any cells that are NA; the FW will be a simple square
    fw_s<-df_n 
    r<-terra::rast(fw_s)
    fw_s<- terra::focal(r, w = 7, fun = "modal", na.policy='omit', fillvalue=NA)%>%
      terra::mask(mask = r)
    fw_s<-raster(fw_s) #convert back to raster object
    
   # terra::writeRaster(fw_s, paste0(root_data_out, "/all_tif/ILLINOIS/bombus/test_rawfw_20032006.tif"), filetype = "GTiff", overwrite = TRUE)
    
    #this evaluates clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw_s, directions = 8)
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count < 3)] #remove pixel clumps (visual inspection) #3 pixels = 2 acres
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw_s<-mask(fw_s, formaskSieve)
    
    # convert to a vector
    fw_polys<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw_s),
                                           as_points = FALSE, merge = TRUE))
    # fill holes
    area_threshf <- units::set_units(22500, m^2) #5 acres
    fw_fills<- fill_holes(fw_polys, threshold = area_threshf)
    
    #clean and smooth edges
    output_of_cleaning<-expand_shrink_clean(fw_fills)
    
    #finally, clip the output by the original CPAA layer
    area_thresh <- units::set_units(8100 , m^2) #2 acres for dropping polygons
    cropped<-st_intersection(sf::st_as_sf(output_of_cleaning),sf::st_as_sf(cpaa)) #intersect the CPAA and the sub delineation
    poly<-st_collection_extract(cropped, "POLYGON")
    final_layer<-drop_crumbs(poly, area_thresh, drop_empty = TRUE) #drop crumbs
    
    setwd(paste0(root_data_out, "/all_tif/ILLINOIS/SUB/bombus"))
    # st_write(final_layer,paste0("bombus_fields_fin.shp"))
    
    st_write(final_layer,paste0("fields_",names(cdl_mask_list[set]),"_fin.shp"))
   
}




#but before you do...first you should stack the data 
#I think I should stick with the 2019 NLCD here; this is different than what was done for the field manuscript
#there, we were testing a bunch of different thresholds and using the closest NLCD to that set of years 
#here, I think I need to stick to that, even if the NLCD for 2019 might not perfectly match 1999
#This was based on the median crop frequency pixel, which was heavily skewed towards 23 years 
