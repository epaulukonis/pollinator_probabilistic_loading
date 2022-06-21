### Probabilistic Crop Loading 

### 04 Delineation of Fields

# Edited by E. Paulukonis June 2022

import_start_time <- Sys.time()
print("stepping into 04_sub_delineation.R")
options(scipen = 999) #remove exponent options, throws R off


####Delineation ----
county_fw_sets<-list()
  for (i in 1:length(thresh_list)){
  
  thresh_layers<-thresh_list[[i]]
  df_n<-thresh_layers[,c(1:2,8)]

    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_ill_rec[[1]])
    #plot(df_n)


    # focal window of 3x3 pixels, or 7x7
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 3, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
      terra::mask(mask = r) 
    fw<-raster(fw) #convert back to raster object

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
    excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this is roughly the same 3x3 window)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw<-mask(fw, formaskSieve)
  
    # convert to a vector
    fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                          as_points = FALSE, merge = TRUE)) 
    # fill holes 
    area_thresh <- units::set_units(3, km^2) #2 km was decided after visually inspecting each county
    fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
    
    county_fw_sets[[i]]<-fw_fill

  }


#add separate function for buffering and cleaning 
area_thresh <- units::set_units(8100, m^2) #drop crumbs below ~ 9 pixels 
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=90, byid=T) # 3 pixel expand to smooth
  shrink<-gBuffer(expand, width=-90, byid=T) #3 pixel shrink to smooth
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
}

#dropped crumbs < 2 acres


#buffered_layers<-lapply(county_fw_sets,expand_shrink_clean

output_of_cleaning<-lapply(county_fw_sets, expand_shrink_clean)


#writeRaster(fw, file.path(cdl_dir, "/fw_bin.tif"), format="GTiff", overwrite = TRUE)
# writeOGR(county_fw_sets[[1]], cdl_dir,  "/low_raw", driver = "ESRI Shapefile")
# writeOGR(county_fw_sets[[2]], cdl_dir,  "/low_med", driver = "ESRI Shapefile")
# writeOGR(county_fw_sets[[3]], cdl_dir,  "/low_high", driver = "ESRI Shapefile")
# #writeOGR(delineation_f, cdl_dir,  "/delineation_f", driver = "ESRI Shapefile")


writeOGR(output_of_cleaning[[1]], cdl_dir,  "low_clean1021", driver = "ESRI Shapefile")
writeOGR(output_of_cleaning[[2]], cdl_dir,  "med_clean1021", driver = "ESRI Shapefile")
writeOGR(output_of_cleaning[[3]], cdl_dir,  "high_clean1021", driver = "ESRI Shapefile")



