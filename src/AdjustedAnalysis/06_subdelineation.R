### Probabilistic Crop Loading 

### 06 Sub-delineation of fields

# Edited by E. Paulukonis March 2022



####Sub-delineation ----
sub_fw_sets<-list()
for (i in 1:length(dataframe_list)){
  
  df_n<-thresh_layers[,c(1:2,6)] 
  
  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_ill_rec[[1]])
  
  #crop to the delineation polygons
  fw_s<-df_n
  fw_s<-setExtent(fw_s, output)
  fw_s<-crop(fw_s, output)
  #fw_s<-projectRaster(fw_s, output, method='ngb',crs(f=output))
  fw_s<-mask(fw_s, output) #mask out by first delineation
  
  
  r<-terra::rast(fw_s)
  fw_s<- terra::focal(r, w = 3, fun = "modal", na.policy='omit', fillvalue=NA)%>% 
    terra::mask(mask = r) 
  fw_s<-raster(fw_s) #convert back to raster object
  
  
  #this evaluates clumps of pixels (nearest neighbor =  8)
  rc <- clump(fw_s, directions = 8) 
  f<-freq(rc)
  f<-as.data.frame(f)
  excludeID <- f$value[which(f$count <= 7)] #remove pixel clump of 7 or fewer
  formaskSieve <- rc
  # assign NA to all clumps whose IDs are found in excludeID
  formaskSieve[rc %in% excludeID] <- NA
  fw_s<-mask(fw_s, formaskSieve)
  
  
  # convert to a vector
  fw_polys<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw_s), 
                                         as_points = FALSE, merge = TRUE)) 
  # fill holes 
  area_thresh <- units::set_units(2, km^2)
  fw_fills<- fill_holes(fw_polys, threshold = area_thresh)
  
  county_fw_sets[[i]]<-fw_fills
  
}


#buffered_layers<-lapply(county_fw_sets,expand_shrink_clean

output_sd<-expand_shrink_clean(fw_fills)


writeRaster(fw_sd, file.path(cdl_dir, "/fw_sd_test.tif"), format="GTiff", overwrite = TRUE)
writeOGR(fw_fills, cdl_dir,  "/test_sd_function", driver = "ESRI Shapefile")


#we'll use the new vectors to look at focal window of sequences in each field above a certain size




####Second-pass Analysis----
field_areass<- as.data.frame(area(fw_droppeds), na.rm=T) #get areas of each field from the vectorized field
colnames(field_areass)[1]<-'area'
#field_areas$area<-field_areas

#for each layer in y (sets of 11 years for each county), extract the values to the created polygons
crop_list<-list()
for(layer in 1:length(y)){
  output<-exact_extract(y[[layer]],fw_droppeds, "mode")
  output<-as.data.frame(output)
  names(output)<-"crops"
  output$area<-field_areass$area
  output<- output %>% group_by(crops) %>%   summarise(area = sum(area)*0.000247105)
  crop_list[[layer]]<-output
}

names(crop_list)<-1999:2009

outliers<-Filter(function(x) nrow(x) <5, crop_list)
main<-Filter(function(x) nrow(x) >=5, crop_list)
field1<-do.call(cbind, lapply(outliers, as.data.frame))
field2<-do.call(cbind, lapply(main, as.data.frame))
field1[5,]<-filler
field1<-field1[c(1:3,5,4),]

value<-field1[,1]
field_crop_areass<-cbind(field1,field2)
field_crop_areass =  field_crop_areass %>% select(-contains(".crops"))
field_crop_areass$value<-value
field_crop_areass<-field_crop_areass[ ,order(names(field_crop_areass))]

field_crop_areass<-field_crop_areass[,c(12,1:11)]
names(field_crop_areass)<-c("crop",1999:2009) #make sure names align

error<-((field_crop_areass[,c(2:12)] - cdl_crop_areas[,c(2:12)])/cdl_crop_areas[,c(2:12)])*100
error$crop<-c("other","corn","soy","ww","NA")
print(error)

colSums(field_crop_areass[,c(2:12)])
colSums(cdl_crop_areas[,c(2:12)])



####Other metrics----
###Crop vs. Non-crop
cdl_crop<-colSums(cdl_crop_areas[1:2,1:11])
cdl_noncrop<-cdl_crop_areas[3,1:11]

field_crop<-colSums(field_crop_areas[1:2,1:11])
field_noncrop<-field_crop_areas[3,1:11]

crop_diff<-((field_crop - cdl_crop)/cdl_crop)*100 
print(crop_diff) #

