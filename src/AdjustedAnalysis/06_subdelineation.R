### Probabilistic Crop Loading 

### 06 Sub-delineation of fields

# Edited by E. Paulukonis June 2022

#### Prepare the CDL layers and the 3 county CPAA layers----
#50 8% DUPAGE low
#1 40% McHENRY medium
#14 72% CHAMPAIGN high

names_of_counties<-c("DUPAGE", "MCHENRY", "CHAMPAIGN")


#function to mask and crop CDL to the three CPAAs
county_set_list<-list()

for (co in 1:length(output_of_cleaning)){
 co_r<-output_of_cleaning[[co]]
  mask_crop<-function(x){
    output<-crop(x, co_r)
    fin_layer<-mask(output, co_r)
  }
  county_set<-lapply(cdl_data_ill_rec[c(10:23)], mask_crop) #crop and mask the fixed CDL to the counties, put in list
  county_set_list[[co]]<-stack(county_set) #represents single county stack
}
names(county_set_list)<-names_of_counties


#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to 0 (or 9 if needed to count)
is_m <- c(0,1,2:4,5,6:23,24,25:256)
becomes <- c(NA,1,rep.int(0,3),2,rep.int(0,18),3,rep.int(0,232))
m<-cbind(is_m,becomes)

county_rec_list<-list()
layer_list<-list()
for (county in 1:length(county_set_list)){
  county_r<-county_set_list[[county]]
  for(layer in 1:nlayers(county_r)){
    layer_list[[layer]] <- reclassify(county_r[[layer]], m)
  }
  county_rec_list[[county]]<-layer_list
}



####Stacking and getting sequence values within the fields----
sub_list<-list()

for(layer in 1:length(county_rec_list)){
  county<-county_rec_list[[layer]]
  y<-county
  s0 = brick(y)
  coords = coordinates(s0) 
  s1 = as.data.frame(getValues(s0))
  layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
  layer.3 <- data.frame(coords, layer.3)
  
  county_binned<-layer.3
  county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 14 years
  county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
  county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
  county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
  county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years

  sub_list[[layer]]<-county_binned
  
}



####Sub-delineation ----
sub_fw_sets<-list()
for (county in 1:length(sub_list)){
  output<-sub_list[[county]]
  output<-output[output$bin >= 14,]
  df_n<-output[,c(1:2,6)] #6 is individual values, 7 is the #years cropped (14, 13, 12, 11, 10, 9...)
  df_n<-na.omit(df_n)

  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_ill_rec[[1]])
  
  #crop to the delineation polygons
  fw_s<-df_n
  # fw_s<-setExtent(fw_s, output)
  # fw_s<-crop(fw_s, output)
  # #fw_s<-projectRaster(fw_s, output, method='ngb',crs(f=output))
  #fw_s<-mask(fw_s, output) #mask out by first delineation
  
#start with 7, then do 3; 7 is 10 acres  
#focal window: 3 pixels (why? because 8100 m2 = 2 acres, lowest fw possible that allowed for small plots within the function
  r<-terra::rast(fw_s)
  fw_s<- terra::focal(r, w = 7, fun = "modal", na.policy='omit', fillvalue=NA)%>% 
    terra::mask(mask = r) 
  fw_s<-raster(fw_s) #convert back to raster object
  #plot(fw_s)
  
  #this evaluates clumps of pixels (nearest neighbor =  8)
  rc <- clump(fw_s, directions = 8) 
  f<-freq(rc)
  f<-as.data.frame(f)
  excludeID <- f$value[which(f$count <= 7)] #remove pixel clumps (visual inspection)
  formaskSieve <- rc
  # assign NA to all clumps whose IDs are found in excludeID
  formaskSieve[rc %in% excludeID] <- NA
  fw_s<-mask(fw_s, formaskSieve)
  
  
  # convert to a vector
  fw_polys<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw_s), 
                                         as_points = FALSE, merge = TRUE)) 
  # fill holes 
  area_thresh <- units::set_units(40468.6, m^2) #10 acres again
  fw_fills<- fill_holes(fw_polys, threshold = area_thresh)
  
  sub_fw_sets[[county]]<-fw_fills
  
}


#buffered_layers<-lapply(county_fw_sets,expand_shrink_clean
area_thresh <- units::set_units(40468.6 , m^2) #10 acres here; we've already eliminated most single pixels (remaining ones are due to FW on edges)
output_of_cleaning<-lapply(sub_fw_sets, expand_shrink_clean)

#dropped crumbs =< 1 pixel (900m2), i.e. 2 acres


#writeRaster(fw_s, file.path(cdl_dir, "/fw_6_test.tif"), format="GTiff", overwrite = TRUE)
#writeOGR(output_of_cleaning[[3]], cdl_dir,  "output_sd_high_newn", driver = "ESRI Shapefile")
#we'll use the new vectors to look at focal window of sequences in each field above a certain size

writeOGR(output_of_cleaning[[1]], field_dir,  "low_sd_77", driver = "ESRI Shapefile")
writeOGR(output_of_cleaning[[2]], field_dir,  "med_sd_77", driver = "ESRI Shapefile")
writeOGR(output_of_cleaning[[3]], field_dir,  "high_sd_77", driver = "ESRI Shapefile")


sub_del<- readOGR(cdl_dir, layer = "_output_sd_med")


