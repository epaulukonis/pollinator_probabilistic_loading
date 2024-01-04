#need to read in basic fields, and then read in CDL, and re-run field analysis but absent of sequence


print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_cpaa<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/CPAA"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_cpaa<-setNames(lapply(ill_cpaa, readOGR), tools::file_path_sans_ext(basename(ill_cpaa)))
ill_cpaa<-ill_cpaa[order(mixedsort(names(ill_cpaa)))]

cpaa_list_ill_f<-list()
cpaa_list_ill_f[[1]]<-ill_cpaa[1:2]
cpaa_list_ill_f[[2]]<-ill_cpaa[4:5]
cpaa_list_ill_f[[3]]<-ill_cpaa[6:7]





#lines 3-12 need fixing to reflect nested layers, but all else was designed for year by year CDL extraction to sub fields
study_area_cdl<-list()
for (co in 1:length(cpaa_list_ill_f)){
  co_r<-cpaa_list_ill_f[[co]]
  mask_crop<-function(x){
    output<-crop(x, co_r)
    fin_layer<-mask(output, co_r)
  }
  county_set<-lapply(cdl_data_ill_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
  study_area_cdl[[co]]<-stack(county_set) #represents single county stack
}
names(study_area_cdl)<-names_of_counties



#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to 0 (or 9 if needed to count)
is_m <- c(0,1,2:4,5,6:23,24,25:256)
becomes <- c(NA,1,rep.int(0,3),2,rep.int(0,18),3,rep.int(0,232))
m<-cbind(is_m,becomes)

county_rec_list<-list()
layer_list<-list()
for (county in 1:length(cdl_mask_list)){
  county_r<-cdl_mask_list[[county]]
  for(layer in 1:length(county_r)){
    layer_list[[layer]] <- reclassify(county_r[[layer]], m)
  }
  county_rec_list[[county]]<-layer_list
}

names(county_rec_list)<-c("Champaign","DuPage","McHenry")


####Stacking and getting sequence values within the fields----
sub_by_county_list<-list()
for(county_layer in 1:length(county_rec_list)){
  county_layer=1
  county<-county_rec_list[[county_layer]]
  
  sub_list<-list()
  for (i in 1:length(county)){
    sub_layer<-county[[i]]
    y<-sub_layer
    s0 = brick(y)
    coords = coordinates(s0) 
    s1 = as.data.frame(getValues(s0))
    layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
    layer.3 <- data.frame(coords, layer.3)
    county_binned<-layer.3
    county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 14 years
    county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
    county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
    #county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
    #county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years
    
    sub_list[[i]]<-county_binned
    
  }
  sub_by_county_list[[county_layer]]<-sub_list
}



####Sub-delineation ----
sub_fw_sets<-list()
sub_fw_by_county<-list()
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=30, byid=T) # 1 pixel smooth
  shrink<-gBuffer(expand, width=-30, byid=T) #1 pixel smooth
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE)
}

area_thresh <- units::set_units(44100 , m^2) #10 acres for dropping polygons

for (county in 1:length(sub_by_county_list)){
  county=1
  sub_list<-sub_by_county_list[[county]]
  
  for (year in 1:length(sub_list)){
    year=1
    output<-sub_list[[year]]
    df_n<-output[,c(1:2,5)] #5 is the field values themselves
    df_n<-na.omit(df_n)
    
    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_ill_rec[[1]])
    
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
    excludeID <- f$value[which(f$count <= 7)] #remove pixel clumps (visual inspection)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw_s<-mask(fw_s, formaskSieve)
    
    # convert to a vector
    fw_polys<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw_s), 
                                           as_points = FALSE, merge = TRUE)) 
    # fill holes 
    area_thresh <- units::set_units(8100, m^2) #2 acres
    fw_fills<- fill_holes(fw_polys, threshold = area_thresh)
    
    sub_fw_sets[[county]]<-fw_fills
    
  }
  
  output_of_cleaning<-lapply(sub_fw_sets, expand_shrink_clean)
  sub_fw_by_county[[county]]<-sub_fw_sets
  
}
