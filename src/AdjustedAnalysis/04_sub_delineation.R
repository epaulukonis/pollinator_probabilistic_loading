### Probabilistic Crop Loading 

### 04 Sub-delineation of fields

# Edited by E. Paulukonis March 2022

import_start_time <- Sys.time()
print("stepping into 04_sub_delineation.R")

options(scipen = 999) #remove exponent options, throws R off


#### Reclassify ----
#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to 0 (or 9 if needed to count)
is_m <- c(0,1,2:4,5,6:23,24,25:256)
becomes <- c(NA,1,rep.int(0,3),2,rep.int(0,18),3,rep.int(0,232))
m<-cbind(is_m,becomes)

county_list<-list()
layer_list<-list()
for (county in 1:length(county_set_list)){
  county_r<-county_set_list[[county]]
  for(layer in 1:nlayers(county_r)){
    layer_list[[layer]] <- reclassify(county_r[[layer]], m)
  }
  county_list[[county]]<-layer_list
}


####Dataframe compilation ----

##in this section: we take the county_list and turn it into a dataframe, and compile the sequence columns
dataframe_list<-list()
for(county in 1:length(county_list)){
i=1

#convert to dataframe
y<-county_list[[i]] ##first county
s0 = brick(y) #brick the raster
coords = coordinates(s0) #get the coordinates
s1 = as.data.frame(getValues(s0)) #get the dataframe
rec_layer = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = '')) #make it nicer looking
rec_layer <- data.frame(coords, rec_layer) #finish as final organized frame

#let's format the sequence data
rec_f_layer<-rec_layer
any_layer <- rec_f_layer[!grepl("NANANANANANANANANANANA", rec_f_layer$rec_layer),] # remove pixels that have no crops in 11 years
any_layer$f<-gsub("NA", "", any_layer$rec_layer) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
any_layer$field<-as.numeric(any_layer$f) #turn f into fields as numeric integer
any_layer$n_years<-as.numeric(gsub(0, "", any_layer$field)) #sub 0s with empty slots
any_layer$bin<-floor(log10(any_layer$n_years)) + 1  #create column to count the number of years


thresh_layers<-any_layer[any_layer$bin >= 6,] #for now
#thresh_layers<-any_layer[tany_layer$bin >= (as.numeric(thresh$Var1) - 1),] 

thresh_layers<-na.omit(thresh_layers)
dataframe_list[[county]]<-thresh_layers
#binned_prop<-split(thresh_layers, f=thresh_layers$bin) #split if needed

}

####Delinearion ----
##Just raw sequences; creates big swaths of fields (this is all sequences)
df_n<-thresh_layers[,c(1:2,6)] 

##try proportions
thresh_layers$prop<-thresh_layers$bin/11
df_n<-thresh_layers[,c(1:2,8)] #prop

###get field delineations for single years and see if it matters when you clump it this way
testy<-binned_prop[[6]] 
testy$sing<-1
df_n<-testy[,c(1:2,6)] #it doesn't, we can bin it this way



####Once you decide, run through a function that 
##seems like sets of threshold years is best
county_fw_sets<-list()
  for (i in 1:length(dataframe_list)){
    
  thresh_layers$prop<-thresh_layers$bin/11
  df_n<-thresh_layers[,c(1:2,8)]

    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_ill_rec[[1]])
    
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 7, fun = "modal", na.policy='all')%>% 
      terra::mask(mask = r) 
    fw<-raster(fw) #convert back to raster object

    #mask out NA areas here using NLCD
    fw<-setExtent(fw, ext)
    fw<-crop(fw, nlcd)
    fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
    fw<-mask(fw, nlcd) #mask out by NLCD NA here
    
    #this evaluates clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw, directions = 8) 
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count <= 7)] #remove pixel clump of 7 or fewer
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw<-mask(fw, formaskSieve)
    

    #additional 'smoothing'
    # agg_fw <- terra::aggregate(fw, fact = 3, fun = modal, na.rm = TRUE)
    # fw_f<- disaggregate(agg_fw, 3) #bilinear resamples 
    # 
    county_fw_sets[[i]]<-fw

  }

fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                        as_points = FALSE, merge = TRUE)) 
area_thresh <- units::set_units(2, km^2)
fw_dropped <- fill_holes(fw_poly, threshold = area_thresh)
#fw_smooth <- smooth(fw_dropped, method = "ksmooth")
#plot(fw_dropped)


writeRaster(fw, file.path(cdl_dir, "/fw_raw_FW.tif"), format="GTiff", overwrite = TRUE)
writeOGR(fw_poly, cdl_dir,  "/proportion_poly_nlcd", driver = "ESRI Shapefile")
writeOGR(fw_dropped, cdl_dir,  "/proportion_polyd_nlcd", driver = "ESRI Shapefile")
#writeOGR(fw_smooth, cdl_dir,  "/proportion_polys_nlcd", driver = "ESRI Shapefile")




#### Get original CDL crop areas ----
filler<-cbind(3, NA)
average_list<-list()
for(layer in 1:length(county_list)){
  layer=1
  county<-county_list[[layer]]
  list_freq<-list()
  for(f in 1:length(county)){
    county[[f]]<-setExtent(county[[f]],nlcd)
    county[[f]]<-mask(county[[f]], nlcd) #mask by CPAA
    sq<-as.data.frame(freq(county[[f]]))
    
    # ifelse(nrow(sq)<5,sq[5,]==filler,sq[5,]==sq[5,])
    list_freq[[f]]<-sq
  }
  #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
  outliers<-Filter(function(x) nrow(x) <5, list_freq)
  main<-Filter(function(x) nrow(x) >=5, list_freq)
  cdl1<-do.call(cbind, lapply(outliers, as.data.frame))
  cdl2<-do.call(cbind, lapply(main, as.data.frame))
  cdl1[5,]<-filler
  cdl1<-cdl1[c(1:3,5,4),]
  
  value<-cdl1[,1]
  cdl_crop_areas<-cbind(cdl1,cdl2)
  cdl_crop_areas = cdl_crop_areas[,!names(cdl_crop_areas) == 'value']
  cdl_crop_areas$value<-value
  cdl_crop_areas<-cdl_crop_areas[,c(12,3:4,1:2,5:11)]
  names(cdl_crop_areas)<-c("Crop",1999:2009) #make sure names align
  cdl_crop_areas[,2:12]<-(cdl_crop_areas[,2:12]*900) * 0.000247105#get area in acres
  
}



####Analysis ----
#field_vector_data<-fw_dropped@data
field_areas<- as.data.frame(area(fw_dropped), na.rm=T) #get areas of each field from the vectorized field
colnames(field_areas)[1]<-'area'
#field_areas$area<-field_areas

#for each layer in y (sets of 11 years for each county), extract the values to the created polygons
crop_list<-list()
for(layer in 1:length(y)){
    output<-exact_extract(y[[layer]],fw_dropped, "mode")
    output<-as.data.frame(output)
    names(output)<-"crops"
    output$area<-field_areas$area
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
field_crop_areas<-cbind(field1,field2)
field_crop_areas =  field_crop_areas %>% select(-contains(".crops"))
field_crop_areas$value<-value
field_crop_areas<-field_crop_areas[ ,order(names(field_crop_areas))]

field_crop_areas<-field_crop_areas[,c(12,1:11)]
names(field_crop_areas)<-c("crop",1999:2009) #make sure names align


error<-((cdl_crop_areas[,c(2:12)] - field_crop_areas[,c(2:12)])/cdl_crop_areas[,c(2:12)])*100
error$crop<-c("other","corn","soy","ww","NA")

error_f<-tidyr::gather(error, key="year", value="residual", 1:11)
ggplot(error_f, aes(x=year, y=residual, fill=crop) )+ 
  geom_boxplot()+
  facet_wrap(~crop)


error_nona<-error_f[error_f$crop !="NA",]

ggplot(error_nona, aes(x=year, y=residual, fill=crop) )+ 
  geom_boxplot()+
  facet_wrap(~crop)


#greater values means original CDL is larger for that crop
#lower values means vectorized fields are larger for that crop

#new plan:
#extract polygons less than x km2 to new layer
#remove same polygons from original layer
#join/merge polygons from new layer
#and combine back with original

####Sub-delineation ----

#we'll use the new vectors to look at focal window of sequences in each field above a certain size



