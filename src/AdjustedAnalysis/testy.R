### Probabilistic Crop Loading 

### 04 Sub-delineation of fields

# Edited by E. Paulukonis March 2022

import_start_time <- Sys.time()
print("stepping into 04_sub_delineation.R")

options(scipen = 999) #remove exponent options, throws R off

#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to NA (or 9 if needed to count)
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


##in this section, we need to buffer and get the larger CPAA

cpaa <- raster(paste0(cdl_dir, "/cpaa_mask.tif")) #use expanded CPAA
cpaa[cpaa == 0] <- NA
ext<-extent(cpaa)


#reclassify for mask layer
is_n <- c(0:81,82:195,196:256)
becomes_n <- c(rep.int(0,83),rep(NA, 113),rep.int(0,61))
n<-cbind(is_n,becomes_n)

county_list_mask<-list()
layer_list<-list()
for (county in 1:length(county_set_list)){
  county_r<-county_set_list[[county]]
  for(layer in 1:nlayers(county_r)){
    layer_list[[layer]] <- reclassify(county_r[[layer]], n)
  }
  county_list_mask[[county]]<-layer_list
}

mask_c<-county_list_mask[[1]][[11]]
plot(mask_c)
mask_c<-setExtent(mask_c,extent(fw))
mask_c<-projectRaster(mask_c, fw, method='ngb',crs(fw))




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
  any_layer<-rec_layer
  any_layer <- any_layer[!grepl("NANANANANANANANANANANA", testdf$layer.3),] # remove pixels that have no crops in 11 years
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

####Options for delineation
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
  
  #set extent and mask/crop
  df_n<-setExtent(df_n, ext)
  df_n<-crop(df_n, cpaa)
  df_n<-projectRaster(df_n, cpaa, method='ngb',crs(cpaa))
  df_n<-mask(df_n, cpaa)
  
  r<-terra::rast(df_n)
  fw<- terra::focal(r, w = 7, fun = "modal", na.policy='all')%>% 
    terra::mask(mask = r) 
  
  fw<-raster(fw) #convert back
  
  rc <- clump(fw, directions = 8) #this evaluates clumps of pixels (nearest neighbor =  8)
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

plot(fw)



fw<-mask(fw, mask_c)
plot(fw)


fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                      as_points = FALSE, merge = TRUE)) 

plot(fw_poly)


writeRaster(fw_clipped, file.path(cdl_dir, "/fw_clipped.tif"), format="GTiff", overwrite = TRUE)
writeOGR(fw_poly, cdl_dir,  "/proportion_polygons", driver = "ESRI Shapefile")


#challenge:
#this doesn't quite fill some of the data, and I think we should smooth this better



#read in the CPAA and mask to get the same extent


cpaa_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(cpaa), 
                                        as_points = FALSE, merge = TRUE)) 

plot(cpaa_poly)
fw_clipped<-mask(fw_d, cpaa_poly)


plot(county_set_list)