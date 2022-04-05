### Probabilistic Crop Loading 

### 03 Vectorization of Fields, and CAA delineation

# Edited by E. Paulukonis Feb 2022
import_start_time <- Sys.time()
print("stepping into 03_vectorization_fields.R")


##### COUNTY LAYER PREP ####
options(scipen = 999) #remove exponent options, throws R off
study$county_names<-names(study)  #add county names to the study area so we can run the crop function of the CDL to each county

#function to mask and crop CDL to each county
county_set_list<-list()
for (co in 1:length(study)){
  co_r<-study[study$county_names == study$county_names[co],]
  mask_crop<-function(x){
    r_list<-crop(x, co_r)
    mask(r_list, co_r)
  }
  county_set<-lapply(cdl_data_ill_rec[c(1:11)], mask_crop) #crop and mask the fixed CDL to the counties, put in list
  county_set_list[[co]]<-stack(county_set) #represents single county stack
}

plot(county_set_list[[50]])

##### YEARLY AVERAGE LAYERS####
#convert the layers to binary; 1 is crops of pesticide interest, 0 is other crops, NA is non-crop
is_n <- c(0,1,2:4,5,6:23,24,25:256)
becomes_n <- c(NA,1,rep.int(0,3),1,rep.int(0,18),1,rep.int(0,232))
n<-cbind(is_n,becomes_n)

county_list<-list()
layer_list<-list()
for (county in 1:length(county_set_list)){
county_r<-county_set_list[[county]]
  for(layer in 1:nlayers(county_r)){
  layer_list[[layer]] <- reclassify(county_r[[layer]], n)
  }
county_list[[county]]<-layer_list
}

#county_r<-county_list[[1]]

get_area_average<-function(county){
  county<-county_list[[1]]
list_freq<-list()
average_list<-list()
  for(f in 1:length(county)){
    sq<-as.data.frame(freq(county[[f]]))
    list_freq[[f]]<-sq
  }
#this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
df_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
value<-df_crop_areas[,1]
df_crop_areas = df_crop_areas[,!names(df_crop_areas) == 'value']
df_crop_areas$value<-value
df_crop_areas<-df_crop_areas[1:3,c(12,1:11)]
names(df_crop_areas)<-c("Crop",1999:2009) #make sure names align
df_crop_areas[,2:12]<-df_crop_areas[,2:12]*900 #get area in meters
#df_crop_areas[is.na(df_crop_areas)] = 0
#row.names(df_crop_areas)[3]<-'sum'
average_1<-rowMeans(df_crop_areas[2,2:12]) #average of 'pesticide' crops year by year
average_0<-rowMeans(df_crop_areas[1,2:12]) #average of 'other' crops year by year
average_list[[f]]<-average_1

}


output<-lapply(county_list,get_area_average)

##### CALCULATE THRESHOLD ####
thresh_list<-list()
for(i in 1:length(county_list)){
i=1
test_county<-county_list[[i]]
y<-test_county
s0 = brick(y)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer.3 <- data.frame(coords, layer.3)

testdf<-layer.3
testdf <- testdf[!grepl("NANANANANANANANANANANA", testdf$layer.3),] # remove pixels that have no crops in 11 years
unique(testdf$layer.3)
testdf$f<-gsub("NA", "", testdf$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
testdf$field<-as.numeric(testdf$f) #turn f into fields as numeric integer
testdf$n_years<-as.numeric(gsub(0, "", testdf$field))
testdf$bin<-floor(log10(testdf$n_years)) + 1  #create column to count the number of years

#N pixels
testdf$bin<-ifelse(testdf$field == 0, 0, testdf$bin)
n_pixels<-as.data.frame(table(testdf$bin))
# p<-ggplot(n_pixels, aes(x=Var1, y=Freq)) +
#   geom_bar(stat="identity")
# p
n_pixels$total<-NA
for(i in nrow(n_pixels):2){
  n_pixels[i,3]<-sum(n_pixels[i:12,2])*900
 # n_pixels[i,3]<-sum(n_pixels[1:i,2])
}

total_n<-sum(n_pixels$Freq)
n_pixels$sample_p<-n_pixels$Freq/total_n
thresh<-as.data.frame(n_pixels[which.min(abs(average_1-n_pixels$total)),]) #which threshold is closest to the average pesticide area?
print(thresh)
thresh_list[[i]]<-thresh

}

#get the bin numbers that coincide with the threshold or greater
thresh_layers<-testdf[testdf$bin >= (as.numeric(thresh$Var1) - 1),] 
unique(thresh_layers$bin) #double check that it looks good
thresh_layers$bin_f<-1

##### CONVERT BACK TO RASTER DATA ####
df<-thresh_layers
df<-df[,c(1:2,8)]
plot_list<-list()
for(n in unique(df$bin)){
  df_n<-df[df$bin ==n,]
  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_ill_rec[[1]])
  plot(df_n)
  plot_list[[n]]<-df_n
  
}
plot_list<-plot_list[lengths(plot_list) != 0] #remove empty list elements



##### VECTORIZE ####
#first, let's remove small individual 'islands' of pixels
cleaned_plots<-list()
for (layer in 1:length(plot_list)){
rc <- clump(plot_list[[layer]], directions = 8) #this evaluates clumps of pixels (nearest neighbor =  8)
f<-freq(rc)
# save frequency table as data frame
f<-as.data.frame(f)
# which rows of the data.frame are only represented by clumps = 1 pixels?
excludeID <- f$value[which(f$count <= 1)]
# make a new raster to be sieved
formaskSieve <- rc
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[rc %in% excludeID] <- NA
plot_l<-plot_list[[layer]]
plot_l<-mask(plot_l, formaskSieve)
cleaned_plots[[layer]]<-plot_l
}

plot(plot_list[[1]], col='blue')
plot(cleaned_plots[[1]], col="blue")

#let's try vectorizing with the sf package, which is quite fast
r1<-cleaned_plots[[1]]
r.to.poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(r1), 
                                         as_points = FALSE, merge = TRUE)) 

#first, we'll fill in some of the holes; I used a larger buffer of 500 km2 here; this might be too big
area_thresh <- units::set_units(200, km^2)
p_dropped <- fill_holes(r.to.poly, threshold = area_thresh)
plot(p_dropped)
p_dropped<-aggregate(p_dropped)

#issue: this might fill too many gaps; for now it's ok but we should focus on sequencing within the original reclassified layers


##write rasters and/or shapefiles
writeRaster(r1, file.path(cdl_dir, "/r1_11"), format="GTiff", overwrite = TRUE)
writeOGR(p_dropped, cdl_dir,  "/r_polyall_fin", driver = "ESRI Shapefile")

