### Probabilistic Crop Loading 

### 03 Vectorization of Fields

# Edited by E. Paulukonis Feb 2022
import_start_time <- Sys.time()
print("stepping into 03_vectorization_fields.R")


##### COUNTY LAYER PREP ####
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


##### YEARLY AVERAGE LAYERS####
#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to 9
is_m <- c(0,1,2:4,5,6:23,24,25:256)
becomes <- c(NA,1,rep.int(9,3),2,rep.int(9,18),3,rep.int(9,232))
m<-cbind(is_m,becomes)

for (county in 1:length(county_set_list)){
county_r<-county_set_list[[county]]
for(layer in 1:nlayers(county_r)){
layer_list[[layer]] <- reclassify(county_r[[layer]], m)
}
county_list[[county]]<-layer_list
}

#county_r<-county_list[[1]]

get_area_average<-function(county){
list_freq<-list()
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

}

output<-lapply(county_list,get_area_average)

##### BINARY N YEAR LAYERS ####
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

thresh_list<-list()
for(i in 1:length(county_list)){

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
  n_pixels[i,3]<-sum(n_pixels[i:12,2])
 # n_pixels[i,3]<-sum(n_pixels[1:i,2])
}

total_n<-sum(n_pixels$Freq)
n_pixels$sample_p<-n_pixels$Freq/total_n

thresh<-n_pixels[which.min(abs(average_1-n_pixels$total)),]

thresh_list[[i]]<-thresh

}

##### PLOT DATA ####
#sample proportion; same thing as above, but with distribution means represented
p<-ggplot(n_pixels, aes(x=Var1, y=sample_p)) +
  geom_bar(stat="identity")
p


#let's map the combinations of years, starting with 11
testl<-testdf
testl<-testl[,c(1:2,7)]

#pal_r = colorRampPalette(c("#ffffcc","#006837"))

pal <-c("#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#e31a1c",
    "#fdbf6f",
    "#ff7f00",
    "#cab2d6",
    "#6a3d9a",
    "#fed976") 

# pal_all<-c(
#   "#525252",
#   "#f7fcf0",
#   "#e0f3db",
#   "#ccebc5",
#   "#a8ddb5",
#   "#7bccc4",
#   "#4eb3d3",
#   "#2b8cbe",
#   "#0868ac",
#   "#084081"
#   )
  
scales::show_col(pal) #take a look at color palettes

# this maps each individual set of N years
df<-testl
plot_list<-list()
for(n in 1:11){
  df_n<-df[df$bin >=n,]
  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_ill_rec[[1]])
  plot(df_n)

 plot_list[[n]]<-df_n

}

#this corrects the colors for N years
plot_maps<-function(raster){
  n<-length(unique(values(raster)))
  plot(raster, col=pal[(n-1):1])
}

plots<-lapply(plot_list,plot_maps) #plot them with the right colors



##look at the total crop areas and get average across all 11 years
writeRaster(set1[[11]], file.path(cdl_dir, "/cdl2009"), format="GTiff", overwrite = TRUE)
writeRaster(testl, file.path(cdl_dir, "/test_"), format="GTiff", overwrite = TRUE)







