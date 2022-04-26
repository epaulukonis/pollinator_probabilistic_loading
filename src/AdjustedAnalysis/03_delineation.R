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

#county_set_list contains the clipped sets of CDLs for each set of years

##### YEARLY AVERAGE LAYERS####
#convert the layers to binary; 1 is crops of pesticide interest, 0 is other crops, NA is non-crop
is_n <- c(0,1,2:4,5,6:23,24,25:256)
becomes_n <- c(NA,1,rep.int(0,3),1,rep.int(0,18),1,rep.int(0,232))
n<-cbind(is_n,becomes_n)

#this reclassifys the layers from county_set_list to be binary (crop and non-crop)
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
average_list<-list()
for(layer in 1:length(county_list)){
  county<-county_list[[layer]]
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

      average_1<-rowMeans(df_crop_areas[2,2:12]) #average of 'pesticide' crops year by year
      average_0<-rowMeans(df_crop_areas[1,2:12]) #average of 'other' crops year by year
      average_list[[f]]<-average_1

}



##### CALCULATE THRESHOLD ####
thresh_list<-list()
for(i in 1:length(county_list)){
  i=1
county<-county_list[[i]]
y<-county
s0 = brick(y)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer.3 <- data.frame(coords, layer.3)

county_binned<-layer.3
county_binned <- county_binned[!grepl("NANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 11 years
county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years

#N pixels
county_binned$bin<-ifelse(county_binned$field == 0, 0, county_binned$bin)
n_pixels<-as.data.frame(table(county_binned$bin))
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

average_1 = average_list[[i]]
thresh<-as.data.frame(n_pixels[which.min(abs(average_1-n_pixels$total)),]) #which threshold is closest to the average pesticide area?
thresh_list[[i]]<-thresh

#get the bin numbers that coincide with the threshold or greater
thresh_layers<-county_binned[county_binned$bin >= (as.numeric(thresh$Var1) - 1),] 
unique(thresh_layers$bin) #double check that it looks good
thresh_layers$bin_f<-1  #if you want binary layer
thresh_list[[i]]<-thresh_layers

}

#####Get CPAA and mask
##in this section, we need to buffer and get the larger CPAA
cpaa <- raster(paste0(cdl_dir, "/cpaa_mask.tif")) #use expanded CPAA
cpaa[cpaa == 0] <- NA
ext<-extent(cpaa)
plot(cpaa)


#use 2008 NLCD to mask out non-crop
# nlcd<-raster(paste0(nlcd_dir,"/NLCD_2008_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))
# nlcd<-setExtent(nlcd, ext)
# nlcd<-crop(nlcd, cpaa)
# nlcd<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
# nlcd<-mask(nlcd, cpaa)
# plot(nlcd)
# nlcd[nlcd<=30]<-NA
# plot(nlcd)
# cpaa<-mask(cpaa, nlcd)
# plot(cpaa)

