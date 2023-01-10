### Probabilistic Crop Loading 

### 03 Getting CPAA Threshold and Masking by NLCD

# Edited by E. Paulukonis June 2022
import_start_time <- Sys.time()
print("stepping into 03_CPAA_threshold.R")

thresh_filename<-paste0(cdl_ill_dir, "/threshold_1.csv")
if(file.exists(thresh_filename)){
  
  print(list.files(path=cdl_ill_dir, pattern='.csv$', all.files=TRUE, full.names=FALSE))
  thresh_ill <- file.path(cdl_ill_dir, list.files(path=cdl_ill_dir, pattern='.csv$', all.files=TRUE, full.names=FALSE))
  thresh_list<-lapply(thresh_ill, read.csv)
  thresh_list<-lapply(thresh_list, function(y) { y["X"] <- NULL; y })
  
  print(list.files(path=paste0(cdl_ill_dir,"/threshold"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  thresh_rasters <- file.path(paste0(cdl_ill_dir,"/threshold"), list.files(path=paste0(cdl_ill_dir,"/threshold"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  three_county_list<-lapply(thresh_rasters, raster)
  
  ##note, this needs to be organized according to the thresh list
  names(three_county_list)<-c("high14","low50","medium1")
  three_county_list <- three_county_list[c("low50","medium1","high14")]
  
  print('we have already done the threshold determination, read in the three county lists and dataframes')
  
}else{


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
  county_set<-lapply(cdl_data_ill_rec[c(10:23)], mask_crop) #crop and mask the fixed CDL to the counties, put in list
  county_set_list[[co]]<-stack(county_set) #represents single county stack
}

#county_set_list contains the clipped sets of CDLs for each set of years


##### CROP PROPORTIONS ####
#for each county; let's calculate the proportion of crop to non-crop; we will select a highly cropped region, a mid-cropped region, and an urban area
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

#### Turn this on or off depending on if you need to re-evaluate the percent of total crop/non-crop

# average_list<-list()
# extracted_field_list<-list()
# for(layer in 1:length(county_list)){
#   county<-county_list[[layer]]
#   list_freq<-list()
#   fill1=c(0,0)
#   fill2=c(1,0)
#  
#   for(f in 1:length(county)){
#     cdl<-county[[f]]
#     # plot(output)
#     # cdl<-setExtent(cdl,output)
#     # cdl<-crop(cdl, output)
#     # cdl<-mask(cdl, output) 
#     sq<-as.data.frame(freq(cdl))
#     #test<-freq(cdl)
#     #writeRaster(cdl, file.path(cdl_dir, "/cdl_mask.tif"), format="GTiff", overwrite = TRUE)
#     
#   sq<- if (nrow(sq) ==2 & sq[1,1] == 1){  ##need to fix the order because of the way total crop area is
#       rbind(sq,fill1)
#   } else if (nrow(sq) == 2 & sq[1,1] == 0){
#     rbind(sq,fill2)
#   } else if (nrow(sq) == 1){
#     rbind(sq,fill1,fill2)
#   }else{
#     sq
#   }
#   
#    sq<-sq[order(sq[,1]),]
#     list_freq[[f]]<-sq
#   }
#   #this unlists the frequencies to count the total proportion of crop area over time 
#   
#   cdl_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
#   value<-cdl_crop_areas$value
#   cdl_crop_areas = cdl_crop_areas[,!names(cdl_crop_areas) == 'value']
#   cdl_crop_areas$value<-value
#   cdl_crop_areas[,1:11]<-(cdl_crop_areas[,1:11]*900) * 0.000247105 #get area in acres
#   names(cdl_crop_areas)<-c(1999:2009,"crop") #make sure names align
#   extracted_field_list[[layer]]<-cdl_crop_areas
#   
#   cdl_crop<-colSums(cdl_crop_areas[1:2,1:11])
#   cdl_noncrop<-cdl_crop_areas[3,1:11]
#   cdl_total<-sum(cdl_crop_areas[1:3,1])
#   percent_cdl_crop<-(cdl_crop/cdl_total)*100
#   
#   average_list[[layer]]<-mean(percent_cdl_crop)
#   
#   
# }


#plot(county_list[[14]][[1]])


#average percent of CDL counties that are ag
# avg_cropped_by_county<-do.call(rbind, lapply(average_list, as.data.frame))
# colnames(avg_cropped_by_county)[1]<-"Average_Acres"
# avg_cropped_by_county$county_code<-row.names(avg_cropped_by_county)
# avg_cropped_by_county<-na.omit(avg_cropped_by_county)
# 
# max(avg_cropped_by_county$Average_Acres) 
# min(avg_cropped_by_county$Average_Acres)

#50 8% DUPAGE low

#1 40% McHENRY medium

#14 72% CHAMPAIGN high


three_county_list<-list(county_list[[50]], county_list[[1]],county_list[[14]])
names(three_county_list)<-c("low50","medium1","high14")
f<-cdl_ill_dir
for(layer in 1:length(three_county_list)){
output<-stack(three_county_list[[layer]])
writeRaster(output, file.path(f, names(three_county_list[layer])), format="GTiff", overwrite = TRUE)
}

##### YEARLY AVERAGE LAYERS#### 
##get average area for threshold here
average_list_focus<-list()
for(layer in 1:length(three_county_list)){
  county<-three_county_list[[layer]]
  list_freq2<-list()
    for(f in 1:length(county)){
      sq<-as.data.frame(freq(county[[f]]))
      list_freq2[[f]]<-sq
    }
      #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
      df_crop_areas<-do.call(cbind, lapply(list_freq2, as.data.frame))
      value<-df_crop_areas[,1]
      df_crop_areas = df_crop_areas[,!names(df_crop_areas) == 'value']
      df_crop_areas$value<-value
      df_crop_areas<-df_crop_areas[1:3,c(15, 1:14)]
      names(df_crop_areas)<-c("Crop",2008:2021) #make sure names align
      df_crop_areas[,2:15]<-(df_crop_areas[,2:15]*900 ) * 0.000247105 #get area in acres

      average_1<-rowMeans(df_crop_areas[2,2:15]) #average of 'pesticide' crops year by year
      average_0<-rowMeans(df_crop_areas[1,2:15]) #average of 'other' crops year by year
      average_list_focus[[layer]]<-average_1

}




##### CALCULATE THRESHOLD ####
thresh_list<-list()
thresh_list_raw<-list() #documents the exact year of the cutoff
for(item in 1:length(three_county_list)){
county<-three_county_list[[item]]
y<-county
s0 = brick(y)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
layer = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer <- data.frame(coords, layer)

county_binned<-layer
county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer),] # remove pixels that have no crops in 13 years
county_binned$f<-gsub("NA", "", county_binned$layer) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
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
  n_pixels[i,3]<-(sum(n_pixels[i:14,2])*900)*0.000247105
 # n_pixels[i,3]<-sum(n_pixels[1:i,2])
}

total_n<-sum(n_pixels$Freq)
n_pixels$sample_p<-n_pixels$Freq/total_n

average_1 = average_list_focus[[item]]
thresh<-as.data.frame(n_pixels[which.min(abs(average_1-n_pixels$total)),]) #which threshold is closest to the average pesticide area?
thresh_list_raw[[item]]<-thresh

#get the bin numbers that coincide with the threshold or greater
thresh_layers<-county_binned[county_binned$bin >= (as.numeric(thresh$Var1)-1),] 
unique(thresh_layers$bin) #double check that it looks good
thresh_layers$bin_f<-1  #if you want binary layer

#names(thresh_layers)<-paste0("threshold",item)


thresh_list[[item]]<-thresh_layers

}

names(thresh_list)<-paste0("threshold_",1:3)
mapply(write.csv, thresh_list, file=paste0(cdl_dir, "/", names(thresh_list), '.csv'))

##for low: min bin is 2
##for med: min bin is 14 (it was previously 6...hm)
##for high: min bin is 14
}


#####Get NLCD mask for non-crop areas (roadS)----
nlcd<-raster(paste0(nlcd_dir,"/NLCD_2019_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))
#nlcd<-raster(paste0(nlcd_dir,"/NLCD_2008_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))

list_of_nlcd_masks<-list()
for (county in 1:3){
cpaa<-three_county_list[[county]][[1]] ##get associated counties
ext<-extent(cpaa)
#plot(cpaa)

#use 2008 NLCD to mask out non-crop

nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
nlcdc<-crop(nlcdc, cpaa)
nlcdc<-mask(nlcdc, cpaa)

nlcdc[nlcdc==11]<-NA
nlcdc[nlcdc==21]<-NA
nlcdc[nlcdc==22]<-NA
nlcdc[nlcdc==23]<-NA
nlcdc[nlcdc==24]<-NA
nlcdc[nlcdc==31]<-NA


list_of_nlcd_masks[[county]]<-nlcdc

}

#writeRaster(nlcd, file.path(cdl_dir, "/nlcd_mask.tif"), format="GTiff", overwrite = TRUE)


