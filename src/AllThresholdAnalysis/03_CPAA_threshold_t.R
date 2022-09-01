### Probabilistic Crop Loading 

### 03 evaluating crop diversity and getting CPAA threshold

# Edited by E. Paulukonis June 2022
import_start_time <- Sys.time()
print("stepping into 03_CPAA_threshold_c.R")

##### COUNTY LAYER PREP
options(scipen = 999) #remove exponent options, throws R off

###NOTE: order of counties is very important; it's crucial to test that you have the correct county by visual aid now and again.

#### Illinois ----
# thresh_ill_filename<-paste0(root_data_out, "/all_thresh/Illinois/DuPage2008.csv")
# if(file.exists(thresh_mi_filename)){
# 
# 
# }else{
study<-ill
sub_group<-c("DuPage","McHenry","Champaign")
sub<-study[study$NAME %in% sub_group,]

county_set_list<-list()
for (co in 1:length(sub)){
  co_r<-sub[sub$NAME == sub$NAME[co],]
  mask_crop<-function(x){
    r_list<-crop(x, co_r)
    mask(r_list, co_r)
  }
  county_set<-lapply(cdl_data_ill_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
  county_set_list[[co]]<-stack(county_set) #represents single county stack
}

names(county_set_list)<-sub_group
du<-county_set_list$DuPage #low
mch<-county_set_list$McHenry #medium
chp<-county_set_list$Champaign #high

chosen_count<-list(du,mch,chp)
names_cc<-c("DuPage","McHenry","Champaign")
names(chosen_count)<-names_cc


#Let's reclassify to count the n years for all thresholds
is_n <- c(0,1:256)
becomes_n <- c(NA,rep.int(1,256))
n<-cbind(is_n,becomes_n)

#this reclassifys the layers from county_set_list to be binary (non-diverse crop and diverse crop)
county_list<-list()
layer_list<-list()
for (county in 1:length(chosen_count)){
  county_r<-chosen_count[[county]]
  for(layer in 1:nlayers(county_r)){
    layer_list[[layer]] <- reclassify(county_r[[layer]], n)
  }
  
  layer_list<-layer_list[-c(1:9)]
  county_list[[county]]<-layer_list
}

names(county_list)<-names_cc


##### CALCULATE THRESHOLD 
thresh_list_by_county_ill<-list() #contains all the datasets by county
thresh_list_by_year_ill<-list() #contains all the datasets by yar

for(item in 1:length(county_list)){ #this loop pulls out the county in the three county list
  county<-county_list[[item]]
  
  for(year in 1:14){ #this loop calculates the threshold data for each individual year, and sticks that in the list
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
  
  #get the bin numbers that coincide with the threshold or greater
  thresh_layers<-county_binned[county_binned$bin >= (as.numeric(year)),] 
  unique(thresh_layers$bin) #double check that it looks good
  thresh_layers$bin_f<-1  #if you want binary layer
  thresh_layers$county<-names(county_list)[item]
  thresh_list_by_year_ill[[year]]<-thresh_layers
  
  }
  
  names(thresh_list_by_year_ill)<-2008:2021
  
  f<-paste0(root_data_out,'/all_thresh/Illinois/')
  for(i in names(thresh_list_by_year_ill)){
    write.csv(thresh_list_by_county_ill[[i]], paste0(f,names(county_list)[item],i,".csv"))
    
  }
  
  
 thresh_list_by_county_ill[[county]]<-thresh_list_by_year_ill #this sticks the list of 14 threshold datasets into a list by county
}


# #####Get NLCD mask for non-crop areas (roadS)
# nlcd<-raster(paste0(nlcd_dir,"/nlcd2019.tif"))
# #nlcd<-raster(paste0(nlcd_dir,"/NLCD_2008_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))
# 
# list_of_nlcd_masks_ill<-list()
# for (county in 1:3){
#   cpaa<-county_list[[county]][[1]] ##get associated counties
#   ext<-extent(cpaa)
#   #plot(cpaa)
#   
#   #use 2008 NLCD to mask out non-crop
#   
#   nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
#   nlcdc<-crop(nlcdc, cpaa)
#   nlcdc<-mask(nlcdc, cpaa)
#   
#   nlcdc[nlcdc==11]<-NA
#   nlcdc[nlcdc==21]<-NA
#   nlcdc[nlcdc==22]<-NA
#   nlcdc[nlcdc==23]<-NA
#   nlcdc[nlcdc==24]<-NA
#   nlcdc[nlcdc==31]<-NA
#   
#   names(nlcdc)<-names(county_list)[[county]][[1]]
#   list_of_nlcd_masks_ill[[county]]<-nlcdc
#   
# }
# 
# names(list_of_nlcd_masks_ill)<-names_cc
# f<-paste0(root_data_out, "/all_NLCD/Illinois")
# for(layer in 1:length(list_of_nlcd_masks_ill)){
#   writeRaster(list_of_nlcd_masks_ill[[layer]], file.path(f, names(list_of_nlcd_masks_ill[[layer]])), format="GTiff", overwrite = TRUE)
#   }
# 
# }












# 
# 
# #### Michigan ----
# 
# thresh_mi_filename<-paste0(cdl_mi_dir, "/thresh_layers/thresh_layersHuron75.csv")
# if(file.exists(thresh_mi_filename)){
# 
#   print(list.files(path=paste0(cdl_mi_dir,"/thresh_layers"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
#   thresh_mi<- file.path(paste0(cdl_mi_dir,"/thresh_layers"), list.files(path=paste0(cdl_mi_dir,"/thresh_layers"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
#   thresh_list_mi<-lapply(thresh_mi, read.csv)
#   thresh_list_mi<-lapply(thresh_list_mi, function(y) { y["X"] <- NULL; y })
# 
#   print(list.files(path=paste0(cdl_mi_dir,"/thresh_layers"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   thresh_rasters <- file.path(paste0(cdl_mi_dir,"/thresh_layers"), list.files(path=paste0(cdl_mi_dir,"/thresh_layers"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   mi_county_list<-lapply(thresh_rasters, raster)
# 
# 
#   f<-paste0(cdl_mi_dir, "/mask_output")
#   print(list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   nlcd_mi<- file.path(f, list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   nlcd_mi<-lapply(nlcd_mi, raster)
# 
#   print('we have already done the threshold determination, read in the three county lists, nlcds, and dataframes')
# 
# }else{
# #function to mask and crop CDL to each county
# study<-mi
# sub_group<-c("Van Buren", "Oceana","Huron")
# sub<-study[study$NAME %in% sub_group,]
# 
# county_set_list<-list()
# for (co in 1:length(sub)){
#   co_r<-sub[sub$NAME == sub$NAME[co],]
#   mask_crop<-function(x){
#     r_list<-crop(x, co_r)
#     mask(r_list, co_r)
#    }
#   county_set<-lapply(cdl_data_mi_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
#   county_set_list[[co]]<-stack(county_set) #represents single county stack
#  }
# 
# #county_set_list contains the clipped sets of CDLs for each set of years
# #### CROP DIVERSITY
# #I chose these based on visual inspection; they have crop diversity > 1, and tree crops
# names(county_set_list)<-sub_group
# hu75<-county_set_list$Huron #75% crop coverage
# van35<-county_set_list$`Van Buren` #50%
# oc25<-county_set_list$Oceana #25%
# 
# 
# #### you can use this section to look at which crops specifically have higher than 3% acreage in the states of interest
# # out_van<-freq(van50)
# # out_hu<-freq(hu75)
# # out_ott<-freq(ott)
# 
# # out<-out_hu
# # prop_list<-list()
# # for(i in 1:length(out)){
# #   x<-out[[i]]
# #   x<-na.omit(x)
# #     prop<-as.data.frame(x[,2]/sum(x[,2]))
# #     names(prop)<-'proportion'
# #     prop$value<-x[,1]
# #     prop_list[[i]]<-prop
# # }
# #
# #
# # diversity_function<-function(x){
# #   out<-which(x[,1] >= 0.03)
# #   x$row<-as.numeric(row.names(x))
# #   crops<-x[x$row %in% out,]
# # }
# #
# # crop_list_hu<-lapply(prop_list,diversity_function)
# # print(crop_list_hu) #what's the general trend in crops?
# # names(crop_list_hu)<-2008:2021
# 
# chosen_count<-list(oc25,van35,hu75)
# names_cc<-c("Oceana25","VanBuren50","Huron75")
# names(chosen_count)<-names_cc
# 
# 
# ##### YEARLY AVERAGE LAYERS
# ##get average area for threshold here
# #this reclassifys all of the non-major crops
# is_n <- c(0,1:256)
# becomes_n <- c(NA,rep.int(1,256))
# n<-cbind(is_n,becomes_n)
# 
# #this reclassifys the layers from county_set_list to be binary (non-diverse crop and diverse crop)
# county_list<-list()
# layer_list<-list()
# for (county in 1:length(chosen_count)){
#   county_r<-chosen_count[[county]]
#   for(layer in 1:nlayers(county_r)){
#     layer_list[[layer]] <- reclassify(county_r[[layer]], n)
#    }
#   county_list[[county]]<-layer_list
#  }
# 
# names(county_list)<-names_cc
# 
# f<-cdl_mi_dir
# for(layer in 1:length(county_list)){
#   output<-stack(county_list[[layer]])
#   writeRaster(output, file.path(f, names(county_list[layer])), format="GTiff", overwrite = TRUE)
#  }
# 
# extracted_field_list<-list()
# average_list_focus<-list()
# for(layer in 1:length(county_list)){
#   county<-county_list[[layer]]
#   list_freq<-list()
#     for(f in 1:length(county)){
#       sq<-as.data.frame(freq(county[[f]]))
#       sq[2,1]<-0
#       list_freq[[f]]<-sq
#     }
#       #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
#     cdl_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
#     value<-cdl_crop_areas$value
#     cdl_crop_areas = cdl_crop_areas[,!names(cdl_crop_areas) == 'value']
#     cdl_crop_areas$value<-value
#     cdl_crop_areas[,1:14]<-(cdl_crop_areas[,1:14]*900) * 0.000247105 #get area in acres
#     names(cdl_crop_areas)<-c(2008:2021,"crop") #make sure names align
#     extracted_field_list[[layer]]<-cdl_crop_areas
# 
#       average_1<-rowMeans(cdl_crop_areas[1,2:14]) #average of crops year by year
#       average_0<-rowMeans(cdl_crop_areas[10,2:14]) #average of 'other' year by year
#       average_list_focus[[layer]]<-average_1
# 
#  }
# 
# 
# ##### CALCULATE THRESHOLD
# thresh_list_mi<-list()
# thresh_list_raw_mi<-list() #documents the exact year of the cutoff
# for(item in 1:length(county_list)){
# county<-county_list[[item]]
# y<-county
# s0 = brick(y)
# coords = coordinates(s0)
# s1 = as.data.frame(getValues(s0))
# layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
# layer.3 <- data.frame(coords, layer.3)
# county_binned<-layer.3
# county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 14 years
# county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
# county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
# county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
# county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years
# 
# #N pixels
# county_binned$bin<-ifelse(county_binned$field == 0, 0, county_binned$bin)
# n_pixels<-as.data.frame(table(county_binned$bin))
# # p<-ggplot(n_pixels, aes(x=Var1, y=Freq)) +
# #   geom_bar(stat="identity")
# # p
# n_pixels$total<-NA
# for(i in nrow(n_pixels):2){
#   n_pixels[i,3]<-(sum(n_pixels[i:14,2])*900)*0.000247105
#  # n_pixels[i,3]<-sum(n_pixels[1:i,2])
#  }
# 
# total_n<-sum(n_pixels$Freq)
# n_pixels$sample_p<-n_pixels$Freq/total_n
# 
# average_1 = average_list_focus[[item]]
# thresh<-as.data.frame(n_pixels[which.min(abs(average_1-n_pixels$total)),]) #which threshold is closest to the average pesticide area?
# thresh_list_raw_mi[[item]]<-thresh
# 
# #get the bin numbers that coincide with the threshold or greater
# thresh_layers<-county_binned[county_binned$bin >= (as.numeric(thresh$Var1)-1),]
# unique(thresh_layers$bin) #double check that it looks good
# thresh_layers$bin_f<-1  #if you want binary layer
# thresh_layers$county<-names(county_list[item])
# thresh_list_mi[[item]]<-thresh_layers
# 
# }
# names(thresh_list_mi)<-names_cc
# f<-paste0(cdl_mi_dir,'/thresh_layers')
# for(i in names(thresh_list_mi)){
#   write.csv(thresh_list_mi[[i]], paste0(f,i,".csv"))
# 
# }
# 
# 
# 
# #####Get NLCD mask for non-crop areas (roadS)
# nlcd<-raster(paste0(nlcd_dir,"/nlcd2019.tif"))
# #nlcd<-raster(paste0(nlcd_dir,"/NLCD_2008_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))
# 
# list_of_nlcd_masks_mi<-list()
# for (county in 1:3){
# cpaa<-county_list[[county]][[1]] ##get associated counties
# ext<-extent(cpaa)
# #plot(cpaa)
# 
# #use 2008 NLCD to mask out non-crop
# 
# nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
# nlcdc<-crop(nlcdc, cpaa)
# nlcdc<-mask(nlcdc, cpaa)
# 
# nlcdc[nlcdc==11]<-NA
# nlcdc[nlcdc==21]<-NA
# nlcdc[nlcdc==22]<-NA
# nlcdc[nlcdc==23]<-NA
# nlcdc[nlcdc==24]<-NA
# nlcdc[nlcdc==31]<-NA
# 
# names(nlcdc)<-names(county_list)[[county]][[1]]
# list_of_nlcd_masks_mi[[county]]<-nlcdc
# 
#  }
# 
# names(list_of_nlcd_masks_mi)<-names_cc
# f<-paste0(cdl_mi_dir, "/mask_output")
# for(layer in 1:length(list_of_nlcd_masks_mi)){
# writeRaster(list_of_nlcd_masks_mi[[layer]], file.path(f, names(list_of_nlcd_masks_mi[[layer]])), format="GTiff", overwrite = TRUE)
#  }
# 
# }
# 
# 





# #### Wisconsin ----
# 
# thresh_wi_filename<-paste0(cdl_wi_dir, "/thresh_layers/thresh_layersWau35.csv")
# if(file.exists(thresh_wi_filename)){
#   
#   print(list.files(path=paste0(cdl_wi_dir,"/thresh_layers"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
#   thresh_wi<- file.path(paste0(cdl_wi_dir,"/thresh_layers"), list.files(path=paste0(cdl_wi_dir,"/thresh_layers"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
#   thresh_list_wi<-lapply(thresh_wi, read.csv)
#   thresh_list_wi<-lapply(thresh_list_wi, function(y) { y["X"] <- NULL; y })
#   
#   print(list.files(path=paste0(cdl_wi_dir,"/thresh_layers"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   thresh_rasters <- file.path(paste0(cdl_wi_dir,"/thresh_layers"), list.files(path=paste0(cdl_wi_dir,"/thresh_layers"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   wi_county_list<-lapply(thresh_rasters, raster)
#   
#   f<-paste0(cdl_wi_dir, "/mask_output")
#   print(list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   nlcd_wi<- file.path(f, list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
#   nlcd_wi<-lapply(nlcd_wi, raster)
#   
#   
#   print('we have already done the threshold determination, read in the three county lists and dataframes')
#   
# }else{
# study<-wi
# sub_group<-c("Waushara","Langlade","Rock")
# sub<-study[study$NAME %in% sub_group,]
# 
# county_set_list<-list()
# for (co in 1:length(sub)){
#   co_r<-sub[sub$NAME == sub$NAME[co],]
#   mask_crop<-function(x){
#     r_list<-crop(x, co_r)
#     mask(r_list, co_r)
#   }
#   county_set<-lapply(cdl_data_wi_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
#   county_set_list[[co]]<-stack(county_set) #represents single county stack
#  }
# 
# 
# #county_set_list contains the clipped sets of CDLs for each set of years
# #### CROP DIVERSITY
# 
# #I chose these based on visual inspection; they have crop diversity > 1, and tree crops
# names(county_set_list)<-sub_group
# rock65<-county_set_list$Rock#65% crop coverage
# wau35<-county_set_list$Waushara #35%
# lang15<-county_set_list$Langlade #10-15%
# 
# 
# #### you can use this section to look at which crops specifically have higher than 3% acreage in the states of interest
# # out_rock<-freq(rock)
# # out_wau<-freq(wau)
# # out_lang<-freq(lang)
# # 
# # out<-out_rock
# # prop_list<-list()
# # for(i in 1:length(out)){
# #   x<-out[[i]]
# #   x<-na.omit(x)
# #   prop<-as.data.frame(x[,2]/sum(x[,2]))
# #   names(prop)<-'proportion'
# #   prop$value<-x[,1]
# #   prop_list[[i]]<-prop
# # }
# # 
# # 
# # diversity_function<-function(x){
# #   out<-which(x[,1] >= 0.03)
# #   x$row<-as.numeric(row.names(x))
# #   crops<-x[x$row %in% out,]
# # }
# # 
# # crop_list_rock<-lapply(prop_list,diversity_function)
# # print(crop_list_rock) #what's the general trend in crops? 
# # names(crop_list_rock)<-2008:2021
# 
# 
# chosen_count<-list(lang15, wau35, rock65)
# names_cc<-c("Lang15","Wau35","Rock65")
# names(chosen_count)<-names_cc
# 
# 
# ##### YEARLY AVERAGE LAYERS
# ##get average area for threshold here
# #this reclassifys all of the non-major crops
# is_n <- c(0,1:256)
# becomes_n <- c(NA,rep.int(1,256))
# n<-cbind(is_n,becomes_n)
# 
# 
# #this re-classifys the layers from county_set_list to be binary (non-diverse crop and diverse crop)
# county_list<-list()
# layer_list<-list()
# for (county in 1:length(chosen_count)){
#   county_r<-chosen_count[[county]]
#   for(layer in 1:nlayers(county_r)){
#     layer_list[[layer]] <- reclassify(county_r[[layer]], n)
#    }
#   county_list[[county]]<-layer_list
# }
# rm(county_r)
# names(county_list)<-names_cc
# 
# f<-cdl_wi_dir
# for(layer in 1:length(county_list)){
#   output<-stack(county_list[[layer]])
#   writeRaster(output, file.path(f, names(county_list[layer])), format="GTiff", overwrite = TRUE)
# }
# rm(chosen_count)
# 
# 
# extracted_field_list<-list()
# average_list_focus<-list()
# for(layer in 1:length(county_list)){
#   county<-county_list[[layer]]
#   list_freq<-list()
#   for(f in 1:length(county)){
#     sq<-as.data.frame(freq(county[[f]]))
#     sq[2,1]<-0
#     list_freq[[f]]<-sq
#   }
#   #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
#   cdl_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
#   value<-cdl_crop_areas$value
#   cdl_crop_areas = cdl_crop_areas[,!names(cdl_crop_areas) == 'value']
#   cdl_crop_areas$value<-value
#   cdl_crop_areas[,1:14]<-(cdl_crop_areas[,1:14]*900) * 0.000247105 #get area in acres
#   names(cdl_crop_areas)<-c(2008:2021,"crop") #make sure names align
#   extracted_field_list[[layer]]<-cdl_crop_areas
#   average_1<-rowMeans(cdl_crop_areas[1,2:14]) #average of crops year by year
#   average_0<-rowMeans(cdl_crop_areas[10,2:14]) #average of 'other' year by year
#   average_list_focus[[layer]]<-average_1
#   }
# 
# ##### CALCULATE THRESHOLD 
# thresh_list_wi<-list()
# thresh_list_raw_wi<-list() #documents the exact year of the cutoff
# for(item in 1:length(county_list)){
#   county<-county_list[[item]]
#   y<-county
#   s0 = brick(y)
#   coords = coordinates(s0) 
#   s1 = as.data.frame(getValues(s0))
#   layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
#   layer.3 <- data.frame(coords, layer.3)
#   
#   county_binned<-layer.3
#   county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 11 years
#   county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
#   county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
#   county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
#   county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years
#   
#   #N pixels
#   county_binned$bin<-ifelse(county_binned$field == 0, 0, county_binned$bin)
#   n_pixels<-as.data.frame(table(county_binned$bin))
#   # p<-ggplot(n_pixels, aes(x=Var1, y=Freq)) +
#   #   geom_bar(stat="identity")
#   # p
#   n_pixels$total<-NA
#   for(i in nrow(n_pixels):2){
#     n_pixels[i,3]<-(sum(n_pixels[i:14,2])*900)*0.000247105
#     # n_pixels[i,3]<-sum(n_pixels[1:i,2])
#   }
#   
#   total_n<-sum(n_pixels$Freq)
#   n_pixels$sample_p<-n_pixels$Freq/total_n
#   
#   average_1 = average_list_focus[[item]]
#   thresh<-as.data.frame(n_pixels[which.min(abs(average_1-n_pixels$total)),]) #which threshold is closest to the average pesticide area?
#   thresh_list_raw_wi[[item]]<-thresh
#   
#   #get the bin numbers that coincide with the threshold or greater
#   thresh_layers<-county_binned[county_binned$bin >= (as.numeric(thresh$Var1)-1),] 
#   unique(thresh_layers$bin) #double check that it looks good
#   thresh_layers$bin_f<-1  #if you want binary layer
#   thresh_layers$county<-names(county_list[item])
#   thresh_list_wi[[item]]<-thresh_layers
#   
# }
# names(thresh_list_wi)<-names_cc
# f<-paste0(cdl_wi_dir,'/thresh_layers')
# for(i in names(thresh_list_wi)){
#   write.csv(thresh_list_wi[[i]], paste0(f,i,".csv"))
#  }
# 
# #####Get NLCD mask for non-crop areas (roadS)
# nlcd<-raster(paste0(nlcd_dir,"/nlcd2019.tif"))
# #nlcd<-raster(paste0(nlcd_dir,"/NLCD_2008_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))
# 
# list_of_nlcd_masks_wi<-list()
# for (county in 1:3){
#   cpaa<-county_list[[county]][[1]] ##get associated counties
#   ext<-extent(cpaa)
#   #plot(cpaa)
#   
#   #use 2008 NLCD to mask out non-crop
#   nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
#   nlcdc<-crop(nlcdc, cpaa)
#   nlcdc<-mask(nlcdc, cpaa)
#   
#   nlcdc[nlcdc==11]<-NA
#   nlcdc[nlcdc==21]<-NA
#   nlcdc[nlcdc==22]<-NA
#   nlcdc[nlcdc==23]<-NA
#   nlcdc[nlcdc==24]<-NA
#   nlcdc[nlcdc==31]<-NA
#   
#   names(nlcdc)<-names(county_list)[[county]][[1]]
#   
#   list_of_nlcd_masks_wi[[county]]<-nlcdc
#   
#    }
# 
# 
# names(list_of_nlcd_masks_wi)<-names_cc
# f<-paste0(cdl_wi_dir, "/mask_output")
# for(layer in 1:length(list_of_nlcd_masks_wi)){
#   writeRaster(list_of_nlcd_masks_wi[[layer]], file.path(f, names(list_of_nlcd_masks_wi[[layer]])), format="GTiff", overwrite = TRUE)
#   }
# 
# }
# 
