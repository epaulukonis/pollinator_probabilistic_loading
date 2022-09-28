### Probabilistic Crop Loading 

### 03 evaluating crop diversity and getting CPAA threshold

# Edited by E. Paulukonis June 2022
import_start_time <- Sys.time()
print("stepping into 03_CPAA_threshold_c.R")

##### COUNTY LAYER PREP
options(scipen = 999) #remove exponent options, throws R off

###NOTE: order of counties is very important; it's crucial to test that you have the correct county by visual aid now and again.

#### Illinois ----
thresh_ill_filename<-paste0(root_data_out, "/all_thresh/Illinois/DuPage2020.csv")
if(file.exists(thresh_ill_filename)){
  
    print(list.files(path=paste0(root_data_out,"/all_thresh/Illinois"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
    thresh_ill<- file.path(paste0(root_data_out,"/all_thresh/Illinois"), list.files(path=paste0(root_data_out,"/all_thresh/Illinois"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
    thresh_list_ill<-setNames(lapply(thresh_ill, read.csv), tools::file_path_sans_ext(basename(thresh_ill)))
    thresh_list_ill<-lapply(thresh_list_ill, function(y) { y["X"] <- NULL; y })
    
    # 
    thresh_list_ill_f<-list()
    thresh_list_ill_f[[1]]<-thresh_list_ill[1:14]
    thresh_list_ill_f[[2]]<-thresh_list_ill[15:28]
    thresh_list_ill_f[[3]]<-thresh_list_ill[29:42]
    # 
    # 
    # #test block
    # thresh_list_ill_f<-list()
    # thresh_list_ill_f[[1]]<-thresh_list_ill[1:2]
    # thresh_list_ill_f[[2]]<-thresh_list_ill[3:4]
    # thresh_list_ill_f[[3]]<-thresh_list_ill[5:6]
    # 
    # 
    f<-paste0(root_data_out, "/all_NLCD/Illinois")
    print(list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
    nlcd_ill<- file.path(f, list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
    nlcd_ill<-setNames(lapply(nlcd_ill, raster), tools::file_path_sans_ext(basename(nlcd_ill)))
    
}else{
study<-ill
sub_group<-c("DuPage","McHenry","Champaign")
sub<-study[study$NAME %in% sub_group,]

names_county<-list()
county_set_list<-list()
  for (co in 1:length(sub)){
    co_r<-sub[sub$NAME == sub$NAME[co],]
    mask_crop<-function(x){
      r_list<-crop(x, co_r)
      mask(r_list, co_r) 
      }
    
    county_set<-lapply(cdl_data_ill_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
    county_set_list[[co]]<-stack(county_set) #represents single county stack
    names_county[co]<-co_r$NAME
    
    }


names(county_set_list)<-unlist(names_county)

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
    
    layer_list<-layer_list[-c(1:9)] ##note, this applies ONLY to Illinois, with 23 years total
    county_list[[county]]<-layer_list
    }

names(county_list)<-names_cc
years<-2008:2021

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
    
    year_name<-years[year]
    
    f<-paste0(root_data_out,'/all_thresh/Illinois/')
    write.csv(thresh_layers, paste0(f,names(county_list)[item],year_name,".csv"))
    
    write.csv(thresh_layers, paste0(f,names(county_list)[item],year_name,"test.csv"))
      }
    
   #thresh_list_by_county_ill[[county]]<-thresh_list_by_year_ill #this sticks the list of 14 threshold datasets into a list by county
    }


# # #####Get NLCD mask for non-crop areas (roadS)
#   for(county_layer in 1:length(county_list)){
#     county_layer=3
#     cpaa<-county_list[[county_layer]][[1]] ##get associated counties
#     ext<-extent(cpaa)
# 
#     list_of_nlcds<-list()
#     for(nlcd_layer in 1:length(nlcd_all)){
#     nlcd<-nlcd_all[[nlcd_layer]]
#     nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
#     nlcdc<-crop(nlcdc, cpaa)
#     nlcdc<-mask(nlcdc, cpaa)
# 
#     nlcdc[nlcdc==11]<-NA
#     nlcdc[nlcdc==21]<-NA
#     nlcdc[nlcdc==22]<-NA
#     nlcdc[nlcdc==23]<-NA
#     nlcdc[nlcdc==24]<-NA
#     nlcdc[nlcdc==31]<-NA
# 
#     names(nlcdc)<-names(nlcd_all[nlcd_layer])
#     f<-paste0(root_data_out, "/all_NLCD/Illinois")
#     writeRaster(nlcdc, file.path(f, paste0(names_cc[[county_layer]],"_",names(nlcdc))), format="GTiff", overwrite = TRUE)
# 
#     }
# 
#   }
# 



}





##### Michigan ----
thresh_mi_filename<-paste0(root_data_out, "/all_thresh/Michigan/VanBuren2008.csv")
if(file.exists(thresh_mi_filename)){

  print(list.files(path=paste0(root_data_out,"/all_thresh/Michigan"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
  thresh_mi<- file.path(paste0(root_data_out,"/all_thresh/Michigan"), list.files(path=paste0(root_data_out,"/all_thresh/Michigan"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
  thresh_list_mi<-setNames(lapply(thresh_mi, read.csv), tools::file_path_sans_ext(basename(thresh_mi)))
  thresh_list_mi<-lapply(thresh_list_ill, function(y) { y["X"] <- NULL; y })
  
  thresh_list_mi_f<-list()
  thresh_list_mi_f[[1]]<-thresh_list_mi[1:14]
  thresh_list_mi_f[[2]]<-thresh_list_mi[15:28]
  thresh_list_mi_f[[3]]<-thresh_list_mi[29:42]


  f<-paste0(root_data_out, "/all_NLCD/Michigan")
  print(list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_mi<- file.path(f, list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_mi<-setNames(lapply(nlcd_mi, raster), tools::file_path_sans_ext(basename(nlcd_mi)))
  
}else{
  study<-mi
  sub_group<-c("Van Buren", "Oceana","Huron")
  sub<-study[study$NAME %in% sub_group,]
  
  names_county<-list()
  county_set_list<-list()
  for (co in 1:length(sub)){
    co_r<-sub[sub$NAME == sub$NAME[co],]
    mask_crop<-function(x){
      r_list<-crop(x, co_r)
      mask(r_list, co_r)
    }
    county_set<-lapply(cdl_data_mi_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
    county_set_list[[co]]<-stack(county_set) #represents single county stack
    names_county[co]<-co_r$NAME
  }
  
  
  names(county_set_list)<-unlist(names_county)
  hu75<-county_set_list$Huron #75% crop coverage
  van35<-county_set_list$`Van Buren` #50%
  oc25<-county_set_list$Oceana #25%
  
  chosen_count<-list(van35,oc25,hu75)
  names_cc<-c("VanBuren", "Oceana","Huron")
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
    
    county_list[[county]]<-layer_list
  }
  
  names(county_list)<-names_cc
  years<-2008:2021
  
  ##### CALCULATE THRESHOLD 
  thresh_list_by_county_mi<-list() #contains all the datasets by county
  thresh_list_by_year_mi<-list() #contains all the datasets by yar
  
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
      thresh_list_by_year_mi[[year]]<-thresh_layers
    
      year_name<-years[year]
      
      f<-paste0(root_data_out,'/all_thresh/Michigan/')
      write.csv(thresh_layers, paste0(f,names(county_list)[item],year_name,".csv"))
    }
    
    # thresh_list_by_county_mi[[county]]<-thresh_list_by_year_mi #this sticks the list of 14 threshold datasets into a list by county
  }
  
  
  # #####Get NLCD mask for non-crop areas (roadS)
  # for(county_layer in 1:length(county_list)){
  #   cpaa<-county_list[[county_layer]][[1]] ##get associated counties
  #   ext<-extent(cpaa)
  # 
  #   list_of_nlcds<-list()
  #   for(nlcd_layer in 1:length(nlcd_all)){
  #     nlcd<-nlcd_all[[nlcd_layer]]
  #     nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
  #     nlcdc<-crop(nlcdc, cpaa)
  #     nlcdc<-mask(nlcdc, cpaa)
  # 
  #     nlcdc[nlcdc==11]<-NA
  #     nlcdc[nlcdc==21]<-NA
  #     nlcdc[nlcdc==22]<-NA
  #     nlcdc[nlcdc==23]<-NA
  #     nlcdc[nlcdc==24]<-NA
  #     nlcdc[nlcdc==31]<-NA
  # 
  #     names(nlcdc)<-names(nlcd_all[nlcd_layer])
  #     f<-paste0(root_data_out, "/all_NLCD/Michigan")
  #     writeRaster(nlcdc, file.path(f, paste0(names_cc[[county_layer]],"_",names(nlcdc))), format="GTiff", overwrite = TRUE)
  # 
  #   }
  # 
  # }

  

}



##### Wisconsin ----
thresh_wi_filename<-paste0(root_data_out, "/all_thresh/Wisconsin/Waushara2008.csv")
if(file.exists(thresh_wi_filename)){
  
  print(list.files(path=paste0(root_data_out,"/all_thresh/Wisconsin"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
  thresh_wi<- file.path(paste0(root_data_out,"/all_thresh/Wisconsin"), list.files(path=paste0(root_data_out,"/all_thresh/Wisconsin"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
  thresh_list_wi<-lapply(thresh_wi, read.csv)
  thresh_list_wi<-lapply(thresh_list_wi, function(y) { y["X"] <- NULL; y })
  
  thresh_list_wi_f<-list()
  thresh_list_wi_f[[1]]<-thresh_list_wi[1:14]
  thresh_list_wi_f[[2]]<-thresh_list_wi[15:28]
  thresh_list_wi_f[[3]]<-thresh_list_wi[29:42]
  
  f<-paste0(root_data_out, "/all_NLCD/Wisconsin")
  print(list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_wi<- file.path(f, list.files(path=f, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_wi<-lapply(nlcd_wi, raster)

  
}else{
  study<-wi
  sub_group<-c("Waushara","Langlade","Rock")
  sub<-study[study$NAME %in% sub_group,]
  
  names_county<-list()
  county_set_list<-list()
  for (co in 1:length(sub)){
    co_r<-sub[sub$NAME == sub$NAME[co],]
    mask_crop<-function(x){
      r_list<-crop(x, co_r)
      mask(r_list, co_r)
    }
    county_set<-lapply(cdl_data_wi_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
    county_set_list[[co]]<-stack(county_set) #represents single county stack
    names_county[co]<-co_r$NAME
  }
  
  names(county_set_list)<-unlist(names_county)
  rock65<-county_set_list$Rock#65% crop coverage
  wau35<-county_set_list$Waushara #35%
  lang15<-county_set_list$Langlade #10-15%
  
  chosen_count<-list(wau35, lang15, rock65)
  names_cc<-c("Waushara","Langlade","Rock")
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
    
    county_list[[county]]<-layer_list
  }
  
  names(county_list)<-names_cc
  years<-2008:2021
  
  ##### CALCULATE THRESHOLD 
  thresh_list_by_county_wi<-list() #contains all the datasets by county
  thresh_list_by_year_wi<-list() #contains all the datasets by yar
  
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
      thresh_list_by_year_wi[[year]]<-thresh_layers
      
      year_name<-years[year]
      
      f<-paste0(root_data_out,'/all_thresh/Wisconsin/')
      write.csv(thresh_layers, paste0(f,names(county_list)[item],year_name,".csv"))
      
    }
    
    
    #thresh_list_by_county_wi[[county]]<-thresh_list_by_year_wi #this sticks the list of 14 threshold datasets into a list by county
  }
  
  
  # ####Get NLCD mask for non-crop areas (roadS)
  # for(county_layer in 1:length(county_list)){
  #   cpaa<-county_list[[county_layer]][[1]] ##get associated counties
  #   ext<-extent(cpaa)
  # 
  #   list_of_nlcds<-list()
  #   for(nlcd_layer in 1:length(nlcd_all)){
  #     nlcd<-nlcd_all[[nlcd_layer]]
  #     nlcdc<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
  #     nlcdc<-crop(nlcdc, cpaa)
  #     nlcdc<-mask(nlcdc, cpaa)
  # 
  #     nlcdc[nlcdc==11]<-NA
  #     nlcdc[nlcdc==21]<-NA
  #     nlcdc[nlcdc==22]<-NA
  #     nlcdc[nlcdc==23]<-NA
  #     nlcdc[nlcdc==24]<-NA
  #     nlcdc[nlcdc==31]<-NA
  # 
  #     names(nlcdc)<-names(nlcd_all[nlcd_layer])
  #     f<-paste0(root_data_out, "/all_NLCD/Wisconsin")
  #     writeRaster(nlcdc, file.path(f, paste0(names_cc[[county_layer]],"_",names(nlcdc))), format="GTiff", overwrite = TRUE)
  # 
  #   }
  # 
  # }
  
}
