### Probabilistic Crop Loading 

### 05 analysis of CPAA

# Edited by E. Paulukonis June 2022

mi_coa<-read.csv(paste0(coa_dir,"/CoA_ILL.csv"))

#sub the years for 2008 to be 2007, the last census prior to CDL
ill_coa$Year[ill_coa$Year ==2007 ] <- 2008
ill_caps<-read.csv(paste0(caps_dir,"/2008_2021_CAPS_ILL.csv"))
ill_caps<-ll_caps[!ll_caps$Year %in% unique(ll_coa$Year),]


#combine caps and coa data
ill_nass<-rbind(ill_coa,ill_caps)
ill_nass<-ill_nass[order(ill_nass$Year),]



cdlkey<-read.csv(paste0(cdl_dir,"/CDL_key.csv")) #cdl key

print(list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE)) #issue is I stuck those tif files in there
print(cdl_mi_dir)
cdl_data_mi <- file.path(cdl_mi_dir, list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_data_mi<-lapply(cdl_data_mi, raster) #create list of cdl rasters 


print(list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(cdl_wi_dir)
cdl_data_wi <- file.path(cdl_wi_dir, list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_data_wi<-lapply(cdl_data_wi, raster) #create list of cdl rasters 

years<-2008:2021


print_thresholds_ill<-matrix(nrow=3,ncol=3,NA)
colnames(print_thresholds_ill)<-c("threshold","county","state")
for(n in 1:length(thresh_list)){
  csv<-thresh_list[[n]]
  n_thresh<-min(csv$bin)
  countyname<-csv[1,9]
  print_thresholds_ill[n,1]<-n_thresh
  print_thresholds_ill[n,2]<-countyname
  print_thresholds_ill[n,3]<-'ILL'
}




#### Get original total crop area ----
average_list<-list()
extracted_cdl_list<-list()
for(layer in 1:length(three_county_list)){
  county<-three_county_list[[layer]]
  output<-output_of_cleaning[[layer]]
  list_freq<-list()
  for(f in 1:length(county)){
    cdl<-county[[f]]
    cdl<-setExtent(cdl,output)
    cdl<-crop(cdl, output)
    cdl<-mask(cdl, output) 
    sq<-as.data.frame(freq(cdl))
    #test<-freq(cdl)
    #writeRaster(cdl, file.path(cdl_dir, "/cdl_mask.tif"), format="GTiff", overwrite = TRUE)
    list_freq[[f]]<-sq
    
  }
  #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
  
  cdl_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
  value<-cdl_crop_areas[,1]
  cdl_crop_areas = cdl_crop_areas[,!names(cdl_crop_areas) == 'value']
  cdl_crop_areas$value<-value
  cdl_crop_areas[,1:14]<-(cdl_crop_areas[,1:14]*900) * 0.000247105 #get area in acres
  names(cdl_crop_areas)<-c(2008:2021,"crop") #make sure names align
  extracted_cdl_list[[layer]]<-cdl_crop_areas
  
}


####First-Pass Analysis ----
#for each layer in y (sets of 14 years for each county), extract the values to the created polygons
crop_list<-list()
extracted_field_list<-list()
for(layer in 1:length(three_county_list)){
  
  field_areas<- as.data.frame(area(output_of_cleaning[[layer]]), na.rm=T) #get areas of each field from the vectorized field
  colnames(field_areas)[1]<-'area'
  colSums(field_areas)*0.000247105
  
  county<-three_county_list[[layer]]
  output<-output_of_cleaning[[layer]]
  
  #plot(county)
  for(f in 1:length(county)){
    extr<-exact_extract(county[[f]],output, "mode") #output is the finalized delineated CPAA
    extr<-as.data.frame(extr)
    names(extr)<-"crops"
    extr$area<-field_areas$area
    extr_f<- extr %>% group_by(crops) %>%   summarise(areaf = (sum(area))*0.000247105)
    #issue: some NAs
    
    fill3=c(NA,0)
    sq<- if (nrow(extr_f) ==2 ){  ##need to fix the order because of the way total crop area is
      rbind(extr_f,fill3)
    } else if (nrow(extr_f) == 1){
      rbind(extr_f,fill2,fill3)
    } else{
      extr_f
    }
    crop_list[[f]]<-sq
  }
  
  #this is the extracted field areas
  field_crop_areas<-do.call(cbind, lapply(crop_list, as.data.frame)) ##6.1.22 issues with no NAs for layer 3
  value<-field_crop_areas[,1]
  field_crop_areas = field_crop_areas[,!names(field_crop_areas) == 'crops']
  field_crop_areas$value<-value
  names(field_crop_areas)<-c(2008:2021,"crop") #make sure names align
  extracted_field_list[[layer]]<-field_crop_areas
}




####Total CDL crop area vs total field crop area

field_crop_areas<-extracted_field_list[[3]] # pull out individual counties here
field_crop_total<-colSums(field_crop_areas[1:2,1:14])

#get cdl areas
cdl_crop_areas<-extracted_cdl_list[[3]]
cdl_crop<-colSums(cdl_crop_areas[1:2,1:14])
print(field_crop_total) #total area of all crops contained within estimated fields
print(cdl_crop) #total area of all CDL crops by year

total_diff<-((field_crop_total - cdl_crop)/cdl_crop)*100 #by how much more or less do we predict field crop coverage than the raw CDL?
print(total_diff) 

ratio_diff<-(field_crop_total/cdl_crop) #what fold increase/decrease?
print(ratio_diff) #increase/decrease of crop field area to cdl area

write.csv(ratio_diff,paste0(root_data_out, "/high_ratio.csv"), row.names = T)


