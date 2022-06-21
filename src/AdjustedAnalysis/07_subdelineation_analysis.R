### Probabilistic Crop Loading 

### 07 sub-delineation analysis

# Edited by E. Paulukonis June 2022

#### Prepare the CDL layers and the 3 county CPAA layers----
#50 8% DUPAGE low
#1 40% McHENRY medium
#14 72% CHAMPAIGN high



####Second-pass Analysis----

#### Field areas extraction
field_areass<- as.data.frame(area(sub_del), na.rm=T) #get areas of each field from the vectorized field
colnames(field_areass)[1]<-'area'
#field_areas$area<-field_areas


#for each layer in y (sets of 11 years for each county), extract the values to the created polygons
crop_list<-list()
y<-county_rec_list[[2]]
for(layer in 1:length(y)){
  output<-exact_extract(y[[layer]], sub_del, "mode")
  output<-as.data.frame(output)
  names(output)<-"crops"
  output$area<-field_areass$area
  output<- output %>% group_by(crops) %>%   summarise(area = sum(area)*0.000247105) #convert to acres
  crop_list[[layer]]<-output
}

names(crop_list)<-1999:2009

#now we pull out outliers (i..e, years where we may have 0 acres)
outliers<-Filter(function(x) nrow(x) <3, crop_list)
main<-Filter(function(x) nrow(x) >=3, crop_list)
field1<-do.call(cbind, lapply(outliers, as.data.frame))
field2<-do.call(cbind, lapply(main, as.data.frame))
filler=c(3,0)
field1[3,]<-filler
field1<-field1[c(1:2,3),]

value<-field1[,1]
field_crop_areass<-cbind(field1,field2)
field_crop_areass =  field_crop_areass %>% select(-contains(".crops"))
field_crop_areass$value<-value
field_crop_areass<-field_crop_areass[ ,order(names(field_crop_areass))]

field_crop_areass<-field_crop_areass[,c(12,1:11)] 
names(field_crop_areass)<-c("crop",1999:2009) #make sure names align


#### CDL areas computation
average_list<-list()
extracted_cdl_list<-list()
for(layer in 1:length(county_rec_list)){
  layer=2
  county<-county_rec_list[[layer]]
  output<-sub_del
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
  names(list_freq)<-1999:2009
  
  outliers<-Filter(function(x) nrow(x) <5, list_freq)
  main<-Filter(function(x) nrow(x) >=5, list_freq)
  field1<-do.call(cbind, lapply(outliers, as.data.frame))
  field2<-do.call(cbind, lapply(main, as.data.frame))
  filler=c(3,0)
  field1[5,]<-filler
  field1<-field1[c(1:3,5,4),]
  
  value<-field1[,1]
  cdl_crop_areass<-cbind(field1,field2)
  cdl_crop_areass =  cdl_crop_areass %>% select(-contains(".value"))
  cdl_crop_areass$value<-value
  cdl_crop_areass<-cdl_crop_areass[ ,order(names(cdl_crop_areass))]
  cdl_crop_areass[,1:11]<-(cdl_crop_areass[,1:11]*900) * 0.000247105 #get area in acres
  cdl_crop_areass<-cdl_crop_areass[,c(12,1:11)] 
  names(cdl_crop_areass)<-c("crop",1999:2009) #make sure names align
  
  extracted_cdl_list[[layer]]<-cdl_crop_areass
  
}

#percent difference 
error<-((field_crop_areass[,c(2:12)] - cdl_crop_areass[c(2:4),c(2:12)])/cdl_crop_areass[c(2:4),c(2:12)])*100
error$crop<-c("corn","soy","ww")
print(error)

field<-colSums(field_crop_areass[,c(2:12)])
cdl<-colSums(cdl_crop_areass[c(2:4),c(2:12)])

output_ratio<-field/cdl #ratio of total field 'pesticide' area to cdl area by year
write.csv(output_ratio,paste0(root_data_out, "med_sub_ratio.csv"), row.names = T)
write.csv(error,paste0(root_data_out, "med_sub_crop.csv"), row.names = T)

####Other metrics----
###Crop vs. Non-crop
cdl_crop<-colSums(cdl_crop_areas[1:2,1:11])
cdl_noncrop<-cdl_crop_areas[3,1:11]

field_crop<-colSums(field_crop_areas[1:2,1:11])
field_noncrop<-field_crop_areas[3,1:11]

crop_diff<-((field_crop - cdl_crop)/cdl_crop)*100 
print(crop_diff) #


