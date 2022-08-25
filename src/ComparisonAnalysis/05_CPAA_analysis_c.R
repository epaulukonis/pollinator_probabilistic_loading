### Probabilistic Crop Loading 

### 05 analysis of CPAA

# Edited by E. Paulukonis June 2022

mi_fields<-list() #order by alphabetical name to match threshold
mi_fields[1]<- readOGR(paste0(cdl_mi_dir,"/CPAA"), layer = "huron75")
mi_fields[2]<- readOGR(paste0(cdl_mi_dir,"/CPAA"), layer = "oceana25")
mi_fields[3]<- readOGR(paste0(cdl_mi_dir,"/CPAA"), layer = "vanburen50")

names(mi_fields)<-c("huron75","ottaway25","vanburen30")

wi_fields<-list()
wi_fields[1]<- readOGR(paste0(cdl_wi_dir,"/CPAA"), layer = "lang15")
wi_fields[2]<- readOGR(paste0(cdl_wi_dir,"/CPAA"), layer = "rock65")
wi_fields[3]<- readOGR(paste0(cdl_wi_dir,"/CPAA"), layer = "wau35")

names(wi_fields)<-c("lang15","rock65","wau35")

#### Read in CoA and OG CDL

mi_coa<-read.csv(paste0(coa_dir,"/CoA_MI.csv"))
wi_coa<-read.csv(paste0(coa_dir,"/CoA_WIS.csv"))

cdlkey<-read.csv(paste0(cdl_dir,"/CDL_key.csv")) #cdl key

print(list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(cdl_mi_dir)
cdl_data_mi <- file.path(cdl_mi_dir, list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_data_mi<-lapply(cdl_data_mi, raster) #create list of cdl rasters 


print(list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(cdl_wi_dir)
cdl_data_wi <- file.path(cdl_wi_dir, list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_data_wi<-lapply(cdl_data_wi, raster) #create list of cdl rasters 



#### Extract CDL to new field data by county; this is just to see how much area we capture 
list_cdl_extracts<-list()
list_of_counties<-list()
field_areas<-list()
for(layer in 1:length(mi_fields)){
  county<-mi_fields[[layer]]
  for(f in 1:length(cdl_data_mi)){
    cdl<-cdl_data_mi_rec[[f]]
    cdl<-exact_extract(cdl,county)
    names(cdl)<-1:length(cdl)

   cdl_ext<-do.call("rbind", cdl)
   cdl_ext$polygon<-as.numeric(row.names(cdl_ext))
   cdl_ext$polygon<-trunc(cdl_ext$polygon)
   cdl_ext$area<-(cdl_ext$coverage_fraction*900)*0.000247105 #convert to acres

   cropareas<-aggregate(cdl_ext$area, by=list(Polygon=cdl_ext$polygon, crop=cdl_ext$value), FUN=sum)
   
   testx<-cropareas[cropareas$Polygon==1,]
   crop_total<-aggregate(cropareas$x, by=list(Class=cropareas$crop), FUN=sum)
   crop_final<-left_join(crop_total, cdlkey, by="Class")
   crop_final$Category<-  toupper(crop_final$Category) 
    list_cdl_extracts[[f]]<-crop_final
    
  }
  list_of_counties[[layer]]<-list_cdl_extracts
  field_areas[[layer]]<-as.data.frame(area(county)*0.000247105, na.rm=T) #convert m2 to acres
  }

# field_areas<-lapply(field_areas, function(x) x*0.000247105)


crop_final<-list_cdl_extracts[[10]]


crop_final$Category<-  toupper(crop_final$Category) 
test<-mi_coa %>%
  filter_at(vars(starts_with("Data.Item")), all_vars(grepl('ACRES HARVESTED', .)))


testy<-test[test$Commodity %in% crop_final$Category & test$County == "VAN BUREN",]








years<-2008:2021



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


