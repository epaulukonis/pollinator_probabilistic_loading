### Probabilistic Crop Loading 

### 05 analysis of CPAA

# Edited by E. Paulukonis August 2022


#### Read in CoA and OG CDL
ill_coa<-read.csv(paste0(coa_dir,"/CoA_ILL.csv"))
mi_coa<-read.csv(paste0(coa_dir,"/CoA_MI.csv"))
wi_coa<-read.csv(paste0(coa_dir,"/CoA_WIS.csv"))


#sub the years for 2008 to be 2007, the last census prior to CDL
ill_coa$Year[ill_coa$Year ==2007 ] <- 2008
mi_coa$Year[mi_coa$Year ==2007 ] <- 2008
wi_coa$Year[wi_coa$Year ==2007 ] <- 2008

ill_caps<-read.csv(paste0(caps_dir,"/2008_2021_CAPS_ill.csv"))
mi_caps<-read.csv(paste0(caps_dir,"/2008_2021_CAPS_MI.csv"))
wi_caps<-read.csv(paste0(caps_dir,"/2008_2021_CAPS_WIS.csv"))

ill_caps<-ill_caps[!ill_caps$Year %in% unique(ill_coa$Year),]
mi_caps<-mi_caps[!mi_caps$Year %in% unique(mi_coa$Year),]
wi_caps<-mi_caps[!wi_caps$Year %in% unique(wi_coa$Year),]





#combine caps and coa data
ill_nass<-rbind(ill_coa,ill_caps)
mi_nass<-rbind(mi_coa,mi_caps)
wi_nass<-rbind(wi_coa,wi_caps)

ill_nass<-ill_nass[order(ill_nass$Year),]
mi_nass<-mi_nass[order(mi_nass$Year),]
wi_nass<-wi_nass[order(wi_nass$Year),]

# mi_nass<-mi_nass[!mi_nass$Year==2007,]
# wi_nass<-mi_nass[!mi_nass$Year==2007,]

cdlkey<-read.csv(paste0(cdl_dir,"/CDL_key.csv")) #cdl key




# print(list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE)) #issue is I stuck those tif files in there
# print(cdl_mi_dir)
# cdl_data_mi <- file.path(cdl_mi_dir, list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
# cdl_data_mi<-lapply(cdl_data_mi, raster) #create list of cdl rasters 
# 
# 
# print(list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE)) #issue is I stuck those tif files in there
# print(cdl_mi_dir)
# cdl_data_mi <- file.path(cdl_mi_dir, list.files(path=cdl_mi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
# cdl_data_mi<-lapply(cdl_data_mi, raster) #create list of cdl rasters 
# 
# 
# print(list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
# print(cdl_wi_dir)
# cdl_data_wi <- file.path(cdl_wi_dir, list.files(path=cdl_wi_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
# cdl_data_wi<-lapply(cdl_data_wi, raster) #create list of cdl rasters 


print(list.files(path=paste0(cdl_dir,"/Illinois"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
avg_thresh_ill<- file.path(paste0(cdl_dir,"/Illinois"), list.files(path=paste0(cdl_dir,"/Illinois"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
avg_thresh_ill<-setNames(lapply(avg_thresh_ill, read.csv), tools::file_path_sans_ext(basename(avg_thresh_ill)))
avg_thresh_ill<-lapply(avg_thresh_ill, function(y) { y["X"] <- NULL; y })


print(list.files(path=paste0(cdl_mi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
avg_thresh_mi<- file.path(paste0(cdl_mi_dir,'/thresh_layers'), list.files(path=paste0(cdl_mi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
avg_thresh_mi<-setNames(lapply(avg_thresh_mi, read.csv), tools::file_path_sans_ext(basename(avg_thresh_mi)))
avg_thresh_mi<-lapply(avg_thresh_mi, function(y) { y["X"] <- NULL; y })


print(list.files(path=paste0(cdl_wi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
avg_thresh_wi<- file.path(paste0(cdl_wi_dir,'/thresh_layers'), list.files(path=paste0(cdl_wi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
avg_thresh_wi<-setNames(lapply(avg_thresh_wi, read.csv), tools::file_path_sans_ext(basename(avg_thresh_wi)))
avg_thresh_wi<-lapply(avg_thresh_wi, function(y) { y["X"] <- NULL; y })



#### first, let's get the actual threshold cutoffs for each year in the singular year data
print_thresholds_ill<-matrix(nrow=3,ncol=3,NA)
colnames(print_thresholds_ill)<-c("threshold","county","state")

print_thresholds_mi<-matrix(nrow=3,ncol=3,NA)
colnames(print_thresholds_mi)<-c("threshold","county","state")

print_thresholds_wi<-matrix(nrow=3,ncol=3,NA)
colnames(print_thresholds_wi)<-c("threshold","county","state")

names_counties<-c("DuPage","McHenry","Champaign")

for(n in 1:length(avg_thresh_ill)){
  csv<-avg_thresh_ill[[n]]
  n_thresh<-min(csv$bin)
  csv$countyname<-names_counties[n]
  countyname<-csv[1,9]
  print_thresholds_ill[n,1]<-n_thresh
  print_thresholds_ill[n,2]<-countyname
  print_thresholds_ill[n,3]<-'ILL'
}

for(n in 1:length(avg_thresh_mi)){
  csv<-avg_thresh_mi[[n]]
  n_thresh<-min(csv$bin)
  countyname<-csv[1,9]
  print_thresholds_mi[n,1]<-n_thresh
  print_thresholds_mi[n,2]<-countyname
  print_thresholds_mi[n,3]<-'MI'
}


for(n in 1:length(avg_thresh_wi)){
  csv<-avg_thresh_wi[[n]]
  n_thresh<-min(csv$bin)
  countyname<-csv[1,9]
  print_thresholds_wi[n,1]<-n_thresh
  print_thresholds_wi[n,2]<-countyname
  print_thresholds_wi[n,3]<-'WI'
}

print_thresh<-as.data.frame(rbind(print_thresholds_ill, print_thresholds_mi,print_thresholds_wi))
print_thresh


years<-2008:2021
#### Extract CDL to new field data by county; this is just to see how much area we initially capture 

#### Illinois ----
list_cdl_extracts<-list()
list_of_ill_counties<-list()
# field_areas_ill<-list()
for(county in 1:length(cpaa_list_ill_f)){
  
  county=1
  county_layers<-cpaa_list_ill_f[[county]]

  for(f in 1:length(cdl_data_ill_rec)){
    f=1
    cdl<-cdl_data_ill_rec[[f]]
    cdl<-exact_extract(cdl,county_layers[[f]])
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
  #names(list_cdl_extracts)<-years
  names(list_cdl_extracts)<-2008
  list_of_ill_counties[[county]]<-list_cdl_extracts
  names(list_of_ill_counties)[county]<-names(cpaa_list_ill_f[county])
  # field_areas_ill[[county]]<-as.data.frame(area(county_layers[[f]])*0.000247105, na.rm=T) #convert m2 to acres, in case we need field reference values
}




list_of_ratio_by_county_and_year<-list()
for(n in 1:length(list_of_ill_counties)){
  n=1
  county<-list_of_ill_counties[[n]]
  county_name<-toupper(names(list_of_ill_counties[n]))
  
  list_of_ratios<-list()
  for(y in 1:length(county)){
    y=1
    county_by_year<-county[[y]]
    county_by_year$year<-as.numeric(names(county[y]))
    county_by_year$Category<-  toupper(county_by_year$Category) 
    
    ill_nass<-ill_nass[!ill_nass$Value == " (D)",]
    ill_nass$Value<-as.numeric(as.numeric(gsub(",", "", ill_nass$Value)))
    
    ill_nass_y<- ill_nass %>%
      filter(County %in% county_name) %>%
      filter(Year %in% c(county_by_year$year)) %>%
      filter_at(vars(starts_with("Data.Item")), all_vars(grepl('- ACRES HARVESTED', .)))%>%
      group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
    
    names(county_by_year)[3]<-"Commodity"
    county_by_year_crops<-inner_join(county_by_year, ill_nass_y, by = "Commodity")
    colnames(county_by_year_crops)[2]<-"fieldacres"
    colnames(county_by_year_crops)[7]<-"NASSacres"
    county_by_year_crops$ratio<-county_by_year_crops$NASSacres/county_by_year_crops$fieldacres
    #county_by_year_crops$County<-county_name
    list_of_ratios[[y]]<-county_by_year_crops
  
    
  }
  # names(list_of_ratios)<-years
  list_of_ratio_by_county_and_year[[n]]<-list_of_ratios
}

list_of_ratio_by_county_and_year


testy<-list_of_ratio_by_county_and_year[[1]][[1]]
testy$Year<-as.character(testy$Year)
testy<-do.call(rbind,test)


ratio_plot<-ggplot(testy, aes(x=Commodity, y=(ratio), group= Year, fill=Year, colour=Year)) + 
  geom_point()+
  xlab("Crop") + 
  ylab("log(Ratio)")+
  labs(title = "Ratio of COA to Extracted Field Acreages")+
  expand_limits(y=0)+
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ratio_plot











#### Michigan ----
list_cdl_extracts<-list()
list_of_mi_counties<-list()
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
  list_of_mi_counties[[layer]]<-list_cdl_extracts
  field_areas[[layer]]<-as.data.frame(area(county)*0.000247105, na.rm=T) #convert m2 to acres
  }

# field_areas<-lapply(field_areas, function(x) x*0.000247105)

names(list_of_mi_counties)<-names(mi_fields)


list_of_ratio_by_county_and_year<-list()
for(n in 1:length(list_of_mi_counties)){
  county<-list_of_mi_counties[[n]]
  county_name<-names(list_of_mi_counties[n])
  names(county)<-years
  
  list_of_ratios<-list()
  for(y in 1:length(county)){
    county_by_year<-county[[y]]
    county_by_year$year<-as.numeric(names(county[y]))
    county_by_year$Category<-  toupper(county_by_year$Category) 
    
    mi_nass<-mi_nass[!mi_nass$Value == " (D)",]
    mi_nass$Value<-as.numeric(as.numeric(gsub(",", "", mi_nass$Value)))
    
    mi_nass_y<- mi_nass %>%
      filter(County %in% county_name) %>%
      filter(Year %in% c(county_by_year$year)) %>%
      filter_at(vars(starts_with("Data.Item")), all_vars(grepl('- ACRES HARVESTED', .)))%>%
      group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
     
      names(county_by_year)[3]<-"Commodity"
      county_by_year_crops<-inner_join(county_by_year, mi_nass_y, by = "Commodity")
      county_by_year_crops$ratio<-county_by_year_crops$sum/county_by_year_crops$x
      colnames(county_by_year_crops)[2]<-"field acres"
      colnames(county_by_year_crops)[7]<-"NASS acres"
      #county_by_year_crops$County<-county_name
      
    list_of_ratios[[y]]<-county_by_year_crops
    
  }
  names(list_of_ratios)<-years
  list_of_ratio_by_county_and_year[[n]]<-list_of_ratios
}

names(list_of_ratio_by_county_and_year)<-names(list_of_mi_counties)


test<-list_of_ratio_by_county_and_year[[1]]
testy$Year<-as.character(testy$Year)
testy<-do.call(rbind,test)


ratio_plot<-ggplot(testy, aes(x=Commodity, y=(ratio), group= Year, fill=Year, colour=Year)) + 
  geom_point()+
  xlab("Crop") + 
  ylab("log(Ratio)")+
  labs(title = "Ratio of Deterministic Crop Area to Simulated Field Probabilistic Crop Areas within 1km VP CH")+
  expand_limits(y=0)+
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ratio_plot







#### Wisconsin ----
list_cdl_extracts<-list()
list_of_wi_counties<-list()
field_areas<-list()
for(layer in 1:length(wi_fields)){
  county<-wi_fields[[layer]]
  for(f in 1:length(cdl_data_wi)){
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
  list_of_wi_counties[[layer]]<-list_cdl_extracts
  field_areas[[layer]]<-as.data.frame(area(county)*0.000247105, na.rm=T) #convert m2 to acres
}

names(list_of_wi_counties)<-names(wi_fields)





