### Probabilistic Crop Loading 

### 05 analysis of CPAA

# Edited by E. Paulukonis June 2022

ill_coa<-read.csv(paste0(coa_dir,"/CoA_ILL.csv"))
#sub the years for 2008 to be 2007, the last census prior to CDL
ill_coa$Year[ill_coa$Year ==2007 ] <- 2008

ill_caps<-read.csv(paste0(caps_dir,"/2008_2021_CAPS_ILL.csv"))
ill_caps<-ill_caps[!ill_caps$Year %in% unique(ill_coa$Year),]


ill_caps$Commodity[which(ill_caps$Commodity == "WHEAT")] = "WINTER WHEAT"
ill_coa$Commodity[which(ill_coa$Commodity == "WHEAT")] = "WINTER WHEAT"

#combine caps and coa data
ill_nass<-rbind(ill_coa,ill_caps)
ill_nass<-ill_nass[order(ill_nass$Year),]


cdlkey<-read.csv(paste0(cdl_dir,"/CDL_key.csv")) #cdl key


cdl_data_ill<-cdl_data_ill_rec[c(10:23)]

years<-2008:2021

names(thresh_list)<-c("DuPage","McHenry","Champaign") #low, medium, high

print_thresholds_ill<-matrix(nrow=3,ncol=3,NA)
colnames(print_thresholds_ill)<-c("threshold","county","state")
for(n in 1:length(thresh_list)){
  csv<-thresh_list[[n]]
  n_thresh<-min(csv$bin)
  countyname<-names(thresh_list)[[n]]
  print_thresholds_ill[n,1]<-n_thresh
  print_thresholds_ill[n,2]<-countyname
  print_thresholds_ill[n,3]<-'ILL'
}

print_thresholds_ill


list_cdl_extracts<-list()
list_of_ill_counties<-list()
field_areas<-list()
for(layer in 1:length(ill_fields)){
  county<-ill_fields[[layer]]
  for(f in 1:length(cdl_data_ill)){
    cdl<-cdl_data_ill[[f]]
    cdl<-exact_extract(cdl,county)
    names(cdl)<-1:length(cdl)
    
    cdl_ext<-do.call("rbind", cdl)
    cdl_ext$polygon<-as.numeric(row.names(cdl_ext))
    cdl_ext$polygon<-trunc(cdl_ext$polygon)
    
    cdl_ext$area<-(cdl_ext$coverage_fraction*900)*0.000247105 #convert to acres
    cropareas<-aggregate(cdl_ext$area, by=list(Polygon=cdl_ext$polygon, crop=cdl_ext$value), FUN=sum)
   # testx<-cropareas[cropareas$Polygon==1,]
    crop_total<-aggregate(cropareas$x, by=list(Class=cropareas$crop), FUN=sum)
    crop_final<-left_join(crop_total, cdlkey, by="Class")
    crop_final$Category<-  toupper(crop_final$Category) 
    list_cdl_extracts[[f]]<-crop_final
    
  }
  list_of_ill_counties[[layer]]<-list_cdl_extracts
  field_areas[[layer]]<-as.data.frame(area(county)*0.000247105, na.rm=T) #convert m2 to acres
}

names(list_of_ill_counties)<-names(ill_fields)



list_of_ratio_by_county_and_year<-list()
for(n in 1:length(list_of_ill_counties)){
  county<-list_of_ill_counties[[n]]
  county_name<-names(list_of_ill_counties[n])
  names(county)<-years
  
  list_of_ratios<-list()
  for(y in 1:length(county)){
    
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
    county_by_year_crops$ratio<-county_by_year_crops$sum/county_by_year_crops$x
    colnames(county_by_year_crops)[2]<-"field acres"
    colnames(county_by_year_crops)[7]<-"NASS acres"
    #county_by_year_crops$County<-county_name
    
    list_of_ratios[[y]]<-county_by_year_crops
    
  }
  names(list_of_ratios)<-years
  list_of_ratio_by_county_and_year[[n]]<-list_of_ratios
}

names(list_of_ratio_by_county_and_year)<-names(list_of_ill_counties)


list_plots<-list()
for(county in 1:length(list_of_ratio_by_county_and_year)){
  data<-list_of_ratio_by_county_and_year[[county]]
  datay<-do.call(rbind,data)
  datay$Year<-as.character(datay$Year)
  ratio_plot<-ggplot(datay, aes(x=Commodity, y=(ratio), fill=Commodity)) + 
    geom_boxplot()+
    xlab("Crop") + 
    ylab("Ratio")+
    labs(title =names(list_of_ill_counties)[[county]])+
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ratio_plot
  
  list_plots[[county]]<-ratio_plot
  
}

do.call("grid.arrange", c(list_plots, ncol = 3)) 






