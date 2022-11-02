### Probabilistic Crop Loading 

### 07 diagnostics

# Edited by E. Paulukonis October 2022


#### Read in field data
print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_field<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_field<-setNames(lapply(ill_field, readOGR), tools::file_path_sans_ext(basename(ill_field)))
ill_fieldf<-ill_field[(mixedsort(as.character(names(ill_field))))]


names(ill_fieldf)

# field_list_ill_f<-list()
# field_list_ill_f[[1]]<-ill_field[1:2]
# field_list_ill_f[[2]]<-ill_field[3:4]
# field_list_ill_f[[3]]<-ill_field[5:6]


field_list_ill_f<-list()
field_list_ill_f[[1]]<-ill_fieldf[1:14]
field_list_ill_f[[2]]<-ill_fieldf[15:28]
field_list_ill_f[[3]]<-ill_fieldf[29:42]
names(field_list_ill_f)<-c("Champaign","DuPage","McHenry")

rm(ill_field)
rm(ill_fieldf)


print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
mi_field<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
mi_field<-setNames(lapply(mi_field, readOGR), tools::file_path_sans_ext(basename(mi_field)))
mi_fieldf<-mi_field[order(mixedsort(names(mi_field)))]

field_list_mi_f<-list()
field_list_mi_f[[1]]<-mi_fieldf[1:14]
field_list_mi_f[[2]]<-mi_fieldf[15:28]
field_list_mi_f[[3]]<-mi_fieldf[29:42]
names(field_list_mi_f)<-c("Huron", "Oceana", "VanBuren") 

rm(mi_field)
rm(mi_fieldf)

print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/CPAA"), pattern='.shp', all.files=TRUE, full.names=FALSE))
wi_field<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN/CPAA"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
wi_field<-setNames(lapply(wi_field, readOGR), tools::file_path_sans_ext(basename(wi_field)))
wi_field<-wi_field[order(mixedsort(names(wi_field)))]

field_list_wi_f<-list()
field_list_wi_f[[1]]<-wi_fieldf[1:14]
field_list_wi_f[[2]]<-wi_fieldf[15:28]
field_list_wi_f[[3]]<-wi_fieldf[29:42]
names(field_list_wi_f)<-c("Langlade","Rock","Waushara")

rm(wi_field)
rm(wi_fieldf)


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



#note; this is the raw cdl data area comparison under the field polygons, but we really need it in the field data itself

years<-2008:2021
cntynames<-c('CHAMPAIGN',"DUPAGE","MCHENRY")

#function for mode
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }






acreages_by_cdl<-list()
acreages_by_county<-list()
cdl_extract_data<-list()
for(county in 1:length(field_list_ill_f)){
  county_layers<-field_list_ill_f[[county]]
  
  for(f in 1:length(cdl_data_ill_rec)){
    cdl<-cdl_data_ill_rec[[f]]
    cdl_extract<-lapply(county_layers, function(x) exact_extract(cdl, x, "mode"))
    cdl_extract<-lapply(cdl_extract, function(x) as.data.frame(x))
  
    field_area<- function(x){as.data.frame(area(x), na.rm=T) }
    field_extract<-lapply(county_layers, field_area)

    cdl_extract<-mapply(c, cdl_extract, field_extract, SIMPLIFY=FALSE)
    cdl_extract_df<-lapply(cdl_extract, function(x) as.data.frame(do.call("cbind", x)))
    
    get_acreages<-function(y){
      y$polygon<-row.names(y)
      colnames(y)[1]<-"Class"
      colnames(y)[2]<-"area"
      y$area<-(y$area)*0.000247105 #convert to acres
      crop_total<-aggregate(y$area, by=list(Class=y$Class), FUN=sum)
      crop_final<-left_join(crop_total, cdlkey, by="Class")
      crop_final$Category<-  toupper(crop_final$Category) 
      crop_final$Year<-years[[f]]
      crop_final
    }
     
    get_acreages_by_cdl<-lapply(cdl_extract_df, get_acreages)
    acreages_by_cdl[[f]]<-get_acreages_by_cdl
  }
  
  names(acreages_by_cdl)<-2008:2021
  acreages_by_county[[county]]<-acreages_by_cdl
  cdl_extract_data[[county]]<-cdl_extract_df
 
}


names(acreages_by_county)<-cntynames










list_of_ratio_by_county_and_year<-list()

for(n in 1:length(acreages_by_county)){
  n=1
  #county_name<-'CHAMPAIGN'
  county<-acreages_by_county[[n]]
  county_name<-names(acreages_by_county[n])
  names(county)<-years
  
  list_of_year<-list()
  for(y in 1:length(county)){
    year_list<-county[[y]]
    list_of_ratios<-list()
    for(layer in 1:length(year_list)){
    layer_by_year<-year_list[[layer]]
    layer_by_year$year<-as.numeric(names(county[y]))
    layer_by_year$Category<-  toupper(layer_by_year$Category) 
    layer_by_year$Category[grepl('WINTER', layer_by_year$Category)] <- 'WHEAT'
    
    
    ill_nassx<-ill_nass[!ill_nass$Value == " (D)",]
    ill_nassx$Value<-as.numeric(as.numeric(gsub(",", "", ill_nassx$Value)))
    
    ill_nass_y<- ill_nassx %>%
      filter(County %in% county_name) %>%
      filter(Year %in% c(layer_by_year$year)) %>%
      filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED', .))) %>%
      group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
    
    names(layer_by_year)[3]<-"Commodity"
    layer_by_year_crops<-inner_join(layer_by_year, ill_nass_y, by = "Commodity")
    layer_by_year_crops$ratio<-layer_by_year_crops$sum/layer_by_year_crops$x
    colnames(layer_by_year_crops)[2]<-"fieldacres"
    colnames(layer_by_year_crops)[8]<-"NASSacres"
    layer_by_year_crops$thresh<-names(year_list[layer])
    #county_by_year_crops$County<-county_name
    list_of_ratios[[layer]]<-layer_by_year_crops
    
    }
    list_of_year[[y]]<-do.call(rbind, list_of_ratios)
    
  }
  
  names(list_of_year)<-years
  final_df_ratio<-do.call(rbind, list_of_year)
  
  list_of_ratio_by_county_and_year[[n]]<-final_df_ratio
}

names(list_of_ratio_by_county_and_year)<-cntynames



#Plot showing the ratio of nass acres to field acres across years, by threshold, crop 
#test1<-test[test$thresh =="Champaign1_fin",]

ratio_plot<-ggplot(test, aes(x=year, y=(ratio), group= Bin, fill=Bin, colour=Bin, shape=thresh)) + 
  geom_point()+
  xlab("Crop") + 
  ylab("ratio")+
  labs(title = "Ratio of Stuff")+
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ratio_plot




for(c in 1:length(list_of_ratio_by_county_and_year)){
  c=1
final_df_ratio<-list_of_ratio_by_county_and_year[[c]]
#Plot showing combined data
sum_field<-final_df_ratio %>% group_by(year, thresh) %>% summarise(sumf = sum(as.numeric(fieldacres)))
sum_nass<-final_df_ratio %>% group_by(year, thresh) %>% summarise(sumf = sum(as.numeric(NASSacres)))

total_ratio<-cbind(sum_field,sum_nass)
names(total_ratio)<-c("year","thresh","sum_field","year","thresh","sum_nass")

total_ratio<-total_ratio[,3:6]
total_ratio$ratio<-total_ratio$sum_field/total_ratio$sum_nass


ratio_plot<-ggplot(total_ratio, aes(x=year, y=(ratio), group= thresh, fill=thresh, colour=thresh)) + 
  geom_point()+
  geom_line()+
  geom_hline(yintercept=1, linetype='dotted', col = 'red')+
  xlab("Year") + 
  ylab("Ratio")+
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  labs(title = "Ratio of Field Acres to NASS Acres, by Threshold")+
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ratio_plot


}



####create polygon layer with crops ----

#2008
champ1<-st_as_sf(county_layers[[1]])
champ1<-cbind(champ1,cdl_extract_df[[1]])
colnames(champ1)
colnames(champ1)[3]<-"crop"
colnames(champ1)[4]<-"area"

st_write(champ1, root_data_out, layer="champaign1_2008.shp", driver = "ESRI Shapefile")
#plot(champ1[3])




#2021
champ1<-st_as_sf(field_list_ill_f[[1]][[1]])
champ1<-cbind(champ1,cdl_extract_data[[1]][[1]])
colnames(champ1)
colnames(champ1)[3]<-"crop"
colnames(champ1)[4]<-"area"
champ1$id<-as.numeric(row.names(champ1))+1

st_write(champ1, root_data_out, layer="test_champaign1_2021.shp", driver = "ESRI Shapefile")
plot(champ1[3])


# st_precision(champ1) <- 0.1 
# touching_list<-st_touches(st_make_valid(champ1))
# library(igraph)
# g = graph.adjlist(touching_list)
# c = components(g)
# #test<-c[[1]]

inst_list<-st_intersects(champ1)
library(igraph)
g = graph.adjlist(inst_list)
c = components(g)


test<-table(c$membership)
test<-as.data.frame(cbind(c$membership, 1:(nrow(champ1)-1)))
names(test)<-c("groups","id")
champ1<-merge(champ1,test,by="id")


testu<-champ1 %>% 
  group_by(groups, crop) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


area_thresh <- units::set_units(45, m^2) #Fill holes, just a few meters
fw_fill<- fill_holes(testu, threshold = area_thresh)


st_write(fw_fill, root_data_out, layer="test_int_fh_champ1_2021.shp", driver = "ESRI Shapefile")




