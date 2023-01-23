### Probabilistic Crop Loading 

### 09 hyperparameter runs

# Edited by E. Paulukonis January 2023


#### Hyperparameter runs ----
cdlkey<-read.csv(paste0(cdl_dir,"/CDL_key.csv")) #cdl key

#Michigan 
print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
mi_field<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
mi_field<-setNames(lapply(mi_field, readOGR), tools::file_path_sans_ext(basename(mi_field)))
#mi_fieldf<-mi_field[order(mixedsort(names(mi_field)))]

field_list_mi_f<-list()
field_list_mi_f[[1]]<-mi_field[1:14]
field_list_mi_f[[2]]<-mi_field[15:28]
field_list_mi_f[[3]]<-mi_field[29:42]
names(field_list_mi_f)<-c("Huron", "Oceana", "VanBuren")
# rm(mi_field)

foreach(i=1:3) %do% {
  areaf<-sapply(field_list_mi_f[[i]],function(x) sum(terra::area(x)))
  field_list_mi_f[[i]]<-field_list_mi_f[[i]][order(areaf)]
  names(field_list_mi_f[[i]])<-paste0(names(field_list_mi_f[i]),14:1,"fin")
}



#Wisconsin
print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), pattern='.shp', all.files=TRUE, full.names=FALSE))
wi_field<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
wi_field<-setNames(lapply(wi_field, readOGR), tools::file_path_sans_ext(basename(wi_field)))
# wi_field<-wi_field[order(mixedsort(names(wi_field)))]

field_list_wi_f<-list()
field_list_wi_f[[1]]<-wi_field[1:14]
field_list_wi_f[[2]]<-wi_field[15:28]
field_list_wi_f[[3]]<-wi_field[29:42]
names(field_list_wi_f)<-c("Langlade","Rock","Waushara")
#rm(wi_field)

foreach(i=1:3) %do% {
  areaf<-sapply(field_list_wi_f[[i]],function(x) sum(terra::area(x)))
  field_list_wi_f[[i]]<-field_list_wi_f[[i]][order(areaf)]
  names(field_list_wi_f[[i]])<-paste0(names(field_list_wi_f[i]),14:1,"fin")
}


### Michigan acreages
cntynames<-c("HURON", "OCEANA", "VAN BUREN")
#acreages_by_countyM<-readRDS(paste0(root_data_out,"/acreages_by_county_sfM.RData"))

acreages_by_cdl<-list()
acreages_by_county<-list()
cdl_extract_data<-list()
for(county in 1:length(field_list_mi_f)){
  county_layers<-field_list_mi_f[[county]]
  
  for(f in 1:length(cdl_data_mi_rec)){
    cdl<-cdl_data_mi_rec[[f]]
    cdl_extract<-lapply(county_layers, function(x) exact_extract(cdl, x, "mode"))
    cdl_extract<-lapply(cdl_extract, function(x) as.data.frame(x))
    
    field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
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

acreages_by_countyM<-acreages_by_county
saveRDS(acreages_by_county, file=paste0(root_data_out,"/acreages_by_county_sfM.RData"))
# names(acreages_by_countyM)<-cntynames
# 
# list_of_final_data_by_countyM<-list()
# for(n in 1:length(acreages_by_countyM)){
#   county<-acreages_by_countyM[[n]]
#   county_name<-names(acreages_by_countyM[n])
#   names(county)<-years
#   
#   list_of_year<-list()
#   for(y in 1:length(county)){
#     year_list<-county[[y]]
#     list_of_field<-list()
#     for(layer in 1:length(year_list)){
#       layer_by_year<-year_list[[layer]]
#       layer_by_year$year<-as.numeric(names(county[y]))
#       layer_by_year<-year_list[[layer]]
#       layer_by_year$year<-as.numeric(names(county[y]))
#       layer_by_year$Category<-  toupper(layer_by_year$Category) 
#       layer_by_year$Category[grep("\\bWINTER WHEAT\\b", layer_by_year$Category)] <- 'WHEAT' 
#       # layer_by_year$Category[grep("\\bALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
#       # layer_by_year$Category[grep("\\bOTHER HAY/NON ALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
#       # layer_by_year$x<-ifelse(layer_by_year$Class ==  36, with(layer_by_year,sum(x[Category =='HAY & HAYLAGE'])), layer_by_year$x)
#       # layer_by_year<-layer_by_year[!layer_by_year$Class == 37,] #drop the other hay class because we merged it with the alfalfa class
#       
#       layer_by_year$threshold<-15-layer
#       list_of_field[[layer]]<-layer_by_year
#       
#     }
#     
#     list_of_year[[y]]<-do.call(rbind, list_of_field)
#     
#   }
#   
#   field_data<-do.call(rbind, list_of_year)
#   #NASS data
#   mi_nassx<-mi_nass[!mi_nass$Value == " (D)",]
#   mi_nassx$Value<-as.numeric(as.numeric(gsub(",", "", mi_nassx$Value)))
#   mi_nassx$Commodity[grep("\\bBEANS\\b", mi_nassx$Commodity)] <- 'DRY BEANS'
#   mi_nass_y<- mi_nassx %>%
#     #filter(Year %in% c(layer_by_year$year)) %>%
#     filter(County %in% county_name) %>%
#     filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .)))%>% #use 'non' to filter bearing and non-bearing
#     filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING|WHEAT, WINTER', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
#   
#   #extra step to deal with hay/alfalfa  
#   #we need to keep ONLY alfalfa in the CoA, and match excluding alfalfa to the same name in the CDL; other types of hay are not specific enough, so are removed
#   # totalhay<-mi_nass_y[(mi_nass_y$Commodity == 'HAY & HAYLAGE'),] 
#   # totalhay<-totalhay %>%  group_by(Year) %>% mutate(Value= sum(as.numeric(Value))) %>% filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('IRRIGATED', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
#   alfalfa<-mi_nass_y[(mi_nass_y$Commodity == 'HAY' & grepl('ALFALFA',mi_nass_y$Data.Item)),]
#   alfalfa$Commodity <- ifelse(grepl('EXCL',alfalfa$Data.Item), 'OTHER HAY/NON ALFALFA','ALFALFA')
#   
#   mi_nass_y <- mi_nass_y %>% 
#     filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('HAY', .))) %>%
#     rbind(alfalfa)%>%
#     group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
#   
#   #join together the field and the CoA data 
#   names(field_data)[3]<-"Commodity"
#   names(field_data)[2]<-"fieldacres"
#   layer_by_year_crops<-left_join(field_data, mi_nass_y, by = c("Year","Commodity"))
#   colnames(layer_by_year_crops)[8]<-"NASSacres"
#   
#   layer_by_year_crops<-na.omit(layer_by_year_crops) #IF there are no crops represented in NASS for that year, drop those rows
#   layer_by_year_crops$County<-cntynames[[n]]
#   list_of_final_data_by_countyM[[n]]<-layer_by_year_crops
#   
# }
# 
# names(list_of_final_data_by_countyM)<-cntynames
# for(i in 1:length(list_of_final_data_by_countyM)){
#   write.csv(list_of_final_data_by_countyM[i], paste0(root_figures, "/", names(list_of_final_data_by_countyM)[i], "_finaldf.csv"))
# }
# 
# cntynames<-c("HURON", "OCEANA", "VAN BUREN")
# 
# 
# list_of_plotsM<-list()
# list_of_dataM<-list()
# for(c in 1:length(list_of_final_data_by_countyM)){
#   layer_by_year_crops<-list_of_final_data_by_countyM[[c]]
#   extracted_cdl_dataM$CDLacres<-((extracted_cdl_dataM$count)*900)*0.000247105
#   colnames(extracted_cdl_dataM)[1]<-'Class'
#   layer_by_year_crops<-left_join(layer_by_year_crops, extracted_cdl_dataM, by=c("County","year","Class"))
#   
#   #Plot showing combined data
#   sum_field<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(fieldacres)))
#   sum_nass<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(NASSacres)))
#   sum_cdl<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(CDLacres)))
#   all_data<-cbind(sum_field,sum_nass, sum_cdl)
#   names(all_data)<-c("year","thresh","sum_field","year","thresh","sum_nass","year","thresh","sum_cdl")
#   all_data<-all_data[,c(1:3,6,9)]
#   
#   #because some thresholds include crops that others don't, we need to modify the final NASS values to reflect that
#   all_data_t <- all_data %>% group_by(year) %>% 
#     mutate(sum_nass = ifelse(max(sum_nass) > min(sum_nass),  max(sum_nass), min(sum_nass))) 
#   all_data_t <- all_data_t %>% group_by(year) %>% 
#     mutate(sum_cdl = ifelse(max(sum_cdl) > min(sum_cdl),  max(sum_cdl), min(sum_cdl)))
#   
#   all_data_t$County<-names(field_list_mi_f)[c]
#   list_of_dataM[[c]]<-all_data_t
#   
#   #plot showing the specific changes in acreage by year specific to each County
#   
#   ratio_plot<-ggplot(all_data_t, aes(x=year, y=(sum_field), fill=factor(thresh), colour=factor(thresh))) + 
#     geom_point()+
#     geom_line()+
#     xlab("Year") +
#     ylab("Acreages")+
#     scale_x_discrete(name ="Year", 
#                      limits=c(2008:2021))+
#     scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0, .1)))+
#     labs(title = paste0(names(field_list_mi_f)[c]," County Total Field Acres to NASS Acres, by Threshold"))+
#     guides(fill=guide_legend(title="Threshold"), colour=guide_legend(title="Threshold"))+
#     theme(panel.background = element_blank(),
#           axis.line = element_line(colour = "black"),
#           axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#           axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#   
#   ratio_plot
#   
#   output<-ratio_plot+
#     geom_point(all_data_t,mapping=aes(y=(sum_nass)), col='black') +
#     geom_line(all_data_t,mapping=aes(y=(sum_nass)), col='black')+
#     geom_point(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey') +
#     geom_line(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey')
#   output
#   
#   
#   list_of_plotsM[[c]]<-output
#   
# }
# 
# finalm<-ggpubr::ggarrange(list_of_plotsM[[1]], list_of_plotsM[[2]], list_of_plotsM[[3]], # list of plots
#                           labels = "AUTO", # labels
#                           common.legend = T, # COMMON LEGEND
#                           legend = "right", # legend position
#                           align = "hv", # Align them both, horizontal and vertical
#                           nrow = 3)  # number of rows
# finalm
# 
# 
# #Plot showing the ratio of NASS acres to field acres across years, by crop at threshold 1
# final_Michigan_by_crop<-do.call(rbind, list_of_final_data_by_countyM)
# final_Michigan_by_crop$Label<-paste0(tools::toTitleCase(tolower(final_Michigan_by_crop$County))," County, Michigan")
# 
# final_Michigan_by_crop$County <- factor(final_Michigan_by_crop$County , levels = c("HURON", "OCEANA", "VAN BUREN"))
# final_Michigan_by_crop$ratio<-final_Michigan_by_crop$fieldacres/final_Michigan_by_crop$NASSacres
# 
# final_Michigan_by_crop <- final_Michigan_by_crop %>% 
#   group_by(County, year, threshold) %>% mutate(percent = (NASSacres/sum(NASSacres))*100) #get percentage of each crop
# final_Michigan_by_crop<-final_Michigan_by_crop[final_Michigan_by_crop$percent>4,] #get crops that make up ~95% of crop area
# 
# final_Michigan_by_crop<-final_Michigan_by_crop[final_Michigan_by_crop$threshold ==1, ]






### Wisconsin Acreages----
#acreages_by_countyW<-readRDS(paste0(root_data_out,"/acreages_by_county_sfW.RData"))
cntynames<-c("LANGLADE","ROCK","WAUSHARA")

acreages_by_cdl<-list()
acreages_by_countyW<-list()
cdl_extract_data<-list()
for(county in 1:length(field_list_wi_f)){
  county_layers<-field_list_wi_f[[county]]
  
  for(f in 1:length(cdl_data_wi_rec)){
    cdl<-cdl_data_wi_rec[[f]]
    cdl_extract<-lapply(county_layers, function(x) exact_extract(cdl, x, "mode"))
    cdl_extract<-lapply(cdl_extract, function(x) as.data.frame(x))
    
    field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
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
  acreages_by_countyW[[county]]<-acreages_by_cdl
  cdl_extract_data[[county]]<-cdl_extract_df
  
}

saveRDS(acreages_by_countyW, file=paste0(root_data_out,"/acreages_by_county_sfW.RData"))
#names(acreages_by_countyW)<-cntynames

# list_of_final_data_by_countyW<-list()
# for(n in 1:length(acreages_by_countyW)){
#   county<-acreages_by_countyW[[n]]
#   county_name<-names(acreages_by_countyW[n])
#   names(county)<-years
#   
#   list_of_year<-list()
#   for(y in 1:length(county)){
#     year_list<-county[[y]]
#     list_of_field<-list()
#     for(layer in 1:length(year_list)){
#       layer_by_year<-year_list[[layer]]
#       layer_by_year$year<-as.numeric(names(county[y]))
#       layer_by_year<-year_list[[layer]]
#       layer_by_year$year<-as.numeric(names(county[y]))
#       layer_by_year$Category<-  toupper(layer_by_year$Category) 
#       layer_by_year$Category[grep("\\bWINTER WHEAT\\b", layer_by_year$Category)] <- 'WHEAT' #change the alfalfa to general hay/haylage
#       # layer_by_year$Category[grep("\\bALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
#       # layer_by_year$Category[grep("\\bOTHER HAY/NON ALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
#       # layer_by_year$x<-ifelse(layer_by_year$Class ==  36, with(layer_by_year,sum(x[Category =='HAY'])), layer_by_year$x)
#       # layer_by_year<-layer_by_year[!layer_by_year$Class == 37,] #drop the other hay class because we merged it with the alfalfa class
#       
#       layer_by_year$threshold<-15-layer
#       list_of_field[[layer]]<-layer_by_year
#       
#     }
#     
#     list_of_year[[y]]<-do.call(rbind, list_of_field)
#     
#   }
#   
#   field_data<-do.call(rbind, list_of_year)
#   #NASS data
#   wi_nassx<-wi_nass[!wi_nass$Value == " (D)",]
#   wi_nassx$Value<-as.numeric(as.numeric(gsub(",", "", wi_nassx$Value)))
#   wi_nassx$Commodity[grep("\\bBEANS\\b", wi_nassx$Commodity)] <- 'DRY BEANS'
#   wi_nass_y<- wi_nassx %>%
#     #filter(Year %in% c(layer_by_year$year)) %>%
#     filter(County %in% county_name) %>%
#     filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .)))%>% #use 'non' to filter bearing and non-bearing
#     filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING|WHEAT, WINTER', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
#   
#   #extra step to deal with hay/alfalfa  
#   #we need to keep ONLY alfalfa in the CoA, and match excluding alfalfa to the same name in the CDL; other types of hay are not specific enough, so are removed
#   # totalhay<-wi_nass_y[(wi_nass_y$Commodity == 'HAY & HAYLAGE'),] 
#   # totalhay<-totalhay %>%  group_by(Year) %>% mutate(Value= sum(as.numeric(Value))) %>% filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('IRRIGATED', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
#   alfalfa<-wi_nass_y[(wi_nass_y$Commodity == 'HAY' & grepl('ALFALFA',wi_nass_y$Data.Item)),]
#   alfalfa$Commodity <- ifelse(grepl('EXCL',alfalfa$Data.Item), 'OTHER HAY/NON ALFALFA','ALFALFA')
#   
#   wi_nass_y <- wi_nass_y %>% 
#     filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('HAY', .))) %>%
#     rbind(alfalfa)%>%
#     group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
#   
#   #join together the field and the CoA data 
#   names(field_data)[3]<-"Commodity"
#   names(field_data)[2]<-"fieldacres"
#   layer_by_year_crops<-left_join(field_data, wi_nass_y, by = c("Year","Commodity"))
#   colnames(layer_by_year_crops)[8]<-"NASSacres"
#   
#   layer_by_year_crops<-na.omit(layer_by_year_crops) #IF there are no crops represented in NASS for that year, drop those rows
#   layer_by_year_crops$County<-cntynames[[n]]
#   list_of_final_data_by_countyW[[n]]<-layer_by_year_crops
#   
# }
# 
# names(list_of_final_data_by_countyW)<-cntynames
# for(i in 1:length(list_of_final_data_by_countyW)){
#   write.csv(list_of_final_data_by_countyW[i], paste0(root_figures, "/", names(list_of_final_data_by_countyW)[i], "_finaldf_og.csv"))
# }
# 
# cntynames<-c("LANGLADE","ROCK","WAUSHARA")
# 
# list_of_plotsW<-list()
# list_of_dataW<-list()
# for(c in 1:length(list_of_final_data_by_countyW)){
#   
#   layer_by_year_crops<-list_of_final_data_by_countyW[[c]]
#   extracted_cdl_dataW$CDLacres<-((extracted_cdl_dataW$count)*900)*0.000247105
#   colnames(extracted_cdl_dataW)[1]<-'Class'
#   layer_by_year_crops<-left_join(layer_by_year_crops, extracted_cdl_dataW, by=c("County","year","Class"))
#   
#   #Plot showing combined data
#   sum_field<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(fieldacres)))
#   sum_nass<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(NASSacres)))
#   sum_cdl<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(CDLacres)))
#   all_data<-cbind(sum_field,sum_nass, sum_cdl)
#   names(all_data)<-c("year","thresh","sum_field","year","thresh","sum_nass","year","thresh","sum_cdl")
#   all_data<-all_data[,c(1:3,6,9)]
#   
#   #because some thresholds include crops that others don't, we need to modify the final NASS values to reflect that
#   all_data_t <- all_data %>% group_by(year) %>% 
#     mutate(sum_nass = ifelse(max(sum_nass) > min(sum_nass),  max(sum_nass), min(sum_nass))) 
#   all_data_t <- all_data_t %>% group_by(year) %>% 
#     mutate(sum_cdl = ifelse(max(sum_cdl) > min(sum_cdl),  max(sum_cdl), min(sum_cdl)))
#   
#   all_data_t$County<-names(field_list_wi_f)[c]
#   list_of_dataW[[c]]<-all_data_t
#   
#   #plot showing the specific changes in acreage by year specific to each County
#   
#   ratio_plot<-ggplot(all_data_t, aes(x=year, y=(sum_field), fill=factor(thresh), colour=factor(thresh))) + 
#     geom_point()+
#     geom_line()+
#     xlab("Year") +
#     ylab("Acreages")+
#     scale_x_discrete(name ="Year", 
#                      limits=c(2008:2021))+
#     scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0, .1)))+
#     labs(title = paste0(names(field_list_wi_f)[c]," County Total Field Acres to NASS Acres, by Threshold"))+
#     guides(fill=guide_legend(title="Threshold"), colour=guide_legend(title="Threshold"))+
#     theme(panel.background = element_blank(),
#           axis.line = element_line(colour = "black"),
#           axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#           axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#   
#   ratio_plot
#   
#   output<-ratio_plot+
#     geom_point(all_data_t,mapping=aes(y=(sum_nass)), col='black') +
#     geom_line(all_data_t,mapping=aes(y=(sum_nass)), col='black')+
#     geom_point(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey') +
#     geom_line(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey')
#   output
#   
#   
#   list_of_plotsW[[c]]<-output
#   
# }
# 
# finalw<-ggpubr::ggarrange(list_of_plotsW[[1]], list_of_plotsW[[2]], list_of_plotsW[[3]], # list of plots
#                           labels = "AUTO", # labels
#                           common.legend = T, # COMMON LEGEND
#                           legend = "right", # legend position
#                           align = "hv", # Align them both, horizontal and vertical
#                           nrow = 3)  # number of rows
# finalw
# 
# 
# 
# #Plot showing the ratio of NASS acres to field acres across years, by crop at threshold 1
# final_Wisconsin_by_crop<-do.call(rbind, list_of_final_data_by_countyW)
# final_Wisconsin_by_crop$Label<-paste0(tools::toTitleCase(tolower(final_Wisconsin_by_crop$County))," County, Wisconsin")
# 
# final_Wisconsin_by_crop$County <- factor(final_Wisconsin_by_crop$County , levels = c("ROCK", "WAUSHARA", "LANGLADE"))
# final_Wisconsin_by_crop$ratio<-final_Wisconsin_by_crop$fieldacres/final_Wisconsin_by_crop$NASSacres
# 
# final_Wisconsin_by_crop <- final_Wisconsin_by_crop %>% 
#   group_by(County, year, threshold) %>% mutate(percent = (NASSacres/sum(NASSacres))*100) #get percentage of each crop
# final_Wisconsin_by_crop<-final_Wisconsin_by_crop[final_Wisconsin_by_crop$percent>4,] #get crops that make up ~95% of crop area
# 
# 
# final_Wisconsin_by_crop<-final_Wisconsin_by_crop[final_Wisconsin_by_crop$threshold ==1, ]
