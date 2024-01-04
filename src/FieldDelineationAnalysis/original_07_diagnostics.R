### Probabilistic Crop Loading 

### 07 diagnostics

# Edited by E. Paulukonis October 2022


### Read in data ---- 

#options(scipen = 999) #remove exponent options, throws R off

#get plot palette
# library(randomcoloR)
# n <- 14
# palette <- distinctColorPalette(n)
# pie(rep(1, n), col=palette)


#Illinois
print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_field<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_field<-setNames(lapply(ill_field, readOGR), tools::file_path_sans_ext(basename(ill_field)))
#ill_fieldf<-ill_field[(mixedsort(as.character(names(ill_field))))]


# field_list_ill_f<-list()
# field_list_ill_f[[1]]<-ill_field[1:2]
# field_list_ill_f[[2]]<-ill_field[3:4]
# field_list_ill_f[[3]]<-ill_field[5:6]

field_list_ill_f<-list()
field_list_ill_f[[1]]<-ill_field[1:14]
field_list_ill_f[[2]]<-ill_field[15:28]
field_list_ill_f[[3]]<-ill_field[29:42]
names(field_list_ill_f)<-c("Champaign","DuPage","McHenry")
#rm(ill_field)

foreach(i=1:3) %do% {
  areaf<-sapply(field_list_ill_f[[i]],function(x) sum(terra::area(x)))
  field_list_ill_f[[i]]<-field_list_ill_f[[i]][order(areaf)]
  names(field_list_ill_f[[i]])<-paste0(names(field_list_ill_f[i]),14:1,"fin")
}

#saveRDS(field_list_mi_f, file=paste0(root_data_out,"/all_tif/MICHIGAN/field_data_I.RData"))

#Michigan 
print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
mi_field<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
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

#saveRDS(field_list_mi_f, file=paste0(root_data_out,"/all_tif/MICHIGAN/field_data_M.RData"))

#Wisconsin
print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB"), pattern='.shp', all.files=TRUE, full.names=FALSE))
wi_field<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN/SUB"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
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

#saveRDS(field_list_mi_f, file=paste0(root_data_out,"/all_tif/MICHIGAN/field_data_W.RData"))

### Read in CoA and OG CDL ----
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
# ill_nass<-rbind(ill_coa,ill_caps)
# mi_nass<-rbind(mi_coa,mi_caps)
# wi_nass<-rbind(wi_coa,wi_caps)

ill_nass<-ill_coa
mi_nass<-mi_coa
wi_nass<-wi_coa

ill_nass<-ill_nass[order(ill_nass$Year),]
mi_nass<-mi_nass[order(mi_nass$Year),]
wi_nass<-wi_nass[order(wi_nass$Year),]


years<-2008:2021


# mi_nass<-mi_nass[!mi_nass$Year==2007,]
# wi_nass<-mi_nass[!mi_nass$Year==2007,]

cdlkey<-read.csv(paste0(cdl_dir,"/CDL_key.csv")) #cdl key

# print(list.files(path=paste0(cdl_dir,"/Illinois"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
# avg_thresh_ill<- file.path(paste0(cdl_dir,"/Illinois"), list.files(path=paste0(cdl_dir,"/Illinois"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
# avg_thresh_ill<-setNames(lapply(avg_thresh_ill, read.csv), tools::file_path_sans_ext(basename(avg_thresh_ill)))
# avg_thresh_ill<-lapply(avg_thresh_ill, function(y) { y["X"] <- NULL; y })
# 
# 
# print(list.files(path=paste0(cdl_mi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
# avg_thresh_mi<- file.path(paste0(cdl_mi_dir,'/thresh_layers'), list.files(path=paste0(cdl_mi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
# avg_thresh_mi<-setNames(lapply(avg_thresh_mi, read.csv), tools::file_path_sans_ext(basename(avg_thresh_mi)))
# avg_thresh_mi<-lapply(avg_thresh_mi, function(y) { y["X"] <- NULL; y })
# 
# 
# print(list.files(path=paste0(cdl_wi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
# avg_thresh_wi<- file.path(paste0(cdl_wi_dir,'/thresh_layers'), list.files(path=paste0(cdl_wi_dir,'/thresh_layers'), pattern='.csv$', all.files=TRUE, full.names=FALSE))
# avg_thresh_wi<-setNames(lapply(avg_thresh_wi, read.csv), tools::file_path_sans_ext(basename(avg_thresh_wi)))
# avg_thresh_wi<-lapply(avg_thresh_wi, function(y) { y["X"] <- NULL; y })
# 
# 
# 
### Get the thresholds----
# print_thresholds_ill<-matrix(nrow=3,ncol=3,NA)
# colnames(print_thresholds_ill)<-c("threshold","county","state")
# 
# print_thresholds_mi<-matrix(nrow=3,ncol=3,NA)
# colnames(print_thresholds_mi)<-c("threshold","county","state")
# 
# print_thresholds_wi<-matrix(nrow=3,ncol=3,NA)
# colnames(print_thresholds_wi)<-c("threshold","county","state")
# 
# names_counties<-c("DuPage","McHenry","Champaign")
# 
#  for(n in 1:length(avg_thresh_ill)){
#   csv<-avg_thresh_ill[[n]]
#   n_thresh<-min(csv$bin)
#   csv$countyname<-names_counties[n]
#   countyname<-csv[1,9]
#   print_thresholds_ill[n,1]<-n_thresh
#   print_thresholds_ill[n,2]<-countyname
#   print_thresholds_ill[n,3]<-'ILL'
# }
# 
# for(n in 1:length(avg_thresh_mi)){
#   csv<-avg_thresh_mi[[n]]
#   n_thresh<-min(csv$bin)
#   countyname<-csv[1,9]
#   print_thresholds_mi[n,1]<-n_thresh
#   print_thresholds_mi[n,2]<-countyname
#   print_thresholds_mi[n,3]<-'MI'
# }
# 
# 
# for(n in 1:length(avg_thresh_wi)){
#   csv<-avg_thresh_wi[[n]]
#   n_thresh<-min(csv$bin)
#   countyname<-csv[1,9]
#   print_thresholds_wi[n,1]<-n_thresh
#   print_thresholds_wi[n,2]<-countyname
#   print_thresholds_wi[n,3]<-'WI'
# }
# 
# print_thresh<-as.data.frame(rbind(print_thresholds_ill, print_thresholds_mi,print_thresholds_wi))
# print_thresh
# 
# 


### Get CDL acreage ----
#remember to activate and reproject the county areas

# all_states<-readOGR(state_dir, layer = "tl_2021_us_county") #read in states
# 
# ill<-all_states[all_states$STATEFP == "17",]
# mi<-all_states[all_states$STATEFP == "26",]
# wi<-all_states[all_states$STATEFP == "55",]

##Illinois
extracted_cdl_dataI<-readRDS(paste0(root_data_out,"/extracted_cdl_dataI.RData"))
# study<-ill
# sub_group<-c("DuPage","McHenry","Champaign")
# sub<-study[study$NAME %in% sub_group,]
# 
# names_county<-list()
# county_set_list<-list()
# for (co in 1:length(sub)){
#   co_r<-sub[sub$NAME == sub$NAME[co],]
#   mask_crop<-function(x){
#     r_list<-crop(x, co_r)
#     mask(r_list, co_r) 
#   }
#   
#   county_set<-lapply(cdl_data_ill_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
#   county_set_list[[co]]<-stack(county_set) #represents single county stack
# 
# }
# 
# years <- list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
# extracted_cdl_data<-list()
# names(county_set_list)<-c("DU PAGE","CHAMPAIGN","MCHENRY") #order kind of wonky
# for(county in 1:length(county_set_list)){
#   county_layers<-county_set_list[[county]]
#   cdl_extract<-freq(county_layers)
#   cdl_extract<-Map(cbind, cdl_extract, year = years)
#   cdl_extract<-as.data.frame(do.call(rbind,cdl_extract))
#   cdl_extract$County<-names(county_set_list[county])
#  extracted_cdl_data[[county]] <-cdl_extract
# }
# 
# extracted_cdl_dataI<-do.call(rbind, extracted_cdl_data)
# saveRDS(extracted_cdl_dataI, file=paste0(root_data_out,"/extracted_cdl_dataI.RData"))


## Michigan
extracted_cdl_dataM<-readRDS(paste0(root_data_out,"/extracted_cdl_dataM.RData"))
# study<-mi
# sub_group<-c("Van Buren", "Oceana","Huron")
# sub<-study[study$NAME %in% sub_group,]
# crs_cdl<-crs(cdl_data_mi_rec[[1]])
# sub<-spTransform(sub, crs_cdl)
# 
# # plot(cdl_data_mi_rec[[1]])
# # plot(sub[1],add=T)
# 
# names_county<-list()
# county_set_list<-list()
# for (co in 1:length(sub)){
#   co_r<-sub[sub$NAME == sub$NAME[co],]
#   mask_crop<-function(x){
#     r_list<-crop(x, co_r)
#     mask(r_list, co_r)
#   }
# 
#   county_set<-lapply(cdl_data_mi_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
#   county_set_list[[co]]<-stack(county_set) #represents single county stack
# 
# }
# 
# years <- list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
# extracted_cdl_data<-list()
# names(county_set_list)<-c("VAN BUREN", "OCEANA","HURON")
# for(county in 1:length(county_set_list)){
#   county_layers<-county_set_list[[county]]
#   cdl_extract<-freq(county_layers)
#   cdl_extract<-Map(cbind, cdl_extract, year = years)
#   cdl_extract<-as.data.frame(do.call(rbind,cdl_extract))
#   cdl_extract$County<-names(county_set_list[county])
#   extracted_cdl_data[[county]] <-cdl_extract
# }
# 
# extracted_cdl_dataM<-do.call(rbind, extracted_cdl_data)
# saveRDS(extracted_cdl_dataM, file=paste0(root_data_out,"/extracted_cdl_dataM.RData"))


## Wisconsin
extracted_cdl_dataW<-readRDS(paste0(root_data_out,"/extracted_cdl_dataW.RData"))
# study<-wi
# sub_group<-c("Waushara","Langlade","Rock")
# sub<-study[study$NAME %in% sub_group,]
# crs_cdl<-crs(cdl_data_wi_rec[[1]])
# sub<-spTransform(sub, crs_cdl)
# 
# names_county<-list()
# county_set_list<-list()
# for (co in 1:length(sub)){
#   co_r<-sub[sub$NAME == sub$NAME[co],]
#   mask_crop<-function(x){
#     r_list<-crop(x, co_r)
#     mask(r_list, co_r)
#   }
# 
#   county_set<-lapply(cdl_data_wi_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
#   county_set_list[[co]]<-stack(county_set) #represents single county stack
# 
# }
# 
# years <- list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
# extracted_cdl_data<-list()
# names(county_set_list)<-c("WAUSHARA","LANGLADE","ROCK")
# for(county in 1:length(county_set_list)){
#   county_layers<-county_set_list[[county]]
#   cdl_extract<-freq(county_layers)
#   cdl_extract<-Map(cbind, cdl_extract, year = years)
#   cdl_extract<-as.data.frame(do.call(rbind,cdl_extract))
#   cdl_extract$County<-names(county_set_list[county])
#   extracted_cdl_data[[county]] <-cdl_extract
# }
# 
# extracted_cdl_dataW<-do.call(rbind, extracted_cdl_data)
# saveRDS(extracted_cdl_dataW, file=paste0(root_data_out,"/extracted_cdl_dataW.RData"))


### Illinois acreages----
acreages_by_countyI<-readRDS(paste0(root_data_out,"/acreages_by_countyI.RData"))
cntynames<-c('CHAMPAIGN',"DU PAGE","MCHENRY")
# acreages_by_cdl<-list()
# acreages_by_county<-list()
# cdl_extract_data<-list()
# for(county in 1:length(field_list_ill_f)){
#   county_layers<-field_list_ill_f[[county]]
#
#   for(f in 1:length(cdl_data_ill_rec)){
#     cdl<-cdl_data_ill_rec[[f]]
#     cdl_extract<-lapply(county_layers, function(x) exact_extract(cdl, x, "mode"))
#     cdl_extract<-lapply(cdl_extract, function(x) as.data.frame(x))
#
#     field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
#     field_extract<-lapply(county_layers, field_area)
#
#     cdl_extract<-mapply(c, cdl_extract, field_extract, SIMPLIFY=FALSE)
#     cdl_extract_df<-lapply(cdl_extract, function(x) as.data.frame(do.call("cbind", x)))
#
#     get_acreages<-function(y){
#       y$polygon<-row.names(y)
#       colnames(y)[1]<-"Class"
#       colnames(y)[2]<-"area"
#       y$area<-(y$area)*0.000247105 #convert to acres
#       crop_total<-aggregate(y$area, by=list(Class=y$Class), FUN=sum)
#       crop_final<-left_join(crop_total, cdlkey, by="Class")
#       crop_final$Category<-  toupper(crop_final$Category)
#       crop_final$Year<-years[[f]]
#       crop_final
#     }
#
#     get_acreages_by_cdl<-lapply(cdl_extract_df, get_acreages)
#     acreages_by_cdl[[f]]<-get_acreages_by_cdl
#   }
#
#   names(acreages_by_cdl)<-2008:2021
#   acreages_by_county[[county]]<-acreages_by_cdl
#   cdl_extract_data[[county]]<-cdl_extract_df
#
# }
# acreages_by_countyI<-acreages_by_county
# saveRDS(acreages_by_county, file=paste0(root_data_out,"/acreages_by_countyI.RData"))


names(acreages_by_countyI)<-cntynames

list_of_final_data_by_countyI<-list()
for(n in 1:length(acreages_by_countyI)){
  county<-acreages_by_countyI[[n]]
  county_name<-names(acreages_by_countyI[n])
  names(county)<-years
  
  list_of_year<-list()
  for(y in 1:length(county)){
    year_list<-county[[y]]
    list_of_field<-list()
    for(layer in 1:length(year_list)){
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year$Category<-  toupper(layer_by_year$Category)
      layer_by_year$Category[grep("\\bWINTER WHEAT\\b", layer_by_year$Category)] <- 'WHEAT'
      # layer_by_year$Category[grep("\\bALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
      # layer_by_year$Category[grep("\\bOTHER HAY/NON ALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
      # layer_by_year$x<-ifelse(layer_by_year$Class ==  36, with(layer_by_year,sum(x[Category =='HAY & HAYLAGE'])), layer_by_year$x)
      # layer_by_year<-layer_by_year[!layer_by_year$Class == 37,] #drop the other hay class because we merged it with the alfalfa class
      
      layer_by_year$threshold<-15-layer
      list_of_field[[layer]]<-layer_by_year
      
    }
    
    list_of_year[[y]]<-do.call(rbind, list_of_field)
    
  }
  
  field_data<-do.call(rbind, list_of_year)
  #NASS data
  ill_nassx<-ill_nass[!ill_nass$Value == " (D)",]
  ill_nassx$Value<-as.numeric(as.numeric(gsub(",", "", ill_nassx$Value)))
  ill_nassx$Commodity[grep("\\bBEANS\\b", ill_nassx$Commodity)] <- 'DRY BEANS'
  ill_nass_y<- ill_nassx %>%
    #filter(Year %in% c(layer_by_year$year)) %>%
    filter(County %in% county_name) %>%
    filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .)))%>% #use 'non' to filter bearing and non-bearing
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING|WHEAT, WINTER', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
  
  #extra step to deal with hay/alfalfa
  #we need to keep ONLY alfalfa in the CoA, and match excluding alfalfa to the same name in the CDL; other types of hay are not specific enough, so are removed
  # totalhay<-ill_nass_y[(ill_nass_y$Commodity == 'HAY & HAYLAGE'),]
  # totalhay<-totalhay %>%  group_by(Year) %>% mutate(Value= sum(as.numeric(Value))) %>% filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('IRRIGATED', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
  alfalfa<-ill_nass_y[(ill_nass_y$Commodity == 'HAY' & grepl('ALFALFA',ill_nass_y$Data.Item)),]
  alfalfa$Commodity <- ifelse(grepl('EXCL',alfalfa$Data.Item), 'OTHER HAY/NON ALFALFA','ALFALFA')
  
  ill_nass_y <- ill_nass_y %>%
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('HAY', .))) %>%
    rbind(alfalfa)%>%
    group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value)))
  
  #join together the field and the CoA data
  names(field_data)[3]<-"Commodity"
  names(field_data)[2]<-"fieldacres"
  layer_by_year_crops<-left_join(field_data, ill_nass_y, by = c("Year","Commodity"))
  colnames(layer_by_year_crops)[8]<-"NASSacres"
  
  layer_by_year_crops<-na.omit(layer_by_year_crops) #IF there are no crops represented in NASS for that year, drop those rows
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyI[[n]]<-layer_by_year_crops
  
}


names(list_of_final_data_by_countyI)<-cntynames
for(i in 1:length(list_of_final_data_by_countyI)){
  write.csv(list_of_final_data_by_countyI[i], paste0(root_figures, "/", names(list_of_final_data_by_countyI)[i], "_finaldf.csv"))
}


cntynames<-c('CHAMPAIGN',"DU PAGE","MCHENRY")
list_of_plotsI<-list()
list_of_dataI<-list()
for(c in 1:length(list_of_final_data_by_countyI)){
  layer_by_year_crops<-list_of_final_data_by_countyI[[c]]
  extracted_cdl_dataI$CDLacres<-((extracted_cdl_dataI$count)*900)*0.000247105
  colnames(extracted_cdl_dataI)[1]<-'Class'
  layer_by_year_crops<-left_join(layer_by_year_crops, extracted_cdl_dataI, by=c("County","year","Class"))
  
  #Plot showing combined data
  sum_field<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(fieldacres)))
  sum_nass<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(NASSacres)))
  sum_cdl<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(CDLacres)))
  all_data<-cbind(sum_field,sum_nass, sum_cdl)
  names(all_data)<-c("year","thresh","sum_field","year","thresh","sum_nass","year","thresh","sum_cdl")
  all_data<-all_data[,c(1:3,6,9)]
  
  #because some thresholds include crops that others don't, we need to modify the final NASS values to reflect that
  all_data_t <- all_data %>% group_by(year) %>%
    mutate(sum_nass = ifelse(max(sum_nass) > min(sum_nass),  max(sum_nass), min(sum_nass)))
  all_data_t <- all_data_t %>% group_by(year) %>%
    mutate(sum_cdl = ifelse(max(sum_cdl) > min(sum_cdl),  max(sum_cdl), min(sum_cdl)))
  
  all_data_t$County<-names(field_list_ill_f)[c]
  list_of_dataI[[c]]<-all_data_t
  
  #plot showing the specific changes in acreage by year specific to each County
  
  ratio_plot<-ggplot(all_data_t, aes(x=year, y=(sum_field), fill=factor(thresh), colour=factor(thresh))) +
    geom_point()+
    geom_line()+
    xlab("Year") +
    ylab("Acreages")+
    scale_x_discrete(name ="Year",
                     limits=c(2008:2021))+
    scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0, .1)))+
    labs(title = paste0(names(field_list_ill_f)[c]," County Total Field Acres to NASS Acres, by Threshold"))+
    guides(fill=guide_legend(title="Threshold"), colour=guide_legend(title="Threshold"))+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ratio_plot
  
  output<-ratio_plot+
    geom_point(all_data_t,mapping=aes(y=(sum_nass)), col='black') +
    geom_line(all_data_t,mapping=aes(y=(sum_nass)), col='black')+
    geom_point(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey') +
    geom_line(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey')
  output
  
  
  list_of_plotsI[[c]]<-output
  
}

#manually adjust any changes to axis needed
list_of_plotsI[[3]]<-list_of_plotsI[[3]] + scale_y_continuous(breaks=c(0,4270,25000, 50000, 75000,100000,125000,150000,200000))


finali<-ggpubr::ggarrange(list_of_plotsI[[1]], list_of_plotsI[[2]], list_of_plotsI[[3]], # list of plots
                          labels = "AUTO", # labels
                          common.legend = T, # COMMON LEGEND
                          legend = "right", # legend position
                          align = "hv", # Align them both, horizontal and vertical
                          nrow = 3)  # number of rows
finali

#Plot showing the ratio of NASS acres to field acres across years, by threshold, crop
final_Illinois_by_crop<-do.call(rbind, list_of_final_data_by_countyI)
final_Illinois_by_crop$Label<-paste0(tools::toTitleCase(tolower(final_Illinois_by_crop$County))," County, Illinois")
final_Illinois_by_crop$Label<-ifelse(final_Illinois_by_crop$Label == "Du Page County, Illinois","DuPage County, Illinois", final_Illinois_by_crop$Label)
final_Illinois_by_crop$Label<-ifelse(final_Illinois_by_crop$Label == "Mchenry County, Illinois","McHenry County, Illinois", final_Illinois_by_crop$Label)
final_Illinois_by_crop$County <- factor(final_Illinois_by_crop$County , levels = c("CHAMPAIGN", "MCHENRY", "DU PAGE"))
final_Illinois_by_crop$ratio<-final_Illinois_by_crop$fieldacres/final_Illinois_by_crop$NASSacres
final_Illinois_by_crop <- final_Illinois_by_crop %>%
  group_by(County, year, threshold) %>% mutate(percent = (NASSacres/sum(NASSacres))*100) #get percentage of each crop
final_Illinois_by_crop<-final_Illinois_by_crop[final_Illinois_by_crop$percent>5,] #get crops that make up 95% of crop area
final_Illinois_by_crop<-final_Illinois_by_crop[final_Illinois_by_crop$threshold ==1, ]
# ratio_plotI<-ggplot(final_Illinois_by_crop, aes(x=as.factor(Commodity), y=(log2(ratio)), fill=Commodity)) +
#   geom_boxplot()+
#   facet_wrap(.~County, scales = "free")+
#   xlab("Crop") +
#   ylab("Acreage Ratio")+
#   labs(title = "Ratio NASS Acres to Field Acres, By Crop")+
#   theme(panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#         axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
# theme(legend.position = "none")
# ratio_plotI


### Michigan acreages----
cntynames<-c("HURON", "OCEANA", "VAN BUREN")
acreages_by_countyM<-readRDS(paste0(root_data_out,"/acreages_by_countyM.RData"))

# acreages_by_cdl<-list()
# acreages_by_county<-list()
# cdl_extract_data<-list()
# for(county in 1:length(field_list_mi_f)){
#   county_layers<-field_list_mi_f[[county]]
#
#   for(f in 1:length(cdl_data_mi_rec)){
#     cdl<-cdl_data_mi_rec[[f]]
#     cdl_extract<-lapply(county_layers, function(x) exact_extract(cdl, x, "mode"))
#     cdl_extract<-lapply(cdl_extract, function(x) as.data.frame(x))
#
#     field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
#     field_extract<-lapply(county_layers, field_area)
#
#     cdl_extract<-mapply(c, cdl_extract, field_extract, SIMPLIFY=FALSE)
#     cdl_extract_df<-lapply(cdl_extract, function(x) as.data.frame(do.call("cbind", x)))
#
#     get_acreages<-function(y){
#       y$polygon<-row.names(y)
#       colnames(y)[1]<-"Class"
#       colnames(y)[2]<-"area"
#       y$area<-(y$area)*0.000247105 #convert to acres
#       crop_total<-aggregate(y$area, by=list(Class=y$Class), FUN=sum)
#       crop_final<-left_join(crop_total, cdlkey, by="Class")
#       crop_final$Category<-  toupper(crop_final$Category)
#       crop_final$Year<-years[[f]]
#       crop_final
#     }
#
#     get_acreages_by_cdl<-lapply(cdl_extract_df, get_acreages)
#     acreages_by_cdl[[f]]<-get_acreages_by_cdl
#   }
#
#   names(acreages_by_cdl)<-2008:2021
#   acreages_by_county[[county]]<-acreages_by_cdl
#   cdl_extract_data[[county]]<-cdl_extract_df
#
# }
#
# acreages_by_countyM<-acreages_by_county
# saveRDS(acreages_by_county, file=paste0(root_data_out,"/acreages_by_countyM.RData"))

names(acreages_by_countyM)<-cntynames

list_of_final_data_by_countyM<-list()
for(n in 1:length(acreages_by_countyM)){
  county<-acreages_by_countyM[[n]]
  county_name<-names(acreages_by_countyM[n])
  names(county)<-years
  
  list_of_year<-list()
  for(y in 1:length(county)){
    year_list<-county[[y]]
    list_of_field<-list()
    for(layer in 1:length(year_list)){
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year$Category<-  toupper(layer_by_year$Category)
      layer_by_year$Category[grep("\\bWINTER WHEAT\\b", layer_by_year$Category)] <- 'WHEAT'
      # layer_by_year$Category[grep("\\bALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
      # layer_by_year$Category[grep("\\bOTHER HAY/NON ALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
      # layer_by_year$x<-ifelse(layer_by_year$Class ==  36, with(layer_by_year,sum(x[Category =='HAY & HAYLAGE'])), layer_by_year$x)
      # layer_by_year<-layer_by_year[!layer_by_year$Class == 37,] #drop the other hay class because we merged it with the alfalfa class
      
      layer_by_year$threshold<-15-layer
      list_of_field[[layer]]<-layer_by_year
      
    }
    
    list_of_year[[y]]<-do.call(rbind, list_of_field)
    
  }
  
  field_data<-do.call(rbind, list_of_year)
  #NASS data
  mi_nassx<-mi_nass[!mi_nass$Value == " (D)",]
  mi_nassx$Value<-as.numeric(as.numeric(gsub(",", "", mi_nassx$Value)))
  mi_nassx$Commodity[grep("\\bBEANS\\b", mi_nassx$Commodity)] <- 'DRY BEANS'
  mi_nass_y<- mi_nassx %>%
    #filter(Year %in% c(layer_by_year$year)) %>%
    filter(County %in% county_name) %>%
    filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .)))%>% #use 'non' to filter bearing and non-bearing
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING|WHEAT, WINTER', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
  
  #extra step to deal with hay/alfalfa
  #we need to keep ONLY alfalfa in the CoA, and match excluding alfalfa to the same name in the CDL; other types of hay are not specific enough, so are removed
  # totalhay<-mi_nass_y[(mi_nass_y$Commodity == 'HAY & HAYLAGE'),]
  # totalhay<-totalhay %>%  group_by(Year) %>% mutate(Value= sum(as.numeric(Value))) %>% filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('IRRIGATED', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
  alfalfa<-mi_nass_y[(mi_nass_y$Commodity == 'HAY' & grepl('ALFALFA',mi_nass_y$Data.Item)),]
  alfalfa$Commodity <- ifelse(grepl('EXCL',alfalfa$Data.Item), 'OTHER HAY/NON ALFALFA','ALFALFA')
  
  mi_nass_y <- mi_nass_y %>%
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('HAY', .))) %>%
    rbind(alfalfa)%>%
    group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value)))
  
  #join together the field and the CoA data
  names(field_data)[3]<-"Commodity"
  names(field_data)[2]<-"fieldacres"
  layer_by_year_crops<-left_join(field_data, mi_nass_y, by = c("Year","Commodity"))
  colnames(layer_by_year_crops)[8]<-"NASSacres"
  
  layer_by_year_crops<-na.omit(layer_by_year_crops) #IF there are no crops represented in NASS for that year, drop those rows
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyM[[n]]<-layer_by_year_crops
  
}

names(list_of_final_data_by_countyM)<-cntynames
for(i in 1:length(list_of_final_data_by_countyM)){
  write.csv(list_of_final_data_by_countyM[i], paste0(root_figures, "/", names(list_of_final_data_by_countyM)[i], "_finaldf.csv"))
}

cntynames<-c("HURON", "OCEANA", "VAN BUREN")

list_of_plotsM<-list()
list_of_dataM<-list()
for(c in 1:length(list_of_final_data_by_countyM)){
  layer_by_year_crops<-list_of_final_data_by_countyM[[c]]
  extracted_cdl_dataM$CDLacres<-((extracted_cdl_dataM$count)*900)*0.000247105
  colnames(extracted_cdl_dataM)[1]<-'Class'
  layer_by_year_crops<-left_join(layer_by_year_crops, extracted_cdl_dataM, by=c("County","year","Class"))
  
  #Plot showing combined data
  sum_field<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(fieldacres)))
  sum_nass<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(NASSacres)))
  sum_cdl<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(CDLacres)))
  all_data<-cbind(sum_field,sum_nass, sum_cdl)
  names(all_data)<-c("year","thresh","sum_field","year","thresh","sum_nass","year","thresh","sum_cdl")
  all_data<-all_data[,c(1:3,6,9)]
  
  #because some thresholds include crops that others don't, we need to modify the final NASS values to reflect that
  all_data_t <- all_data %>% group_by(year) %>%
    mutate(sum_nass = ifelse(max(sum_nass) > min(sum_nass),  max(sum_nass), min(sum_nass)))
  all_data_t <- all_data_t %>% group_by(year) %>%
    mutate(sum_cdl = ifelse(max(sum_cdl) > min(sum_cdl),  max(sum_cdl), min(sum_cdl)))
  
  all_data_t$County<-names(field_list_mi_f)[c]
  list_of_dataM[[c]]<-all_data_t
  
  #plot showing the specific changes in acreage by year specific to each County
  
  ratio_plot<-ggplot(all_data_t, aes(x=year, y=(sum_field), fill=factor(thresh), colour=factor(thresh))) +
    geom_point()+
    geom_line()+
    xlab("Year") +
    ylab("Acreages")+
    scale_x_discrete(name ="Year",
                     limits=c(2008:2021))+
    scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0, .1)))+
    labs(title = paste0(names(field_list_mi_f)[c]," County Total Field Acres to NASS Acres, by Threshold"))+
    guides(fill=guide_legend(title="Threshold"), colour=guide_legend(title="Threshold"))+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ratio_plot
  
  output<-ratio_plot+
    geom_point(all_data_t,mapping=aes(y=(sum_nass)), col='black') +
    geom_line(all_data_t,mapping=aes(y=(sum_nass)), col='black')+
    geom_point(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey') +
    geom_line(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey')
  output
  
  
  list_of_plotsM[[c]]<-output
  
}

finalm<-ggpubr::ggarrange(list_of_plotsM[[1]], list_of_plotsM[[2]], list_of_plotsM[[3]], # list of plots
                          labels = "AUTO", # labels
                          common.legend = T, # COMMON LEGEND
                          legend = "right", # legend position
                          align = "hv", # Align them both, horizontal and vertical
                          nrow = 3)  # number of rows
finalm


#Plot showing the ratio of NASS acres to field acres across years, by crop at threshold 1
final_Michigan_by_crop<-do.call(rbind, list_of_final_data_by_countyM)
final_Michigan_by_crop$Label<-paste0(tools::toTitleCase(tolower(final_Michigan_by_crop$County))," County, Michigan")

final_Michigan_by_crop$County <- factor(final_Michigan_by_crop$County , levels = c("HURON", "OCEANA", "VAN BUREN"))
final_Michigan_by_crop$ratio<-final_Michigan_by_crop$fieldacres/final_Michigan_by_crop$NASSacres

final_Michigan_by_crop <- final_Michigan_by_crop %>%
  group_by(County, year, threshold) %>% mutate(percent = (NASSacres/sum(NASSacres))*100) #get percentage of each crop
final_Michigan_by_crop<-final_Michigan_by_crop[final_Michigan_by_crop$percent>4,] #get crops that make up ~95% of crop area


final_Michigan_by_crop<-final_Michigan_by_crop[final_Michigan_by_crop$threshold ==1, ]
# ratio_plotM<-ggplot(final_Michigan_by_crop, aes(x=as.factor(Commodity), y=(log2(ratio)), fill=Commodity)) +
#   geom_boxplot()+
#   facet_wrap(.~County, scales = "free")+
#   xlab("Crop") +
#   ylab("Acreage Ratio")+
#   labs(title = "Ratio NASS Acres to Field Acres, By Crop")+
#   theme(panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#         axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(legend.position = "none")
# ratio_plotM





### Wisconsin Acreages----
acreages_by_countyW<-readRDS(paste0(root_data_out,"/acreages_by_countyWn.RData"))
cntynames<-c("LANGLADE","ROCK","WAUSHARA")

# acreages_by_cdl<-list()
# acreages_by_countyW<-list()
# cdl_extract_data<-list()
# for(county in 1:length(field_list_wi_f)){
#   county_layers<-field_list_wi_f[[county]]
# 
#   for(f in 1:length(cdl_data_wi_rec)){
#     cdl<-cdl_data_wi_rec[[f]]
#     cdl_extract<-lapply(county_layers, function(x) exact_extract(cdl, x, "mode"))
#     cdl_extract<-lapply(cdl_extract, function(x) as.data.frame(x))
# 
#     field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
#     field_extract<-lapply(county_layers, field_area)
# 
#     cdl_extract<-mapply(c, cdl_extract, field_extract, SIMPLIFY=FALSE)
#     cdl_extract_df<-lapply(cdl_extract, function(x) as.data.frame(do.call("cbind", x)))
# 
#     get_acreages<-function(y){
#       y$polygon<-row.names(y)
#       colnames(y)[1]<-"Class"
#       colnames(y)[2]<-"area"
#       y$area<-(y$area)*0.000247105 #convert to acres
#       crop_total<-aggregate(y$area, by=list(Class=y$Class), FUN=sum)
#       crop_final<-left_join(crop_total, cdlkey, by="Class")
#       crop_final$Category<-  toupper(crop_final$Category)
#       crop_final$Year<-years[[f]]
#       crop_final
#     }
# 
#     get_acreages_by_cdl<-lapply(cdl_extract_df, get_acreages)
#     acreages_by_cdl[[f]]<-get_acreages_by_cdl
#   }
# 
#   names(acreages_by_cdl)<-2008:2021
#   acreages_by_countyW[[county]]<-acreages_by_cdl
#   cdl_extract_data[[county]]<-cdl_extract_df
# 
# }
# 
# saveRDS(acreages_by_countyW, file=paste0(root_data_out,"/acreages_by_countyW.RData"))
names(acreages_by_countyW)<-cntynames

list_of_final_data_by_countyW<-list()
for(n in 1:length(acreages_by_countyW)){
  county<-acreages_by_countyW[[n]]
  county_name<-names(acreages_by_countyW[n])
  names(county)<-years
  
  list_of_year<-list()
  for(y in 1:length(county)){
    year_list<-county[[y]]
    list_of_field<-list()
    for(layer in 1:length(year_list)){
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year$Category<-  toupper(layer_by_year$Category) 
      layer_by_year$Category[grep("\\bWINTER WHEAT\\b", layer_by_year$Category)] <- 'WHEAT' #change the alfalfa to general hay/haylage
      # layer_by_year$Category[grep("\\bALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
      # layer_by_year$Category[grep("\\bOTHER HAY/NON ALFALFA\\b", layer_by_year$Category)] <- 'HAY & HAYLAGE' #change the alfalfa to general hay/haylage
      # layer_by_year$x<-ifelse(layer_by_year$Class ==  36, with(layer_by_year,sum(x[Category =='HAY'])), layer_by_year$x)
      # layer_by_year<-layer_by_year[!layer_by_year$Class == 37,] #drop the other hay class because we merged it with the alfalfa class
      
      layer_by_year$threshold<-15-layer
      list_of_field[[layer]]<-layer_by_year
      
    }
    
    list_of_year[[y]]<-do.call(rbind, list_of_field)
    
  }
  
  field_data<-do.call(rbind, list_of_year)
  #NASS data
  wi_nassx<-wi_nass[!wi_nass$Value == " (D)",]
  wi_nassx$Value<-as.numeric(as.numeric(gsub(",", "", wi_nassx$Value)))
  wi_nassx$Commodity[grep("\\bBEANS\\b", wi_nassx$Commodity)] <- 'DRY BEANS'
  wi_nass_y<- wi_nassx %>%
    #filter(Year %in% c(layer_by_year$year)) %>%
    filter(County %in% county_name) %>%
    filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .)))%>% #use 'non' to filter bearing and non-bearing
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING|WHEAT, WINTER', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
  
  #extra step to deal with hay/alfalfa  
  #we need to keep ONLY alfalfa in the CoA, and match excluding alfalfa to the same name in the CDL; other types of hay are not specific enough, so are removed
  # totalhay<-wi_nass_y[(wi_nass_y$Commodity == 'HAY & HAYLAGE'),] 
  # totalhay<-totalhay %>%  group_by(Year) %>% mutate(Value= sum(as.numeric(Value))) %>% filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('IRRIGATED', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
  alfalfa<-wi_nass_y[(wi_nass_y$Commodity == 'HAY' & grepl('ALFALFA',wi_nass_y$Data.Item)),]
  alfalfa$Commodity <- ifelse(grepl('EXCL',alfalfa$Data.Item), 'OTHER HAY/NON ALFALFA','ALFALFA')
  
  wi_nass_y <- wi_nass_y %>% 
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('HAY', .))) %>%
    rbind(alfalfa)%>%
    group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
  
  #join together the field and the CoA data 
  names(field_data)[3]<-"Commodity"
  names(field_data)[2]<-"fieldacres"
  layer_by_year_crops<-left_join(field_data, wi_nass_y, by = c("Year","Commodity"))
  colnames(layer_by_year_crops)[8]<-"NASSacres"
  
  layer_by_year_crops<-na.omit(layer_by_year_crops) #IF there are no crops represented in NASS for that year, drop those rows
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyW[[n]]<-layer_by_year_crops
  
}

names(list_of_final_data_by_countyW)<-cntynames
for(i in 1:length(list_of_final_data_by_countyW)){
  write.csv(list_of_final_data_by_countyW[i], paste0(root_figures, "/", names(list_of_final_data_by_countyW)[i], "_finaldf_og.csv"))
}

cntynames<-c("LANGLADE","ROCK","WAUSHARA")

list_of_plotsW<-list()
list_of_dataW<-list()
for(c in 1:length(list_of_final_data_by_countyW)){
  
  layer_by_year_crops<-list_of_final_data_by_countyW[[c]]
  extracted_cdl_dataW$CDLacres<-((extracted_cdl_dataW$count)*900)*0.000247105
  colnames(extracted_cdl_dataW)[1]<-'Class'
  layer_by_year_crops<-left_join(layer_by_year_crops, extracted_cdl_dataW, by=c("County","year","Class"))
  
  #Plot showing combined data
  sum_field<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(fieldacres)))
  sum_nass<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(NASSacres)))
  sum_cdl<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(CDLacres)))
  all_data<-cbind(sum_field,sum_nass, sum_cdl)
  names(all_data)<-c("year","thresh","sum_field","year","thresh","sum_nass","year","thresh","sum_cdl")
  all_data<-all_data[,c(1:3,6,9)]
  
  #because some thresholds include crops that others don't, we need to modify the final NASS values to reflect that
  all_data_t <- all_data %>% group_by(year) %>% 
    mutate(sum_nass = ifelse(max(sum_nass) > min(sum_nass),  max(sum_nass), min(sum_nass))) 
  all_data_t <- all_data_t %>% group_by(year) %>% 
    mutate(sum_cdl = ifelse(max(sum_cdl) > min(sum_cdl),  max(sum_cdl), min(sum_cdl)))
  
  all_data_t$County<-names(field_list_wi_f)[c]
  list_of_dataW[[c]]<-all_data_t
  
  #plot showing the specific changes in acreage by year specific to each County
  
  ratio_plot<-ggplot(all_data_t, aes(x=year, y=(sum_field), fill=factor(thresh), colour=factor(thresh))) + 
    geom_point()+
    geom_line()+
    xlab("Year") +
    ylab("Acreages")+
    scale_x_discrete(name ="Year", 
                     limits=c(2008:2021))+
    scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0, .1)))+
    labs(title = paste0(names(field_list_wi_f)[c]," County Total Field Acres to NASS Acres, by Threshold"))+
    guides(fill=guide_legend(title="Threshold"), colour=guide_legend(title="Threshold"))+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ratio_plot
  
  output<-ratio_plot+
    geom_point(all_data_t,mapping=aes(y=(sum_nass)), col='black') +
    geom_line(all_data_t,mapping=aes(y=(sum_nass)), col='black')+
    geom_point(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey') +
    geom_line(all_data_t,mapping=aes(y=(sum_cdl)), col='darkgrey')
  output
  
  
  list_of_plotsW[[c]]<-output
  
}

finalw<-ggpubr::ggarrange(list_of_plotsW[[1]], list_of_plotsW[[2]], list_of_plotsW[[3]], # list of plots
                          labels = "AUTO", # labels
                          common.legend = T, # COMMON LEGEND
                          legend = "right", # legend position
                          align = "hv", # Align them both, horizontal and vertical
                          nrow = 3)  # number of rows
finalw



#Plot showing the ratio of NASS acres to field acres across years, by crop at threshold 1
final_Wisconsin_by_crop<-do.call(rbind, list_of_final_data_by_countyW)
final_Wisconsin_by_crop$Label<-paste0(tools::toTitleCase(tolower(final_Wisconsin_by_crop$County))," County, Wisconsin")

final_Wisconsin_by_crop$County <- factor(final_Wisconsin_by_crop$County , levels = c("ROCK", "WAUSHARA", "LANGLADE"))
final_Wisconsin_by_crop$ratio<-final_Wisconsin_by_crop$fieldacres/final_Wisconsin_by_crop$NASSacres

final_Wisconsin_by_crop <- final_Wisconsin_by_crop %>% 
  group_by(County, year, threshold) %>% mutate(percent = (NASSacres/sum(NASSacres))*100) #get percentage of each crop
final_Wisconsin_by_crop<-final_Wisconsin_by_crop[final_Wisconsin_by_crop$percent>4,] #get crops that make up ~95% of crop area


final_Wisconsin_by_crop<-final_Wisconsin_by_crop[final_Wisconsin_by_crop$threshold ==1, ]
# ratio_plotW<-ggplot(final_Wisconsin_by_crop, aes(x=as.factor(Commodity), y=(log2(ratio)), fill=Commodity)) +
#   geom_boxplot()+
#   facet_wrap(.~County, scales = "free")+
#   xlab("Crop") +
#   ylab("Acreage Ratio")+
#   labs(title = "Ratio NASS Acres to Field Acres, By Crop")+
#   theme(panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#         axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   theme(legend.position = "none")
# ratio_plotW

### Combine Boxplots----
final_Illinois<-do.call(rbind, list_of_dataI)
final_Illinois$County <- factor(final_Illinois$County , levels = c("Champaign", "McHenry", "DuPage"))
final_Illinois<-final_Illinois[!c(final_Illinois$County == 'McHenry' & final_Illinois$year==2021),] #remove
final_Illinois$State<-"Illinois"

final_Michigan<-do.call(rbind, list_of_dataM)
final_Michigan$County <- factor(final_Michigan$County , levels = c("Huron", "Oceana", "VanBuren"))
final_Michigan$State<-"Michigan"

final_Wisconsin<-do.call(rbind, list_of_dataW)
final_Wisconsin$County <- factor(final_Wisconsin$County , levels = c("Rock", "Waushara", "Langlade"))
final_Wisconsin$State<-"Wisconsin"

final_all<-rbind(final_Illinois,final_Michigan,final_Wisconsin)
final_all$Label<-paste0(final_all$County," County, ",final_all$State)
final_all$Label<-ifelse(final_all$Label == "VanBuren County, Michigan","Van Buren County, Michigan", final_all$Label) #correct Van Buren
#final_all$Label <- factor(final_all$Label , levels=unique(as.character(final_all$Label )) )
final_all <- transform(final_all, Label=reorder(Label, -sum_nass) ) 

nass_dat<-final_all[final_all$thresh ==1,]
nass_dat<-nass_dat %>% group_by(County) %>% mutate(avgnass =mean(sum_nass))%>% mutate(avgcdl = mean(sum_cdl))
#nass_dat$thresh<-c(1:14,1,2,5,10,1:12,14)
nass_dat$thresh<-rep(c(1,5,14,1,5,14,1,5,14),3)


t_box<-
  ggplot(final_all, aes(x = as.factor(thresh), y = sum_field, color=as.factor(thresh)))+
  geom_boxplot()+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgnass, group=1),size=1,color="black")+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgcdl, group=1),size=1,color="darkgrey")+
  facet_wrap(.~Label, scales = "free_y")+
  scale_y_continuous(n.breaks=10,expand = expansion(mult = c(0, .1)))+
  xlab("Threshold") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

t_box

#what's the total percentage change across threshold? 
final_all <- final_all %>% group_by(thresh, County) %>% 
  mutate(avgfield =mean(sum_field))%>% 
  mutate(avgnass =mean(sum_nass)) %>%
  mutate(avgcdl =mean(sum_cdl))

percent<-final_all[final_all$year ==2008,]
percent<-percent %>% group_by(County, year) %>% 
  mutate(percentf = ((max(avgfield) - min (avgfield))/max(avgfield))*100) %>%
  mutate(percentn = ((max(avgnass) - max (avgfield))/max(avgnass))*100) %>%
  mutate(percentc = ((max(avgcdl) - max (avgfield))/max(avgcdl))*100)

percent[,12:14]<-round(percent[,12:14],1)
#percent<-percent %>% group_by(County) %>% mutate(sdf=100*(sd(avgfield)/mean(avgfield)) )


percent<-percent %>% 
  group_by(County) %>% 
  mutate(heightf = min(avgfield)) %>%
  # mutate(endf = min(avgfield)) %>%
  # mutate(topf = max(avgfield)) %>%
  #mutate(avgnass = ifelse(County %in% counties, max(avgnass) + .3 * sd(avgfield)), avgnass) %>%
  mutate(heightn = avgnass ) %>%
  # mutate(endn = min(avgnass)) %>%
  # mutate(topn = max(avgnass)) %>%
  # mutate(avgcdl = ifelse(County %in% counties, max(avgcdl) + .3 * sd(avgfield)), avgcdl) %>%
  mutate(heightc = avgcdl) 
# mutate(endc = min(avgcdl)) %>%
# mutate(topc = max(avgcdl)) 


fin_box<-t_box +
  geom_line(percent, mapping=aes(x=as.factor(thresh), y=avgfield, group=Label,),size=1, alpha=0.4, color="darkblue")+
  facet_wrap(.~Label, scales = "free_y")+
  geom_text(percent, mapping=aes(x = 15.5, y = heightf, label = paste0(percentf, "%")), size= 4, col='darkblue', stat = "identity")+
  geom_text(percent, mapping=aes(x = 15, y = heightn, label = paste0(percentn, "%")), size= 4, col='black', stat = "identity")+
  geom_text(percent, mapping=aes(x = 15, y = heightc, label = paste0(percentc, "%")), size= 4, col='darkgrey',  stat = "identity")+
  coord_cartesian(xlim = c(1, 16), # This focuses the x-axis on the range of interest
                  clip = 'off') +  
  theme(panel.background = element_blank(),
        panel.spacing.x= unit(2.5, "lines"),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14, face='bold'),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14, face='bold'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(expand = c(0.1,0))

fin_box


#### Combine Plot Ratios ----
final_by_crop<-rbind(final_Illinois_by_crop,final_Michigan_by_crop,final_Wisconsin_by_crop)
#levels(final_by_crop$Label)<- levels(final_all$Label)
final_by_crop<-final_by_crop%>%group_by(Label)%>%mutate(NASSacresm = mean(NASSacres))
final_by_crop<-na.omit(final_by_crop)

#this is to add an additional rank to sort the plot the same as the boxplot above
order<-as.data.frame(cbind(as.numeric(9:1) , c("Champaign County, Illinois", "Huron County, Michigan", "Rock County, Wisconsin",
                                               'McHenry County, Illinois',"Van Buren County, Michigan","Waushara County, Wisconsin",
                                               "Oceana County, Michigan", "Langlade County, Wisconsin", "DuPage County, Illinois")))
names(order)<-c("Rank","Label")
final_by_crop<-merge(final_by_crop,order,by="Label")
final_by_crop <- transform(final_by_crop, Label=reorder(Label, -as.numeric(Rank)) ) 

ratio_plotf<-ggplot(final_by_crop, aes(x=as.factor(Commodity), y=(log2(ratio)), fill=Commodity)) +
  geom_boxplot()+
  facet_wrap(.~Label, scales = "free")+
  xlab("Crop") +
  ylab("Ratio Sum of Crop Acreage")+
  labs(title = "Ratio NASS Acres to Field Acres, By Crop")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")
ratio_plotf



final_by_crop<-final_by_crop[final_by_crop$threshold==1,]
final_by_crop$Commodity<-tools::toTitleCase(tolower(final_by_crop$Commodity))
write.csv(final_by_crop, paste0(root_figures, "/", "individualcrops", "_NASSproportioncsv"))


final_by_crop_sum<-final_by_crop %>% group_by(Label, Commodity) %>% summarise(avg = mean(percent))
write.csv(final_by_crop_sum, paste0(root_figures, "/", "individualcrops", "_NASSproportion_sum.csv"))


#### Get field size distributions ----
acreages_by_field<-list()
field_extract_wi<-list()
for(county in 1:length(field_list_wi_f)){
  county_layers<-field_list_wi_f[[county]]
  
  field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
  field_extract<-lapply(county_layers, field_area)
  field_extract_df<-lapply(field_extract, function(x) as.data.frame(do.call("cbind", x)))
  
  get_acreages<-function(y){
    y$Polygon<-row.names(y)
    colnames(y)[1]<-"Acreage"
    y$Acreage<-(y$Acreage)*0.000247105 #convert to acres
    
  }
  get_acreages_by_field<-lapply(field_extract_df, get_acreages)
  
  acreages_by_field <- purrr::map_df(get_acreages_by_field, ~as.data.frame(.x), .id="Name")
  acreages_by_field$County<-cntynames[county]
  
  field_extract_wi[[county]]<-acreages_by_field 
  
}
# acreages_by_countyI<-acreages_by_county


