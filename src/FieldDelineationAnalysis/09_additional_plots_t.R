### Probabilistic Crop Loading 

### 08 additional diagnostics

# Edited by E. Paulukonis December 2022
options(scipen = 999)



ill_coa<-read.csv(paste0(coa_dir,"/all_CoA//CoA_ILL.csv"))
mi_coa<-read.csv(paste0(coa_dir,"/all_CoA/CoA_MI.csv"))
wi_coa<-read.csv(paste0(coa_dir,"/all_CoA/CoA_WIS.csv"))


#sub the years for 2008 to be 2007, the last census prior to CDL
ill_coa$Year[ill_coa$Year ==2007 ] <- 2008
mi_coa$Year[mi_coa$Year ==2007 ] <- 2008
wi_coa$Year[wi_coa$Year ==2007 ] <- 2008

ill_nass<-ill_coa
mi_nass<-mi_coa
wi_nass<-wi_coa

ill_nass<-ill_nass[order(ill_nass$Year),]
mi_nass<-mi_nass[order(mi_nass$Year),]
wi_nass<-wi_nass[order(wi_nass$Year),]


years<-2008:2021

extracted_cdl_dataI<-readRDS(paste0(root_data_out,"/extracted_cdl_dataI.RData"))
extracted_cdl_dataM<-readRDS(paste0(root_data_out,"/extracted_cdl_dataM.RData"))
extracted_cdl_dataW<-readRDS(paste0(root_data_out,"/extracted_cdl_dataW.RData"))


field_list_ill_f<-list(1,2,3)
names(field_list_ill_f)<-c("Champaign","DuPage","McHenry")

field_list_mi_f<-list(1,2,3)
names(field_list_mi_f)<-c("Huron", "Oceana", "VanBuren")

field_list_wi_f<-list(1,2,3)
names(field_list_wi_f)<-c("Langlade","Rock","Waushara")


#### Prepare data for threshold 1 year-acreage line plots - Large ----


#Illinois
acreages_by_countyI<-readRDS(paste0(root_data_out,"/acreages_by_countyI.RData"))
cntynames<-c('CHAMPAIGN',"DU PAGE","MCHENRY")
names(acreages_by_countyI)<-cntynames

list_of_final_data_by_countyI<-list()
for(n in 1:length(acreages_by_countyI)){ 
  #county_name<-'CHAMPAIGN'
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
  
  coayears<-c(2008,2012,2017)
  layer_by_year_crops$NASSacres<-ifelse(layer_by_year_crops$Year %in% coayears, layer_by_year_crops$NASSacres, 0)
  layer_by_year_crops<-na.omit(layer_by_year_crops) 

  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyI[[n]]<-layer_by_year_crops
  
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

}

final_Illinois<-do.call(rbind, list_of_dataI)
final_Illinois <- final_Illinois %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))



#Michigan
acreages_by_countyM<-readRDS(paste0(root_data_out,"/acreages_by_countyM.RData"))
cntynames<-c("HURON", "OCEANA", "VAN BUREN")
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
  # totalhay<-ill_nass_y[(ill_nass_y$Commodity == 'HAY & HAYLAGE'),] 
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
  
  coayears<-c(2008,2012,2017)
  layer_by_year_crops$NASSacres<-ifelse(layer_by_year_crops$Year %in% coayears, layer_by_year_crops$NASSacres, 0)
  layer_by_year_crops<-na.omit(layer_by_year_crops) 
  
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyM[[n]]<-layer_by_year_crops
  
}

cntynames<-c("HURON", "OCEANA", "VAN BUREN")
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
  
}

final_Michigan<-do.call(rbind, list_of_dataM)
final_Michigan <- final_Michigan %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))


#Wisconsin
acreages_by_countyW<-readRDS(paste0(root_data_out,"/acreages_by_countyW.RData"))
cntynames<-c("LANGLADE","ROCK","WAUSHARA")
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
  # totalhay<-ill_nass_y[(ill_nass_y$Commodity == 'HAY & HAYLAGE'),] 
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
  
  coayears<-c(2008,2012,2017)
  layer_by_year_crops$NASSacres<-ifelse(layer_by_year_crops$Year %in% coayears, layer_by_year_crops$NASSacres, 0)
  layer_by_year_crops<-na.omit(layer_by_year_crops) 
  
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyW[[n]]<-layer_by_year_crops
  
}

cntynames<-c("LANGLADE","ROCK","WAUSHARA")
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

}

final_Wisconsin<-do.call(rbind, list_of_dataW)
final_Wisconsin <- final_Wisconsin %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))


#### Here is where we put everything together for threshold 1 line plots - Large----
final_I<-final_Illinois[final_Illinois$thresh==1,]
final_M<-final_Michigan[final_Michigan$thresh==1,]
final_W<-final_Wisconsin[final_Wisconsin$thresh==1,]

#refactor names
final_I$County <- factor(final_I$County , levels = c("Champaign", "McHenry", "DuPage"))
final_M$County <- factor(final_M$County , levels = c("Huron", "Oceana", "VanBuren"))
final_W$County <- factor(final_W$County , levels = c("Rock", "Waushara", "Langlade"))

#remove weird outlier from Ill
final_I$sum_nass<-ifelse(final_I$year == 2021 & final_I$County == "McHenry",0,final_I$sum_nass )
#change names for legends
final_M$County<-gsub("([a-z])([A-Z])","\\1 \\2",final_M$County)


#Illinois 
final_Ix<-tidyr::gather(final_I, "Category", "n", 3:5)
final_Ix$n<-ifelse(final_Ix$n == 0,NA,final_Ix$n)
final_Ix$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Ix$Category]
final_Ix$Category <- factor(final_Ix$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Ix)[, Combo := .GRP, .(County, Category)]
final_Ix$Combo[final_Ix$Category == 'NASS Acreage']<-4
final_Ix$Combo[final_Ix$Category == 'CDL Acreage']<-5
final_Ix$Combo <- factor(final_Ix$Combo , levels = c(1,3,2,4,5))
#this is where this is going wrong!


ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Ix$Combo)),"Set1")
names(myColors) <- levels(final_Ix$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Champaign Fields","McHenry Fields","DuPage Fields","NASS","CDL"),name = "Legend",values = myColors)

myShapes<-c(19,17,15,4,0)
shapeScale <- scale_shape_manual(labels=c("Champaign Fields","McHenry Fields","DuPage Fields","NASS","CDL"),name = "Legend",values = myShapes)

scales::show_col(myColors)
final_Ix<-na.omit(final_Ix)


line_plotI<-ggplot(final_Ix, aes(x=(year), y=log(n),group=interaction(Category,County), color =Combo, shape=Combo)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  # guides(shape = "none")+
  colScale+
  shapeScale+
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Illinois County Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotI


### Michigan
final_Mx<-tidyr::gather(final_M, "Category", "n", 3:5)
final_Mx$n<-ifelse(final_Mx$n == 0,NA,final_Mx$n)
final_Mx$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Mx$Category]
final_Mx$Category <- factor(final_Mx$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Mx)[, Combo := .GRP, .(County, Category)]
final_Mx$Combo[final_Mx$Category == 'NASS Acreage']<-4
final_Mx$Combo[final_Mx$Category == 'CDL Acreage']<-5
final_Mx$Combo <- factor(final_Mx$Combo , levels = c(1,2,3,4,5))

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Mx$Combo)),"Set1")
names(myColors) <- levels(final_Mx$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Huron Fields","Oceana Fields","Van Buren Fields","NASS","CDL"), name = "Legend",values = myColors)

myShapes<-c(19,17,15,4,0)
shapeScale <- scale_shape_manual(labels=c("Huron Fields","Oceana Fields","Van Buren Fields","NASS","CDL"),name = "Legend",values = myShapes)

final_Mx<-na.omit(final_Mx)

line_plotM<-ggplot(final_Mx, aes(x=year, y=log(n),group=interaction(Category,County), color =Combo, shape=Combo)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  colScale+
  shapeScale+
  ylab("Log(Sum of Crop Acreages)")+
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Michigan Counties Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotM


### Wisconsin
final_Wx<-tidyr::gather(final_W, "Category", "n", 3:5)
final_Wx$n<-ifelse(final_Wx$n == 0,NA,final_Wx$n)
final_Wx$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Wx$Category]
final_Wx$Category <- factor(final_Wx$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Wx)[, Combo := .GRP, .(County, Category)]
final_Wx$Combo[final_Wx$Category == 'NASS Acreage']<-4
final_Wx$Combo[final_Wx$Category == 'CDL Acreage']<-5
final_Wx$Combo <- factor(final_Wx$Combo , levels = c(2,3,1,4,5))

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Wx$Combo)),"Set1")
names(myColors) <- levels(final_Wx$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Rock Fields","Waushara Fields","Langlade Fields","NASS","CDL"), name = "Legend",values = myColors)

myShapes<-c(19,17,15,4,0)
shapeScale <- scale_shape_manual(labels=c("Rock Fields","Waushara Fields","Langlade Fields","NASS","CDL"),name = "Legend",values = myShapes)


final_Wx<-na.omit(final_Wx)

line_plotW<-ggplot(final_Wx, aes(x=year, y=log(n),group=interaction(Category,County), color =Combo, shape=Combo)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  colScale+
  shapeScale+
  xlab("Year") +
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Wisconsin Counties Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotW


ggpubr::ggarrange(line_plotI, line_plotM, line_plotW, # list of plots
                  #labels = 1, # labels
                  #common.legend = T, # COMMON LEGEND
                  #legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 1)  # number of rows








#### Prepare data for threshold 1 year-acreage line plots - Small ----

#Illinois
acreages_by_countyI<-readRDS(paste0(root_data_out,"/acreages_by_county_sfI.RData"))
cntynames<-c('CHAMPAIGN',"DU PAGE","MCHENRY")
names(acreages_by_countyI)<-cntynames

list_of_final_data_by_countyI<-list()
for(n in 1:length(acreages_by_countyI)){ 
  #county_name<-'CHAMPAIGN'
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
  
  coayears<-c(2008,2012,2017)
  layer_by_year_crops$NASSacres<-ifelse(layer_by_year_crops$Year %in% coayears, layer_by_year_crops$NASSacres, 0)
  layer_by_year_crops<-na.omit(layer_by_year_crops) 
  
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyI[[n]]<-layer_by_year_crops
  
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
  
}

final_Illinois<-do.call(rbind, list_of_dataI)
final_Illinois <- final_Illinois %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))



#Michigan
acreages_by_countyM<-readRDS(paste0(root_data_out,"/acreages_by_county_sfM.RData"))
cntynames<-c("HURON", "OCEANA", "VAN BUREN")
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
  # totalhay<-ill_nass_y[(ill_nass_y$Commodity == 'HAY & HAYLAGE'),] 
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
  
  coayears<-c(2008,2012,2017)
  layer_by_year_crops$NASSacres<-ifelse(layer_by_year_crops$Year %in% coayears, layer_by_year_crops$NASSacres, 0)
  layer_by_year_crops<-na.omit(layer_by_year_crops) 
  
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyM[[n]]<-layer_by_year_crops
  
}

cntynames<-c("HURON", "OCEANA", "VAN BUREN")
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
  
}

final_Michigan<-do.call(rbind, list_of_dataM)
final_Michigan <- final_Michigan %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))


#Wisconsin
acreages_by_countyW<-readRDS(paste0(root_data_out,"/acreages_by_county_sfW.RData"))
cntynames<-c("LANGLADE","ROCK","WAUSHARA")
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
  # totalhay<-ill_nass_y[(ill_nass_y$Commodity == 'HAY & HAYLAGE'),] 
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
  
  coayears<-c(2008,2012,2017)
  layer_by_year_crops$NASSacres<-ifelse(layer_by_year_crops$Year %in% coayears, layer_by_year_crops$NASSacres, 0)
  layer_by_year_crops<-na.omit(layer_by_year_crops) 
  
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyW[[n]]<-layer_by_year_crops
  
}

cntynames<-c("LANGLADE","ROCK","WAUSHARA")
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
  
}

final_Wisconsin<-do.call(rbind, list_of_dataW)
final_Wisconsin <- final_Wisconsin %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))


#### Here is where we put everything together for threshold 1 line plots - Small ----
final_I<-final_Illinois[final_Illinois$thresh==1,]
final_M<-final_Michigan[final_Michigan$thresh==1,]
final_W<-final_Wisconsin[final_Wisconsin$thresh==1,]

#refactor names
final_I$County <- factor(final_I$County , levels = c("Champaign", "McHenry", "DuPage"))
final_M$County <- factor(final_M$County , levels = c("Huron", "Oceana", "VanBuren"))
final_W$County <- factor(final_W$County , levels = c("Rock", "Waushara", "Langlade"))

#remove weird outlier from Ill
final_I$sum_nass<-ifelse(final_I$year == 2021 & final_I$County == "McHenry",0,final_I$sum_nass )
#change names for legends
final_M$County<-gsub("([a-z])([A-Z])","\\1 \\2",final_M$County)


#Illinois 
final_Ix<-tidyr::gather(final_I, "Category", "n", 3:5)
final_Ix$n<-ifelse(final_Ix$n == 0,NA,final_Ix$n)
final_Ix$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Ix$Category]
final_Ix$Category <- factor(final_Ix$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Ix)[, Combo := .GRP, .(County, Category)]
final_Ix$Combo[final_Ix$Category == 'NASS Acreage']<-4
final_Ix$Combo[final_Ix$Category == 'CDL Acreage']<-5
final_Ix$Combo <- factor(final_Ix$Combo , levels = c(1,3,2,4,5))
#this is where this is going wrong!

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Ix$Combo)),"Set1")
names(myColors) <- levels(final_Ix$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Champaign Fields","McHenry Fields","DuPage Fields","NASS","CDL"),name = "Legend",values = myColors)

myShapes<-c(19,17,15,4,0)
shapeScale <- scale_shape_manual(labels=c("Champaign Fields","McHenry Fields","DuPage Fields","NASS","CDL"),name = "Legend",values = myShapes)


scales::show_col(myColors)
final_Ix<-na.omit(final_Ix)

line_plotI<-ggplot(final_Ix, aes(x=(year), y=log(n),group=interaction(Category,County), color =Combo, shape=Combo)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  colScale+
  shapeScale+
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Illinois County Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotI


### Michigan
final_Mx<-tidyr::gather(final_M, "Category", "n", 3:5)
final_Mx$n<-ifelse(final_Mx$n == 0,NA,final_Mx$n)
final_Mx$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Mx$Category]
final_Mx$Category <- factor(final_Mx$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Mx)[, Combo := .GRP, .(County, Category)]
final_Mx$Combo[final_Mx$Category == 'NASS Acreage']<-4
final_Mx$Combo[final_Mx$Category == 'CDL Acreage']<-5
final_Mx$Combo <- factor(final_Mx$Combo , levels = c(1,2,3,4,5))

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Mx$Combo)),"Set1")
names(myColors) <- levels(final_Mx$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Huron Fields","Oceana Fields","Van Buren Fields","NASS","CDL"), name = "Legend",values = myColors)

myShapes<-c(19,17,15,4,0)
shapeScale <- scale_shape_manual(labels=c("Huron Fields","Oceana Fields","Van Buren Fields","NASS","CDL"),name = "Legend",values = myShapes)

final_Mx<-na.omit(final_Mx)

line_plotM<-ggplot(final_Mx, aes(x=year, y=log(n),group=interaction(Category,County), color =Combo, shape=Combo)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  colScale+
  shapeScale+
  ylab("Log(Sum of Crop Acreages)")+
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Michigan Counties Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotM


### Wisconsin
final_Wx<-tidyr::gather(final_W, "Category", "n", 3:5)
final_Wx$n<-ifelse(final_Wx$n == 0,NA,final_Wx$n)
final_Wx$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Wx$Category]
final_Wx$Category <- factor(final_Wx$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Wx)[, Combo := .GRP, .(County, Category)]
final_Wx$Combo[final_Wx$Category == 'NASS Acreage']<-4
final_Wx$Combo[final_Wx$Category == 'CDL Acreage']<-5
final_Wx$Combo <- factor(final_Wx$Combo , levels = c(2,3,1,4,5))

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Wx$Combo)),"Set1")
names(myColors) <- levels(final_Wx$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Rock Fields","Waushara Fields","Langlade Fields","NASS","CDL"), name = "Legend",values = myColors)

myShapes<-c(19,17,15,4,0)
shapeScale <- scale_shape_manual(labels=c("Rock Fields","Waushara Fields","Langlade Fields","NASS","CDL"),name = "Legend",values = myShapes)

final_Wx<-na.omit(final_Wx)

line_plotW<-ggplot(final_Wx, aes(x=year, y=log(n),group=interaction(Category,County), color =Combo, shape=Combo)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  colScale+
  shapeScale+
  xlab("Year") +
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Wisconsin Counties Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotW


ggpubr::ggarrange(line_plotI, line_plotM, line_plotW, # list of plots
                  #labels = 1, # labels
                  #common.legend = T, # COMMON LEGEND
                  #legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 1)  # number of rows
















#### Get field size distributions ----
#### Large fields
#Illinois
# print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# ill_field<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# ill_field<-setNames(lapply(ill_field, readOGR), tools::file_path_sans_ext(basename(ill_field)))
# ill_fieldf<-ill_field[(mixedsort(as.character(names(ill_field))))]
# 
# field_list_ill_f<-list()
# field_list_ill_f[[1]]<-ill_field[1:14]
# field_list_ill_f[[2]]<-ill_field[15:28]
# field_list_ill_f[[3]]<-ill_field[29:42]
# names(field_list_ill_f)<-c("Champaign","DuPage","McHenry")
# #rm(ill_field)
# 
# foreach(i=1:3) %do% {
#    areaf<-sapply(field_list_ill_f[[i]],function(x) sum(terra::area(x)))
#    field_list_ill_f[[i]]<-field_list_ill_f[[i]][order(areaf)]
#     names(field_list_ill_f[[i]])<-paste0(names(field_list_ill_f[i]),14:1,"fin")
# }
# 
# saveRDS(field_list_ill_f, file=paste0(root_data_out,"/all_tif/ILLINOIS/field_data_I.RData"))
# 
# #Michigan
# print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# mi_field<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# mi_field<-setNames(lapply(mi_field, readOGR), tools::file_path_sans_ext(basename(mi_field)))
# mi_fieldf<-mi_field[order(mixedsort(names(mi_field)))]
# 
# field_list_mi_f<-list(1,2,3)
# field_list_mi_f[[1]]<-mi_field[1:14]
# field_list_mi_f[[2]]<-mi_field[15:28]
# field_list_mi_f[[3]]<-mi_field[29:42]
# names(field_list_mi_f)<-c("Huron", "Oceana", "VanBuren")
# # rm(mi_field)
# 
# foreach(i=1:3) %do% {
#   areaf<-sapply(field_list_mi_f[[i]],function(x) sum(terra::area(x)))
#   field_list_mi_f[[i]]<-field_list_mi_f[[i]][order(areaf)]
#   names(field_list_mi_f[[i]])<-paste0(names(field_list_mi_f[i]),14:1,"fin")
# }
# 
# saveRDS(field_list_mi_f, file=paste0(root_data_out,"/all_tif/MICHIGAN/field_data_M.RData"))
# 
# #Wisconsin
# print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB"), pattern='.shp', all.files=TRUE, full.names=FALSE))
# wi_field<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN/SUB"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# wi_field<-setNames(lapply(wi_field, readOGR), tools::file_path_sans_ext(basename(wi_field)))
# wi_field<-wi_field[order(mixedsort(names(wi_field)))]
# 
# field_list_wi_f<-list(1,2,3)
# field_list_wi_f[[1]]<-wi_field[1:14]
# field_list_wi_f[[2]]<-wi_field[15:28]
# field_list_wi_f[[3]]<-wi_field[29:42]
# names(field_list_wi_f)<-c("Langlade","Rock","Waushara")
# #rm(wi_field)
# 
# foreach(i=1:3) %do% {
#   areaf<-sapply(field_list_wi_f[[i]],function(x) sum(terra::area(x)))
#   field_list_wi_f[[i]]<-field_list_wi_f[[i]][order(areaf)]
#   names(field_list_wi_f[[i]])<-paste0(names(field_list_wi_f[i]),14:1,"fin")
# }
# 
# saveRDS(field_list_wi_f, file=paste0(root_data_out,"/all_tif/WISCONSIN/field_data_W.RData"))



field_list_ill_f <-readRDS(paste0(root_data_out,"/all_tif/ILLINOIS/field_data_I.RData"))
field_list_mi_f  <-readRDS(paste0(root_data_out,"/all_tif/MICHIGAN/field_data_M.RData"))
field_list_wi_f  <-readRDS(paste0(root_data_out,"/all_tif/WISCONSIN/field_data_W.RData"))


### extract fields
acreages_by_field<-list()
field_extract_ill<-list()
for(county in 1:length(field_list_ill_f)){
  county_layers<-field_list_ill_f[[county]]
  field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
  field_extract<-lapply(county_layers, field_area)
  field_extract_df<-lapply(field_extract, function(x) as.data.frame(do.call("cbind", x)))
  get_acreages<-function(y){
    colnames(y)[1]<-"Acreage"
    y$Polygon<-row.names(y)
    y$Acreage<-(y$Acreage)*0.000247105 #convert to acres
    
  }
  get_acreages_by_field<-lapply(field_extract_df, get_acreages)
  acreages_by_field <- purrr::map_df(get_acreages_by_field, ~as.data.frame(.x), .id="Name")
  colnames(acreages_by_field)[2]<-"Acreage"
  acreages_by_field$County<-names(field_list_ill_f[county])
  field_extract_ill[[county]]<-acreages_by_field 
}

fields_ill<-do.call(rbind, field_extract_ill)


acreages_by_field<-list()
field_extract_mi<-list()
for(county in 1:length(field_list_mi_f)){
  county_layers<-field_list_mi_f[[county]]
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
  colnames(acreages_by_field)[2]<-"Acreage"
  acreages_by_field$County<-names(field_list_mi_f[county])
  field_extract_mi[[county]]<-acreages_by_field 
}

fields_mi<-do.call(rbind, field_extract_mi)

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
  colnames(acreages_by_field)[2]<-"Acreage"
  acreages_by_field$County<-names(field_list_wi_f[county])
  field_extract_wi[[county]]<-acreages_by_field 
}

fields_wi<-do.call(rbind, field_extract_wi)

# fields_ill$County<- factor(fields_ill$County , levels = c("Champaign", "McHenry", "DuPage"))
# fields_mi$County<- factor(fields_mi$County , levels = c("Huron", "VanBuren","Oceana"))
# fields_wi$County<- factor(fields_wi$County , levels = c("Rock", "Waushara", "Langlade"))



# colnames(fields_ill)[2]<-"FieldSize"
# dup<-fields_ill[fields_ill$County == "DuPage",]
# champ<-fields_ill[fields_ill$County == "Champaign",]
# 
# ggplot(fields_ill, aes(x=Acreage, y=County, colour = Name)) +
#   geom_density_ridges(scale = 1) +
#   scale_x_continuous(n.breaks=5, limits=c(0,300))+
#   theme(legend.position = "none")   


# ggplot(dup, aes(x=Acreage, y=Name, colour = Name)) +
#   geom_density_ridges(scale = 1) +
#   scale_x_continuous(n.breaks=5, limits=c(0,300))+
#   theme(legend.position = "none") 
# 
# ggplot(champ, aes(x=Acreage, y=Name, colour = Name)) +
#   geom_density_ridges(scale = 1) +
#   scale_x_continuous(n.breaks=5, limits=c(0,300))+
#   theme(legend.position = "none") 


# ill<-ggplot(fields_ill, aes(x=Acreage, colour = County, group=Name)) +
#   geom_density() +
#   scale_x_continuous(n.breaks=5, limits=c(0,300))+
#   theme_minimal()
# 
# mi<-ggplot(fields_mi, aes(x=Acreage, colour = County, group=Name)) +
#   geom_density() +
#   scale_x_continuous(n.breaks=5, limits=c(0,150))+
#   theme_minimal()
# 
# wi<-ggplot(fields_wi, aes(x=Acreage, colour = County, group=Name)) +
#   geom_density() +
#   scale_x_continuous(n.breaks=5, limits=c(0,150))+
#   theme_minimal()
# 
# final<-ggpubr::ggarrange(ill,mi,wi, # list of plots
#                           legend = "right", # legend position
#                           align = "hv", # Align them both, horizontal and vertical
#                           nrow = 3)  # number of rows
# 
# final



#### Small fields
#Illinois
# print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# ill_field<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# ill_field<-setNames(lapply(ill_field, readOGR), tools::file_path_sans_ext(basename(ill_field)))
# ill_fieldf<-ill_field[(mixedsort(as.character(names(ill_field))))]
# 
# field_list_ill_sf<-list()
# field_list_ill_sf[[1]]<-ill_field[1:14]
# field_list_ill_sf[[2]]<-ill_field[15:28]
# field_list_ill_sf[[3]]<-ill_field[29:42]
# names(field_list_ill_sf)<-c("Champaign","DuPage","McHenry")
# #rm(ill_field)
# 
# foreach(i=1:3) %do% {
#   areaf<-sapply(field_list_ill_sf[[i]],function(x) sum(terra::area(x)))
#   field_list_ill_sf[[i]]<-field_list_ill_sf[[i]][order(areaf)]
#   names(field_list_ill_sf[[i]])<-paste0(names(field_list_ill_sf[i]),14:1,"fin")
# }
# 
# saveRDS(field_list_ill_sf, file=paste0(root_data_out,"/all_tif/ILLINOIS/field_data_Isf.RData"))
# 
# #Michigan
# print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# mi_field<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# mi_field<-setNames(lapply(mi_field, readOGR), tools::file_path_sans_ext(basename(mi_field)))
# mi_fieldf<-mi_field[order(mixedsort(names(mi_field)))]
# 
# field_list_mi_sf<-list(1,2,3)
# field_list_mi_sf[[1]]<-mi_field[1:14]
# field_list_mi_sf[[2]]<-mi_field[15:28]
# field_list_mi_sf[[3]]<-mi_field[29:42]
# names(field_list_mi_sf)<-c("Huron", "Oceana", "VanBuren")
# # rm(mi_field)
# 
# foreach(i=1:3) %do% {
#   areaf<-sapply(field_list_mi_sf[[i]],function(x) sum(terra::area(x)))
#   field_list_mi_sf[[i]]<-field_list_mi_sf[[i]][order(areaf)]
#   names(field_list_mi_sf[[i]])<-paste0(names(field_list_mi_sf[i]),14:1,"fin")
# }
# 
# saveRDS(field_list_mi_sf, file=paste0(root_data_out,"/all_tif/MICHIGAN/field_data_Msf.RData"))
# 
# #Wisconsin
# print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), pattern='.shp', all.files=TRUE, full.names=FALSE))
# wi_field<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# wi_field<-setNames(lapply(wi_field, readOGR), tools::file_path_sans_ext(basename(wi_field)))
# wi_field<-wi_field[order(mixedsort(names(wi_field)))]
# 
# field_list_wi_sf<-list(1,2,3)
# field_list_wi_sf[[1]]<-wi_field[1:14]
# field_list_wi_sf[[2]]<-wi_field[15:28]
# field_list_wi_sf[[3]]<-wi_field[29:42]
# names(field_list_wi_sf)<-c("Langlade","Rock","Waushara")
# #rm(wi_field)
# 
# foreach(i=1:3) %do% {
#   areaf<-sapply(field_list_wi_sf[[i]],function(x) sum(terra::area(x)))
#   field_list_wi_sf[[i]]<-field_list_wi_sf[[i]][order(areaf)]
#   names(field_list_wi_sf[[i]])<-paste0(names(field_list_wi_sf[i]),14:1,"fin")
# }
# 
# saveRDS(field_list_wi_sf, file=paste0(root_data_out,"/all_tif/WISCONSIN/field_data_Wsf.RData"))

field_list_ill_sf <-readRDS(paste0(root_data_out,"/all_tif/ILLINOIS/field_data_Isf.RData"))
field_list_mi_sf  <-readRDS(paste0(root_data_out,"/all_tif/MICHIGAN/field_data_Msf.RData"))
field_list_wi_sf  <-readRDS(paste0(root_data_out,"/all_tif/WISCONSIN/field_data_Wsf.RData"))


### extract fields
acreages_by_field<-list()
field_extract_ill<-list()
for(county in 1:length(field_list_ill_sf)){
  county_layers<-field_list_ill_sf[[county]]
  field_area<- function(x){as.data.frame(terra::area(x), na.rm=T) }
  field_extract<-lapply(county_layers, field_area)
  field_extract_df<-lapply(field_extract, function(x) as.data.frame(do.call("cbind", x)))
  get_acreages<-function(y){
    colnames(y)[1]<-"Acreage"
    y$Polygon<-row.names(y)
    y$Acreage<-(y$Acreage)*0.000247105 #convert to acres
    
  }
  get_acreages_by_field<-lapply(field_extract_df, get_acreages)
  acreages_by_field <- purrr::map_df(get_acreages_by_field, ~as.data.frame(.x), .id="Name")
  colnames(acreages_by_field)[2]<-"Acreage"
  acreages_by_field$County<-names(field_list_ill_sf[county])
  field_extract_ill[[county]]<-acreages_by_field 
}

fields_ill_sf<-do.call(rbind, field_extract_ill)


acreages_by_field<-list()
field_extract_mi<-list()
for(county in 1:length(field_list_mi_sf)){
  county_layers<-field_list_mi_sf[[county]]
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
  colnames(acreages_by_field)[2]<-"Acreage"
  acreages_by_field$County<-names(field_list_mi_sf[county])
  field_extract_mi[[county]]<-acreages_by_field 
}

fields_mi_sf<-do.call(rbind, field_extract_mi)

acreages_by_field<-list()
field_extract_wi<-list()
for(county in 1:length(field_list_wi_sf)){
  county_layers<-field_list_wi_sf[[county]]
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
  colnames(acreages_by_field)[2]<-"Acreage"
  acreages_by_field$County<-names(field_list_wi_sf[county])
  field_extract_wi[[county]]<-acreages_by_field 
}

fields_wi_sf<-do.call(rbind, field_extract_wi)

# fields_ill_sf$County<- factor(fields_ill_sf$County , levels = c("Champaign", "McHenry", "DuPage"))
# fields_mi_sf$County<- factor(fields_mi_sf$County , levels = c("Huron", "VanBuren","Oceana"))
# fields_wi_sf$County<- factor(fields_wi_sf$County , levels = c("Rock", "Waushara", "Langlade"))



# illsf<-ggplot(fields_ill_sf, aes(x=Acreage, colour = County, group=Name)) +
#   geom_density() +
#   scale_x_continuous(n.breaks=5, limits=c(0,200))+
#   theme_minimal()
# 
# 
# misf<-ggplot(fields_mi_sf, aes(x=Acreage, colour = County, group=Name)) +
#   geom_density() +
#   scale_x_continuous(n.breaks=5, limits=c(0,100))+
#   theme_minimal()
# 
# wisf<-ggplot(fields_wi_sf, aes(x=Acreage, colour = County, group=Name)) +
#   geom_density() +
#   scale_x_continuous(n.breaks=5, limits=c(0,100))+
#   theme_minimal()
# 
# finalsf<-ggpubr::ggarrange(illsf,misf,wisf, # list of plots
#                          legend = "right", # legend position
#                          align = "hv", # Align them both, horizontal and vertical
#                          nrow = 3)  # number of rows
# 
# finalsf


# ## combine
# ggpubr::ggarrange(final,finalsf,# list of plots
#                   labels=NULL,
#                   common.legend = T,
#                   legend = "right", # legend position
#                   align = "hv", # Align them both, horizontal and vertical
#                   ncol=2,
#                   nrow = 1)  # number of rows



fields_ill_s<-fields_ill[fields_ill$Name %in% c('Champaign1fin','DuPage1fin','McHenry1fin'),]
fields_ill_s$Hyperparameter<-"Large"
fields_ill_sf_s<-fields_ill_sf[fields_ill_sf$Name %in% c('Champaign1fin','DuPage1fin','McHenry1fin'),]
fields_ill_sf_s$Hyperparameter<-"Small"
fields_illinois<-rbind(fields_ill_s,fields_ill_sf_s)
fields_illinois$State<-"Illinois"
fields_illinois$County<- factor(fields_illinois$County , levels = c("Champaign", "McHenry", "DuPage"))


ill_fields<-ggplot(fields_illinois, aes(x=Acreage, color=County, linetype=Hyperparameter)) +
  geom_density(size = 1) +
  scale_x_continuous(n.breaks=5, limits=c(0,150))+
  theme_minimal()

ill_fields



fields_mi_s<-fields_mi[fields_mi$Name %in% c('Huron1fin','VanBuren1fin','Oceana1fin'),]
fields_mi_s$Hyperparameter<-"Large"
fields_mi_sf_s<-fields_mi_sf[fields_mi_sf$Name %in% c('Huron1fin','VanBuren1fin','Oceana1fin'),]
fields_mi_sf_s$Hyperparameter<-"Small"
fields_michigan<-rbind(fields_mi_s,fields_mi_sf_s)
fields_michigan$State<-"Michigan"
fields_michigan$County<- factor(fields_michigan$County , levels = c("Huron", "VanBuren","Oceana"))


mi_fields<-ggplot(fields_michigan, aes(x=Acreage, color=County, linetype=Hyperparameter)) +
  geom_density(size = 1) +
  scale_x_continuous(n.breaks=5, limits=c(0,150))+
  theme_minimal()

mi_fields



fields_wi_s<-fields_wi[fields_wi$Name %in% c('Rock1fin','Waushara1fin','Langlade1fin'),]
fields_wi_s$Hyperparameter<-"Large"
fields_wi_sf_s<-fields_wi_sf[fields_wi_sf$Name %in% c('Rock1fin','Waushara1fin','Langlade1fin'),]
fields_wi_sf_s$Hyperparameter<-"Small"
fields_wisconsin<-rbind(fields_wi_s,fields_wi_sf_s)
fields_wisconsin$State<-"Wisconsin"
fields_wisconsin$County<- factor(fields_wisconsin$County , levels = c("Rock", "Waushara", "Langlade"))


wi_fields<-ggplot(fields_wisconsin, aes(x=Acreage, color=County, linetype=Hyperparameter)) +
  geom_density(size = 1) +
  scale_x_continuous(n.breaks=5, limits=c(0,150))+
  theme_minimal()

wi_fields



labels<-c("Illinois","Michigan","Wisconsin")

ggpubr::ggarrange(ill_fields,mi_fields,wi_fields,# list of plots
                  labels=labels,

                  legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol=1,
                  nrow = 3)  # number of rows




#### Get CDL acreages with SD for Table 1 ----

##Illinois
extracted_cdl_dataIf<-readRDS(paste0(root_data_out,"/extracted_cdl_dataI_acreage.RData"))
# county_field_list<-list()
# for(county in 1:length(field_list_ill_f)){
# cropx<-field_list_ill_f[[county]][[1]]
# county_set<-lapply(cdl_data_ill_rec, function(x) crop(x, cropx)) #crop and mask the fixed CDL to the counties, put in list
# county_field_list[[county]]<-stack(county_set)
# names(county_field_list)[[county]]<-names(field_list_ill_f[county])
#
# }
#
#
# years <- list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
# extracted_cdl_data<-list()
# #names(county_set_list)<-c("DU PAGE","CHAMPAIGN","MCHENRY") #order kind of wonky
# for(county in 1:length(county_field_list)){
#   county_layers<-county_field_list[[county]]
#   cdl_extract<-freq(county_layers)
#   cdl_extract<-Map(cbind, cdl_extract, year = years)
#   cdl_extract<-as.data.frame(do.call(rbind,cdl_extract))
#   cdl_extract$County<-names(county_field_list[county])
#  extracted_cdl_data[[county]] <-cdl_extract
# }
#
# extracted_cdl_dataI<-do.call(rbind, extracted_cdl_data)
# saveRDS(extracted_cdl_dataI, file=paste0(root_data_out,"/extracted_cdl_dataI_acreage.RData"))


##Michigan
extracted_cdl_dataMf<-readRDS(paste0(root_data_out,"/extracted_cdl_dataM_acreage.RData"))

# county_field_list<-list()
# for(county in 1:length(field_list_mi_f)){
# cropx<-field_list_mi_f[[county]][[1]]
# county_set<-lapply(cdl_data_mi_rec, function(x) crop(x, cropx)) #crop and mask the fixed CDL to the counties, put in list
# county_field_list[[county]]<-stack(county_set)
# names(county_field_list)[[county]]<-names(field_list_mi_f[county])
# 
# }
# 
# 
# years <- list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
# extracted_cdl_data<-list()
# #names(county_set_list)<-c("DU PAGE","CHAMPAIGN","MCHENRY") #order kind of wonky
# for(county in 1:length(county_field_list)){
#   county_layers<-county_field_list[[county]]
#   cdl_extract<-freq(county_layers)
#   cdl_extract<-Map(cbind, cdl_extract, year = years)
#   cdl_extract<-as.data.frame(do.call(rbind,cdl_extract))
#   cdl_extract$County<-names(county_field_list[county])
#  extracted_cdl_data[[county]] <-cdl_extract
# }
# 
# extracted_cdl_dataM<-do.call(rbind, extracted_cdl_data)
# saveRDS(extracted_cdl_dataM, file=paste0(root_data_out,"/extracted_cdl_dataM_acreage.RData"))


##Michigan
extracted_cdl_dataWf<-readRDS(paste0(root_data_out,"/extracted_cdl_dataW_acreage.RData"))

# county_field_list<-list()
# for(county in 1:length(field_list_wi_f)){
# cropx<-field_list_wi_f[[county]][[1]]
# county_set<-lapply(cdl_data_wi_rec, function(x) crop(x, cropx)) #crop and mask the fixed CDL to the counties, put in list
# county_field_list[[county]]<-stack(county_set)
# names(county_field_list)[[county]]<-names(field_list_wi_f[county])
# 
# }
# 
# 
# years <- list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
# extracted_cdl_data<-list()
# #names(county_set_list)<-c("DU PAGE","CHAMPAIGN","MCHENRY") #order kind of wonky
# for(county in 1:length(county_field_list)){
#   county_layers<-county_field_list[[county]]
#   cdl_extract<-freq(county_layers)
#   cdl_extract<-Map(cbind, cdl_extract, year = years)
#   cdl_extract<-as.data.frame(do.call(rbind,cdl_extract))
#   cdl_extract$County<-names(county_field_list[county])
#  extracted_cdl_data[[county]] <-cdl_extract
# }
# 
# extracted_cdl_dataW<-do.call(rbind, extracted_cdl_data)
# saveRDS(extracted_cdl_dataW, file=paste0(root_data_out,"/extracted_cdl_dataW_acreage.RData"))


# 
# cdl_I<-(extracted_cdl_dataIf)
# cdl_M<-(extracted_cdl_dataMf)
# cdl_W<-(extracted_cdl_dataWf)
# 
# cdl_I[is.na(cdl_I)]<-0
# cdl_M[is.na(cdl_M)]<-0
# cdl_W[is.na(cdl_W)]<-0
# 
# 
# cdl_I$acres<-(cdl_I$count*900)*0.000247105
# cdl_M$acres<-(cdl_M$count*900)*0.000247105
# cdl_W$acres<-(cdl_W$count*900)*0.000247105
# 
# 
# 
# cdl_I_grp<-cdl_I %>% 
#   group_by(year, County) %>% 
#   mutate(sumv = ifelse(value != 0, sum(acres[value != 0]) , acres)) %>%
#   mutate(sumall = sum(unique(sumv))) %>%
#   mutate(percent = (sumv/sumall)*100)
# 
# cdl_I_grp<-cdl_I_grp %>% 
#   group_by(County) %>%
#   mutate(meanp = ifelse(value != 0, mean(percent[value !=0]), NA))%>%
#   mutate(sdp = ifelse(value != 0, sd(percent[value !=0]), NA))
# 
# 
# 
# cdl_M_grp<-cdl_M %>% 
#   group_by(year, County) %>% 
#   mutate(sumv = ifelse(value != 0, sum(acres[value != 0]) , acres)) %>%
#   mutate(sumall = sum(unique(sumv))) %>%
#   mutate(percent = (sumv/sumall)*100)
# 
# cdl_M_grp<-cdl_M_grp %>% 
#   group_by(County) %>%
#   mutate(meanp = ifelse(value != 0, mean(percent[value !=0]), NA))%>%
#   mutate(sdp = ifelse(value != 0, sd(percent[value !=0]), NA))
# 
# 
# cdl_W_grp<-cdl_W %>% 
#   group_by(year, County) %>% 
#   mutate(sumv = ifelse(value != 0, sum(acres[value != 0]) , acres)) %>%
#   mutate(sumall = sum(unique(sumv))) %>%
#   mutate(percent = (sumv/sumall)*100)
# 
# cdl_W_grp<-cdl_W_grp %>% 
#   group_by(County) %>%
#   mutate(meanp = ifelse(value != 0, mean(percent[value !=0]), NA))%>%
#   mutate(sdp = ifelse(value != 0, sd(percent[value !=0]), NA))
# 
# 
# cdl_I_grp<-cdl_I_grp[cdl_I_grp$value == 1 & cdl_I_grp$year == 2008,]
# cdl_M_grp<-cdl_M_grp[cdl_M_grp$value == 1 & cdl_M_grp$year == 2008,]
# cdl_W_grp<-cdl_W_grp[cdl_W_grp$value == 1 & cdl_W_grp$year == 2008,]





#### Get threshold distributions for Figure----

print(list.files(path=paste0(root_data_out,"/all_thresh/Illinois/plot"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
thresh_ill<- file.path(paste0(root_data_out,"/all_thresh/Illinois/plot"), list.files(path=paste0(root_data_out,"/all_thresh/Illinois/plot"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
thresh_list_ill<-setNames(lapply(thresh_ill, read.csv), tools::file_path_sans_ext(basename(thresh_ill)))
thresh_list_ill<-lapply(thresh_list_ill, function(y) { y["X"] <- NULL; y })

print(list.files(path=paste0(root_data_out,"/all_thresh/Michigan/plot"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
thresh_mi<- file.path(paste0(root_data_out,"/all_thresh/Michigan/plot"), list.files(path=paste0(root_data_out,"/all_thresh/Michigan/plot"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
thresh_list_mi<-setNames(lapply(thresh_mi, read.csv), tools::file_path_sans_ext(basename(thresh_mi)))
thresh_list_mi<-lapply(thresh_list_mi, function(y) { y["X"] <- NULL; y })

print(list.files(path=paste0(root_data_out,"/all_thresh/Wisconsin/plot"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
thresh_wi<- file.path(paste0(root_data_out,"/all_thresh/Wisconsin/plot"), list.files(path=paste0(root_data_out,"/all_thresh/Wisconsin/plot"), pattern='.csv$', all.files=TRUE, full.names=FALSE))
thresh_list_wi<-setNames(lapply(thresh_wi, read.csv), tools::file_path_sans_ext(basename(thresh_wi)))
thresh_list_wi<-lapply(thresh_list_wi, function(y) { y["X"] <- NULL; y })

master_list<-list(thresh_list_ill,thresh_list_mi,thresh_list_wi)

#Illinois
  thresh_list<-master_list[[1]]
  state_data<-do.call(rbind, thresh_list)
  morder<-c("Champaign", "McHenry", "DuPage")
  state_data$county<- factor(state_data$county , levels = (morder))
  names(state_data)[9]<-"County"
  
  med<-state_data %>% group_by(County) %>% mutate(med=median(bin))

  ill<-ggplot(state_data, aes(x = bin, y=County, fill = County)) +
      geom_density_ridges(stat = "binline", bins = 20, scale = 2,alpha = 0.7) +
    scale_x_continuous(name ="Crop Pixel Frequency", 
                       limits=c(0,15), breaks=1:14)+
      theme_minimal()+
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
            
#Michigan
  thresh_list<-master_list[[2]]
  state_data<-do.call(rbind, thresh_list)
  morder<-c("Huron", "VanBuren","Oceana")
  state_data$county<- factor(state_data$county , levels = (morder))
  names(state_data)[9]<-"County"
  
  med<-state_data %>% group_by(County) %>% mutate(med=median(bin))
  
  mi<-ggplot(state_data, aes(x = bin, y=County, fill = County)) +
    geom_density_ridges(stat = "binline", bins = 20, scale = 2,alpha = 0.7) +
    scale_x_continuous(name ="Crop Pixel Frequency", 
                       limits=c(0,15), breaks=1:14)+
    theme_minimal()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
  
  #Wisconsin
  thresh_list<-master_list[[3]]
  state_data<-do.call(rbind, thresh_list)
  morder<- c("Rock", "Waushara", "Langlade")
  state_data$county<- factor(state_data$county , levels = (morder))
  names(state_data)[9]<-"County"
  
  med<-state_data %>% group_by(County) %>% mutate(med=median(bin))
  
  wi<-ggplot(state_data, aes(x = bin, y=County, fill = County)) +
    geom_density_ridges(stat = "binline", bins = 20, scale =2,alpha = 0.7) +
    scale_x_continuous(name ="Crop Pixel Frequency", 
                     limits=c(0,15), breaks=1:14)+
    theme_minimal()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
  
  
  
  ggpubr::ggarrange(ill,mi,wi,# list of plots
                    
                    legend = "right", # legend position
                    align = "hv", # Align them both, horizontal and vertical
                    ncol=1,
                    nrow = 3)  # number of rows
  
  
  
  

