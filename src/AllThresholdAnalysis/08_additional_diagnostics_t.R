### Probabilistic Crop Loading 

### 08 additional diagnostics

# Edited by E. Paulukonis December 2022


#### Prepare data for threshold 1 year-acreage line plots ----

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





#### Here is where we put everything together for threshold 1 line plots ----
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
colScale <- scale_colour_manual(labels=c("Champaign Fields","McHenry Fields","DuPage Fields","NASS","CDL"),name = "Acreages",values = myColors)


scales::show_col(myColors)
final_Ix<-na.omit(final_Ix)

line_plotI<-ggplot(final_Ix, aes(x=(year), y=log(n),group=interaction(Category,County), color =Combo, shape=County)) + 
  geom_line()+
  geom_point()+
  colScale+
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
colScale <- scale_colour_manual(labels=c("Huron Fields","Oceana Fields","Van Buren Fields","NASS","CDL"), name = "Acreages",values = myColors)

final_Mx<-na.omit(final_Mx)

line_plotM<-ggplot(final_Mx, aes(x=year, y=log(n),group=interaction(Category,County), color =Combo, shape=County)) + 
  geom_point()+
  geom_line()+
  colScale+
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
colScale <- scale_colour_manual(labels=c("Rock Fields","Waushara Fields","Langlade Fields","NASS","CDL"), name = "Acreages",values = myColors)

final_Wx<-na.omit(final_Wx)

line_plotW<-ggplot(final_Wx, aes(x=year, y=log(n),group=interaction(Category,County), color =Combo, shape=County)) + 
  geom_point()+
  geom_line()+
  colScale+
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




#### high/low comparison ----
final_Ih<-final_Illinois[final_Illinois$County == 'Champaign' & final_Illinois$thresh==1,]
final_Mh<-final_Michigan[final_Michigan$County == 'Huron' & final_Michigan$thresh==1,]
final_Wh<-final_Wisconsin[final_Wisconsin$County == 'Rock' & final_Wisconsin$thresh==1,]
final_Ih$State<-"Illinois"
final_Mh$State<-"Michigan"
final_Wh$State<-"Wisconsin"
high<-rbind(final_Ih,final_Mh,final_Wh)

final_Il<-final_Illinois[final_Illinois$County == 'DuPage' & final_Illinois$thresh==1,]
final_Ml<-final_Michigan[final_Michigan$County == 'VanBuren' & final_Michigan$thresh==1,]
final_Wl<-final_Wisconsin[final_Wisconsin$County == 'Langlade' & final_Wisconsin$thresh==1,]
final_Il$State<-"Illinois"
final_Ml$State<-"Michigan"
final_Wl$State<-"Wisconsin"
final_Ml$County<-gsub("([a-z])([A-Z])","\\1 \\2",final_Ml$County)
low<-rbind(final_Il,final_Ml,final_Wl)

high$sum_nass<-ifelse(high$sum_nass == 0, NA, high$sum_nass)
low$sum_nass<-ifelse(low$sum_nass == 0, NA, low$sum_nass)

high<-na.omit(high)
low<-na.omit(low)

#low
nass_datL<-low %>% group_by(County) %>% mutate(avg =mean(sum_nass)) %>% mutate(avgcdl =mean(sum_cdl))
#high
nass_datH<-high %>% group_by(County) %>% mutate(avg =mean(sum_nass)) %>% mutate(avgcdl =mean(sum_cdl))

nass_data<-rbind(nass_datH,nass_datL)

nass_data$County <- factor(nass_data$County , levels = c("Champaign", "DuPage","Huron","Van Buren","Rock","Langlade"))
compare_box<-ggplot(nass_data, aes(x = as.factor(County), y = log(sum_field), color=State))+
  geom_boxplot()+
  xlab("County") +
  ylab("Log(Sum of Crop Acreages)")+
  labs(title = paste0("Comparison of Sum Total Crop Acreages in High vs. Low Agriculture Counties"))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.2)))+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))
 # facet_wrap(.~State, scales = "free")

compare_box

compare_boxf<-compare_box + 
  geom_point(nass_data, mapping=aes(x=as.factor(County), y=log(avg),group=1),size=3,color="black", shape=17)+
  geom_point(nass_data, mapping=aes(x=as.factor(County), y=log(avgcdl),group=1),size=3,color="darkgrey", shape=19)+
  #scale_y_continuous(limit = c(2.75, 6))+
  theme_minimal()+
 theme(axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"))

compare_boxf








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

final_all<-rbind(final_Michigan,final_Wisconsin)
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
