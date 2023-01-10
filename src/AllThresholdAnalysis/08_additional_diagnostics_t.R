### Probabilistic Crop Loading 

### 08 additional diagnostics

# Edited by E. Paulukonis December 2022


#### threshold 1 line sum by year plots ----

#here, I took the code from the OG line plots but changed the way the data is compiled by letting NASS = 0 at non-COA years
#then, I put everything together, plotted the lines for all years, and then took out any NASS years with 0
#I need to do this for the other two States and put them together, and also figure out how to get the legend to do what I want
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
      layer_by_year$Category<-  toupper(layer_by_year$Category) 
      #layer_by_year<-layer_by_year[!layer_by_year$Category == "OTHER HAY/NON ALFALFA",] #add in winter wheat?? no
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year$Category<-  toupper(layer_by_year$Category) 
      layer_by_year$Category[grepl('WINTER', layer_by_year$Category)] <- 'WHEAT'
      layer_by_year$Category[grepl('OTHER HAY/NON ALFALFA', layer_by_year$Category)] <- 'HAY'
      layer_by_year$Category[grepl('ALFALFA', layer_by_year$Category)] <- 'HAY'
      layer_by_year$x<-ifelse(layer_by_year$Class ==  36, with(layer_by_year,sum(x[Category =='HAY'])), layer_by_year$x)
      layer_by_year<-layer_by_year[!layer_by_year$Class == 37,] #drop the other hay class because we merged it with the alfalfa class
      layer_by_year$threshold<-15-layer
      
      list_of_field[[layer]]<-layer_by_year
      
    }
    
    list_of_year[[y]]<-do.call(rbind, list_of_field)
    
  }
  
  field_data<-do.call(rbind, list_of_year)
  #NASS data
  ill_nassx<-ill_nass[!ill_nass$Value == " (D)",]
  ill_nassx$Value<-as.numeric(as.numeric(gsub(",", "", ill_nassx$Value)))
  ill_nass_y<- ill_nassx %>%
    #filter(Year %in% c(layer_by_year$year)) %>%
    filter(County %in% county_name) %>%
    filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .)))%>% #use 'non' to filter bearing and non-bearing
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING', .)))%>%
    group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
  
  
  #|SILAGE|IRRIGATED
  
  names(field_data)[3]<-"Commodity"
  names(field_data)[2]<-"fieldacres"
  layer_by_year_crops<-left_join(field_data, ill_nass_y, by = c("Year","Commodity"))
  colnames(layer_by_year_crops)[8]<-"NASSacres"
  layer_by_year_crops[is.na(layer_by_year_crops)] <- 0
  
  #layer_by_year_crops<-na.omit(layer_by_year_crops) #IF there are no crops represented in NASS for that year, drop those rows
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

final_Illinois<-do.call(rbind, list_of_dataI)
final_Illinois <- final_Illinois %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))



###Here is where we put everything together...
final_I<-final_Illinois[final_Illinois$thresh==1,]
final_M<-final_Michigan[final_Michigan$thresh==1,]
final_W<-final_Wisconsin[final_Wisconsin$thresh==1,]

#refactor names
final_I$County <- factor(final_I$County , levels = c("Champaign", "McHenry", "DuPage"))
final_M$County <- factor(final_M$County , levels = c("Huron", "Oceana", "VanBuren"))
final_W$County <- factor(final_W$County , levels = c("Rock", "Waushara", "Langlade"))

#remove weird outlier from Ill
final_I<-final_I[!c(final_I$year == 2021 & final_I$County == "McHenry"),]
#change names for legends
final_M$County<-gsub("([a-z])([A-Z])","\\1 \\2",final_M$County)



ratio_plot<-ggplot(final_I, aes(x=year, y=log(sum_field), group=County, fill=factor(County), colour=factor(County))) +
geom_point()+
geom_line()+
xlab("Year") +
ylab("Log(Sum Acreages)")+
scale_x_discrete(name ="Year",
limits=c(2008:2021))+
scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0, .1)))+
labs(title = paste0("Illinois County Total Field Acres to NASS Acres"))+
guides(fill=guide_legend(title="County"), colour=guide_legend(title="County"))+
theme(panel.background = element_blank(),
axis.line = element_line(colour = "black"),
axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ratio_plot

final_I$sum_nass<-ifelse(final_I$sum_nass == 0,NA,final_I$sum_nass)
final_I<-na.omit(final_I)

Ioutput<-ratio_plot+
geom_point(final_I,mapping=aes(y=log(sum_nass),group=County, shape=County),col='black') +
geom_line(final_I,mapping=aes(y=log(sum_nass),group=County), col='black')+
geom_point(final_I,mapping=aes(y=log(sum_cdl),group=County, shape=County),col='darkgrey') +
geom_line(final_I,mapping=aes(y=log(sum_cdl),group=County),col='darkgrey')
Ioutput

#got it to work!


#### low vs high boxplots ----

#use threshold 1, all years
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

#high
nass_datH<-high %>% group_by(County) %>% mutate(avg =mean(sum_nass)) %>% mutate(avgcdl =mean(sum_cdl))
high_box<-ggplot(nass_datH, aes(x = as.factor(County), y = sum_field, color=as.factor(County)))+
  geom_boxplot()+
  xlab("County") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))+
  facet_wrap(.~County, scales = "free")

high_box
high_boxf<-high_box + 
  geom_point(nass_datH, mapping=aes(x=as.factor(County), y=avg,group=1),size=3,color="black", shape=17)+
  geom_point(nass_datH, mapping=aes(x=as.factor(County), y=avgcdl,group=1),size=3,color="darkgrey", shape=19)+
  facet_wrap(.~as.factor(County), scales = "free")
high_boxf


#low
nass_datL<-low %>% group_by(County) %>% mutate(avg =mean(sum_nass)) %>% mutate(avgcdl =mean(sum_cdl))
low_box<-ggplot(nass_datL, aes(x = as.factor(County), y = sum_field, color=as.factor(County)))+
  geom_boxplot()+
  xlab("County") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))+
  facet_wrap(.~County, scales = "free")

low_box
low_boxf<-low_box + 
  geom_point(nass_datL, mapping=aes(x=as.factor(County), y=avg,group=1),size=3,color="black", shape=17)+
  geom_point(nass_datL, mapping=aes(x=as.factor(County), y=avgcdl,group=1),size=3,color="darkgrey", shape=19)+
  facet_wrap(.~as.factor(County), scales = "free")
low_boxf



ggpubr::ggarrange(high_boxf,low_boxf, # list of plots
                  labels = 1, # labels
                  # common.legend = T, # COMMON LEGEND
                  # legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 1)  # number of rows

### new high/low comparison ----

nass_data<-rbind(nass_datH,nass_datL)
nass_data$County <- factor(nass_data$County , levels = c("Champaign", "DuPage","Huron","Van Buren","Rock","Langlade"))
compare_box<-ggplot(nass_data, aes(x = as.factor(County), y = log10(sum_field), color=State))+
  geom_boxplot()+
  xlab("County") +
  ylab("Log10(Sum Acreages)")+
  labs(title = paste0("Comparison of Sum Total Crop Acreages in High vs. Low Agriculture Counties"))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.2)))+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))
 # facet_wrap(.~State, scales = "free")

compare_box

compare_boxf<-compare_box + 
  geom_point(nass_data, mapping=aes(x=as.factor(County), y=log10(avg),group=1),size=3,color="black", shape=17)+
  geom_point(nass_data, mapping=aes(x=as.factor(County), y=log10(avgcdl),group=1),size=3,color="darkgrey", shape=19)+
  scale_y_continuous(limit = c(2.75, 6))+
  theme_minimal()

compare_boxf





