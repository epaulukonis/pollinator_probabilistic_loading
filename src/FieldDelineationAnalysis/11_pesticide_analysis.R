### Probabilistic Crop Loading 

### 11 pesticide analysis

#Edited by E. Paulukonis June 2023


#get area of each crop in polygons within bombus habitat, 2008-2021
#get area of each crop in cdl raster wtihin bombus habitat, 2008-2021
#use plauseable loading to demonstrate utility? 
#plauseable field loadings 
#similar to table 1


#get data
#fields<-sf::st_read(paste0(root_data_out,"/final_champ1_2021.shp"))

print(list.files(path=paste0(root_data_out), pattern='.shp$', all.files=TRUE, full.names=FALSE))
field<- file.path(paste0(root_data_out), list.files(path=paste0(root_data_out), pattern='.shp$', all.files=TRUE, full.names=FALSE))
field<-setNames(lapply(field, readOGR), tools::file_path_sans_ext(basename(field)))
fieldf<-field[(mixedsort(as.character(names(field))))]

bomb_h <- st_read(paste0(bombus_dir,"/RPBB_High_Potential_Zones_03172021.shp"))
#transform to match crs
bomb_h<-st_transform(bomb_h, crs(fieldf[[1]]))

#get champiagn county extent (from 01_studyarea)
county<-"Champaign"
illc<-ill[ill$NAME %in% county,]
illc<-st_as_sf(illc)
illc<-st_transform(illc, crs(fieldf[[1]]))
# plot(ill)

#let's clip the bomb_h to the single county
bomb_h<-st_intersection(bomb_h,illc)
#plot(bomb_h$geometry)
bomb_h<-bomb_h[2,] #geometry of interest

years<-(2008:2021)


#### CDL ----
###here, we need to get average CDL data kg over time, by combining the COA acres and the state pesticide useages
###note that you'll need to double check the directory
state_pest<-read.csv(paste0(pest_dir, "/PNSP/state/PNSP_IL.csv"))

ill_coa<-read.csv(paste0(coa_dir,"/ILL_bombus.csv"))
ill_coa<-ill_coa[order(ill_coa$Year),]
ill_nass<- ill_coa%>%
  group_by(County,Year)%>%
  filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .))) %>%
  filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING|WHEAT, WINTER', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
ill_nass<-ill_nass[!ill_nass$Value == " (D)",]
ill_nass$Value<-as.numeric(as.numeric(gsub(",", "", ill_nass$Value)))
crop_by_county<- ill_nass %>% group_by(County, Year, Commodity) %>% summarise(sumf = sum(as.numeric(Value)))

## 3. get state crop data for calculating percentage planted
ill_coa<-read.csv(paste0(coa_dir,"/ILL_bombus_statewide.csv"))
ill_coa<-ill_coa[order(ill_coa$Year),]
ill_nass<-filter(ill_coa,Domain == "TOTAL")
ill_nass$Value<-as.numeric(as.numeric(gsub(",", "", ill_nass$Value)))
crop_by_state<- ill_nass %>% group_by(Year, Commodity) %>% summarise(acres = sum(as.numeric(Value))) #total acres harvested
crop_by_state<-crop_by_state[crop_by_state$Year %in% c(1997,2002,2007,2012,2017),]

## 4. get average applications in loads, as was done in Douglas et al. 2022, but we'll convert to per pixel rate
state_pest<-gather(state_pest, "Commodity", "kg", 6:15)
state_pest$Commodity<-toupper(state_pest$Commodity)
state_pest<-state_pest[state_pest$Commodity %in% crop_by_state$Commodity, ]

pest_prop_by_crop<-left_join(state_pest, crop_by_state, by = c("Year","Commodity"))

#fill interval generator
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

#calculate the amount of compound in total kg/total acreage
pest_prop_by_crop<-  pest_prop_by_crop %>% group_by(Compound,Commodity) %>% mutate(kg_f =f1(kg))
pest_prop_by_crop<-  pest_prop_by_crop %>% group_by(Commodity) %>% mutate(acres_f =f1(acres))
pest_prop_by_crop$kg_acre<-(pest_prop_by_crop$kg_f/pest_prop_by_crop$acres_f) #kg/acre

compounds<-c("CLOTHIANIDIN",
             "IMIDACLOPRID")
pest_prop_by_crop<-pest_prop_by_crop[pest_prop_by_crop$Compound %in% compounds,]
pest_prop_by_crop<-pest_prop_by_crop[pest_prop_by_crop$Year %in% years,]


#### extract the CDL specific crop area, and calculate an estimated loading for clothianidan and imidacloprid in kg by year
#here, we'll extract the original CDL to the bombus_area
extractions_cdl<-list()
  for(f in 1:length(cdl_data_ill_rec)){
  cdl<-cdl_data_ill_rec[[f]]
  cdl<-crop(cdl,bomb_h)
  cdl<-mask(cdl,bomb_h)
  #plot(cdl)
  area_cdl<-as.data.frame(freq(cdl))
  area_cdl$acreage<-area_cdl$count*900*0.000247105 #convert to acreages
  area_cdl<-area_cdl[area_cdl$value == 1 | area_cdl$value == 5, ] #keep corn or soy only
  area_cdl<-na.omit(area_cdl)
  
  cornapp<-pest_prop_by_crop[which(pest_prop_by_crop$Year == years[f] & pest_prop_by_crop$Commodity == "CORN" & pest_prop_by_crop$Compound == "CLOTHIANIDIN"), ] 
  soyapp<-pest_prop_by_crop[which(pest_prop_by_crop$Year == years[f] & pest_prop_by_crop$Commodity == "SOYBEANS" & pest_prop_by_crop$Compound == "IMIDACLOPRID"), ] 
  
  area_cdl$loading_kg<-ifelse(area_cdl$value == 1, area_cdl$acreage*cornapp$kg_acre, area_cdl$acreage*soyapp$kg_acre)
  area_cdl$year<-years[f]
  extractions_cdl[[f]]<-area_cdl
  }

cdl_load<-as.data.frame(do.call("rbind", extractions_cdl))
names(cdl_load)[1]<-"crop"

#backfill 2020,2021
cdl_load<-cdl_load %>% group_by(crop) %>% mutate(loading_kg =f1(loading_kg))


#### FIELDS ----
### using proportions from Hitaj et al., calculate the estimated loading of clothianidan/imidacloprid using % fields treated
#Take from Hitaj et al. Figure 2
##Corn: 55, 60, 68,72,75,80,90
##Soy: 25, 35, 50, 60, 70, 70, 75
cornp<-c(55,60,68,72,75,80,90,90,90,90,90,90,90,90)/100
soyp<- c(25,35,50,60,70,70,75,75,75,75,75,75,75,75 )/100

extractions_fields<-list()
for(f in 1:length(fieldf)){

  fields<-st_as_sf(fieldf[[f]])
  field_inst<-st_intersection(bomb_h,fields)
  field_inst<-field_inst[field_inst$crop == 1 | field_inst$crop == 5, ] #keep corn or soy only
  field_inst<-field_inst[!is.na(field_inst$Id),]
  corn<-field_inst[field_inst$crop == 1,]
  soy<-field_inst[field_inst$crop == 5,]
  corn$sample<-1:nrow(corn)
  soy$sample<-1:nrow(soy)
  
  #function to sample polygons randomly 100 times
  sample_func<-function(x,y){sample.int(nrow(x), y[f]*nrow(x))}
  
  cornsample<-replicate(100, sample_func(corn,cornp))
  soysample<-replicate(100, sample_func(soy,soyp))
    
  #rsamples<-sample.int(nrow(field_instn), props[p]*nrow(field_instn))
  
  simulations<-list()
  for(x in 1:100){
  cornf<-corn[corn$sample %in% cornsample[,x],]
  soyf<-soy[soy$sample %in% soysample[,x],]
  
  field_instn<-rbind(cornf,soyf)
  field_instn$area_m2<-st_area(field_instn)
  field_area<-field_instn %>% group_by(crop) %>% summarise(Freq = sum(area_m2))
  #field_area<-na.omit(field_area)
  field_area$acreage<-field_area$Freq*0.000247105

  #how much total loading of relevant pesticides? 
  #clothianidan for corn (0.0319702 kg/acre, max label rate for imida to corn) from douglas and tooker 2015
  #imidacloprod for soy (0.0388498 kg/acre, max label rate for cloth to soy) from douglas and tooker 2015
  field_area$loading_kg<-ifelse(field_area$crop == 1, field_area$acreage*0.0319702, field_area$acreage*0.0388498)
  field_area$year<-years[f]
  field_area$sim<-x
  simulations[[x]]<-field_area
  
  }

  fields_sim<-as.data.frame(do.call("rbind", simulations))
  extractions_fields[[f]]<-fields_sim
  
}
fields_load<-as.data.frame(do.call("rbind", extractions_fields))


#combine datasets
cdl_load$sim<-NA
fields_load$Source<-"Field Output"
cdl_load$Source<-"CDL Output"

#fields_load<-fields_load[,c(1,4:7)]
cdl_load<-cdl_load[,c(1,3:7)]
data_combo<-rbind(fields_load,cdl_load)


# corn<-data_combo[data_combo$crop == 1,]
# soy<-data_combo[data_combo$crop == 5,]

data_combo_avg_by_crop<-data_combo %>%
  group_by(Source,year,crop) %>% 
  summarise(loading_total = mean(loading_kg)) %>%
  summarise(summed = sum(loading_total))


data_combo_sum<- data_combo %>% group_by(Source,year,sim) %>% summarise(loading_total = sum(loading_kg))


# compare_results<-ggplot(data_combo_sum, aes(x=year, y=loading_total, group=interaction(Source,sim), color = factor(Source))) +
#   #geom_point()+
#   #geom_smooth(aes(linetype=Source))+
#   ylab("Total Neonicotinoid Seed Treatment Load (kg)")+
#   scale_x_discrete(name ="Year",
#                    limits=c(2008:2021))+
#   scale_colour_manual(values=c("deepskyblue3","brown4"),name = "Source")+
#   theme(panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# compare_results

cdl_data<-data_combo_sum[data_combo_sum$Source == "CDL Output",]
field_data<-data_combo_sum[data_combo_sum$Source == "Field Output",]


compare_results<-ggplot()+
  geom_boxplot(data = field_data,
               mapping = aes(year, loading_total,  fill = factor(year))) +
  geom_smooth(data = cdl_data, 
            mapping = aes(year, loading_total, color = factor(Source)), show.legend = FALSE) +

  ylab("Total Neonicotinoid Seed Treatment Load (kg)")+
  scale_x_discrete(name ="Year",
                   limits=c(2008:2021))+
  scale_fill_discrete(guide="none")+
  scale_colour_manual(values=c("deepskyblue3"),name = "Source")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12, face="bold"),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1, size=12, face="bold"))

compare_results







#all data
# compare_results<-ggplot(data_combo, aes(x=year, y=loading_kg, group=interaction(Source,crop), color = factor(crop), shape = Source)) +
#   geom_point()+
#   #geom_line()+
#   ylab("Pesticide Loading (kg)")+
#   scale_x_discrete(name ="Year",
#                    limits=c(2008:2021))+
#   scale_colour_manual(values=c("darkgoldenrod","darkgreen"),name = "Crop")+
#   theme(panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# compare_results


# 
# hist_acres<-ggplot(data_combo, aes(x=Source, y=acreage, colour=Source)) +geom_boxplot()+
#   ylab("Acreage")+
#   theme(panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#### Plots of the mapped area
area_bb<-st_as_sfc(st_bbox(field_inst))
cdl_bb<-st_as_sfc(st_bbox(cdl))
plot(area_bb)
plot(field_inst)

cdl[cdl > 5] <- NA
#plot(cdl)


field_inst<-field_inst[field_inst$crop == 1 | field_inst$crop == 5, ] 
field_inst<-field_inst[!is.na(field_inst$Id),]

field_inst$Crop<-ifelse(field_inst$crop == 1, "Corn","Soy")

ref1 = ggplot() + 
  geom_sf(data = area_bb, fill = "white") + 
  geom_sf(data = field_inst, mapping = aes(fill = factor(Crop))) +
  scale_fill_manual(values=c("darkgoldenrod","darkgreen"), name="Crop")+
  theme_void()
ref1

cdl<-st_as_stars(cdl)
#plot(cdl)

ref2 = ggplot() + 
  geom_sf(data = cdl_bb, fill = "white") + 
  geom_stars(
    data = cdl)+
  scale_fill_gradient(high="darkgreen",low="darkgoldenrod",na.value=NA)+
  # scale_fill_gradient(values=c("darkgoldenrod","darkgreen"), name="Crop")+
  theme_void()+
  theme(legend.position = "none")
ref2

ill<-st_as_sf(ill)
ill_bb<-st_as_sfc(st_bbox(ill))
c_bb<-st_as_sfc(st_bbox(illc))

ref3 = ggplot() + 
  geom_sf(data = ill, fill = "lightgrey") +
  #geom_sf(bomb_h, fill="lightblue")+
  geom_sf(data = c_bb, fill = NA, color = "red", size = 0.7) +
  geom_sf(data=bomb_h, fill="lightblue")+
  theme_void()
ref3


gg_inset_map1 = ggdraw() +
  draw_plot(compare_results,x = 0.01, y = 0.15, width = 0.70, height = 0.60)+
  draw_plot(ref1, x = 0.73, y=0.08, width=0.35, height=0.35) +
  draw_plot(ref2, x = 0.57, y=0.08, width=0.35, height=0.35) +
  draw_plot(ref3, x = 0.60, y = 0.45, width = 0.5, height = 0.5)
  #draw_plot(hist_acres, x = 0.59, y= 0.05, width=0.40, height=0.40)+
 

gg_inset_map1





