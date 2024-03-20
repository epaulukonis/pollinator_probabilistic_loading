### Probabilistic Crop Loading 

### 10 Applying models to spatial data 

# Edited by E. Paulukonis November 2023

#Code for modeling on-site field concentrations from applications in 4 media types, within modeled scenarios

#### Prepare individual scenario dataframes ----

#get information to run PWC and associated .zts files
# get_pwc_information<-on_field_area%>% 
#   filter(!is.na(applicationday)) %>%
#   group_by(Compound, Commodity, ApplicationType) %>% 
#   summarise(type=toString(unique(applicationday))) %>%
#   ungroup()
#get_pwc_information



#set.seed(7489)
on_field_area<-on_field_area[,c(1:10,18,19)]
off_field_area<-off_field_area[,c(1:10,18,19)]

unique(on_field_area$Compound)

#### for the scenarios, we need to group by the individual ID

individual_fields_by_group_onfield<-split(on_field_area, list(on_field_area$id,on_field_area$Compound,on_field_area$ApplicationType, on_field_area$Commodity),drop=T )
individual_fields_by_group_offfield<-split(off_field_area, list(off_field_area$id,off_field_area$Compound,off_field_area$ApplicationType, off_field_area$Commodity),drop=T )

# examine<-individual_fields_by_group_onfield[sapply(individual_fields_by_group_onfield, nrow) >1]
# x<-examine[[2]]
# plot(x$geometry)

#here, we create function to treat each of these individual dataframes as its own unique set of residue information
prepare_data_frames<-function(x){
  x<-x%>%top_n(1,shapearea)
  x<-x[rep(seq_len(nrow(x)), 151),]
  x$day<-0:150
  x[2:151,9]<-NA
  x<-x[rep(seq_len(nrow(x)), 4),]
  a<-rep("Air",151)
  s<-rep("Soil",151)
  p<-rep("Pollen",151)
  n<-rep("Nectar",151)
  submedia<-c(a,s,p,n)
  x$MediaSub<-submedia
  x
}

individual_fields_by_group_onfield<-lapply(individual_fields_by_group_onfield,prepare_data_frames)
original_names_of_fields<-names(individual_fields_by_group_onfield)
#change so that the dataframe matches 
names(individual_fields_by_group_onfield)<-gsub(".", "_", names(individual_fields_by_group_onfield), fixed=T)

individual_fields_by_group_offfield<-lapply(individual_fields_by_group_offfield,prepare_data_frames)
original_names_of_fields<-names(individual_fields_by_group_offfield)
#change so that the dataframe matches 
names(individual_fields_by_group_offfield)<-gsub(".", "_", names(individual_fields_by_group_offfield), fixed=T)


##### ONSITE OUTPUTS BY FIELD
#alter the gather function slightly
gather_data<-function(x){
  df<-x
  df<-gather(df, "MediaSub", "Value", 4:ncol(df))
  df
}
modeled_onsite_outputs_by_field<-list()
for(field in 1:length(individual_fields_by_group_onfield)){
  scenario_df<-individual_fields_by_group_onfield[[field]]
  
  #get name of modeled data that we need to match to the field scenario
  pulldf<-names(on_field_residue_list)[amatch(names(individual_fields_by_group_onfield[field]), names(on_field_residue_list), maxDist = Inf)]
  
  #match on_field_residue list of modeled outputs
  model_df<-on_field_residue_list[names(on_field_residue_list) %in% pulldf]
  model_df<- map_df(model_df, ~as.data.frame((.)))

  colnames(model_df)[4:ncol(model_df)]<-sub("\\_.*", "", names(model_df)[4:ncol(model_df)])
  
  organize_data<-gather_data(model_df)
  organize_data$Value<-ifelse(organize_data$Media == "Air" & !organize_data$day == 0,0,organize_data$Value) # remove all values for air except on day of application
  
 #this should merge the scenario df for each field ID with the estimated residue concentrations 
 on_field_scenario<- merge(scenario_df, organize_data, by=c("MediaSub","day","Compound","Commodity"), all=T) 
 
 #remove any duplicated rows (can happen sometimes with certain application dates)
 on_field_scenario<-on_field_scenario[!duplicated(on_field_scenario), ]
 on_field_scenario<- on_field_scenario[!is.na(on_field_scenario$Year),]
 
 modeled_onsite_outputs_by_field[[field]]<-on_field_scenario
  
}

#this is the scenario_specific output for on-site models
names(modeled_onsite_outputs_by_field)<-names(individual_fields_by_group_onfield)




#examine<-modeled_onsite_outputs_by_field[sapply(modeled_onsite_outputs_by_field, nrow) == 755]
# testy<-examine[[1]]
# 
# 
# mylist_sub <- modeled_onsite_outputs_by_field[grep('1498', names(modeled_onsite_outputs_by_field))]

##### OFFSITE OUTPUTS BY FIELD
#alter the gather function slightly
# gather_data_offfield<-function(x){
#   df<-x
#   df<-gather(df, "MediaSub", "Value", 5:ncol(df))
#   df
#   
# }

#field<-1
modeled_offsite_outputs_by_field<-list()
for(field in 1:length(individual_fields_by_group_offfield)){
  
  scenario_df<-individual_fields_by_group_offfield[[field]]
  
  #get name of modeled data that we need to match to the field scenario
  pulldf<-names(off_field_residue_list)[amatch(names(individual_fields_by_group_offfield[field]), names(off_field_residue_list), maxDist = Inf)]
  
  #match on_field_residue list of modeled outputs
  model_df<-off_field_residue_list[names(off_field_residue_list) %in% pulldf]
  model_df<- map_df(model_df, ~as.data.frame((.)))
  
  model_df<-model_df[,!names(model_df) %in% "ApplicationType"]
  
  
  colnames(model_df)[4:ncol(model_df)]<-sub("\\_.*", "", names(model_df)[4:ncol(model_df)])
  
  organize_data<-gather_data(model_df)
  organize_data$Value<-ifelse(organize_data$Media == "Air" & !organize_data$day == 0,0,organize_data$Value) # remove all values for air except on day of application
  
  #this should merge the scenario df for each field ID with the estimated residue concentrations 
  off_field_scenario<- merge(scenario_df, organize_data, by=c("MediaSub","day","Compound","Commodity"), all=T) 
  off_field_scenario<-off_field_scenario[!duplicated(off_field_scenario), ]
  off_field_scenario<- off_field_scenario[!is.na(off_field_scenario$Year),]
  modeled_offsite_outputs_by_field[[field]]<-off_field_scenario
  
}

#this is the scenario_specific output for on-site models
names(modeled_offsite_outputs_by_field)<-names(individual_fields_by_group_offfield)




#### Get moving averages 
#testy<-modeled_onsite_outputs_by_field[[800]]
#x<-testy

get_rolling_average<-function(x){

#   why_no_work<-list()
#   for(field in 1:length(modeled_onsite_outputs_by_field)){
  #x<-modeled_onsite_outputs_by_field[[field]]
  
x$Value<-ifelse(x$MediaSub == "Air" & x$day > 0 | x$MediaSub == "Dust" & x$day > 0, NA, x$Value)
x<- x[!is.na(x$Value),]
 
#pull out single deposition value for air/dust
 depo<-x[x$MediaSub%in% c("Air","Dust"),]
 depo$movingavg<-depo$Value
 
 x<-x[!x$MediaSub %in% c("Air","Dust"), ]
 x<-x[!x$Value == 0,]
 
 x<-x[order(x$MediaSub,x$day),]
 
 x<- x %>% group_by(MediaSub) %>% 
  slice(1:7) 

 
 y<- x %>% 
    group_by(MediaSub) %>%
    mutate(movingavg = zoo::rollmean(x=Value,7)) %>%
    filter(row_number()==1)

 ##sliding window of 7 days
 
y<-rbind(depo,y)
# why_no_work[[field]]<-y
# 
# 
#   }
}

##on-field
moving_avg_list_on<-lapply(modeled_onsite_outputs_by_field,get_rolling_average)
on_field_moving_averages <- do.call("rbind", moving_avg_list_on)


##off-field
moving_avg_list_off<-lapply(modeled_offsite_outputs_by_field,get_rolling_average)
off_field_moving_averages <- do.call("rbind", moving_avg_list_off)


# 
# st_write(off_field_moving_averages , paste0(root_data_out, "/all_bombus/outputforshiny/off_field_movingavgn.shp"),overwrite=T, driver = "ESRI Shapefile")
# st_write(on_field_moving_averages , paste0(root_data_out, "/all_bombus/outputforshiny/on_field_movingavgn.shp"), overwrite=T,driver = "ESRI Shapefile")





#### Prepare data ----
onfield_tox_thresh<-left_join(on_field_moving_averages,beetox[,c(1:3)])
offfield_tox_thresh<-left_join(off_field_moving_averages,beetox[,c(1:3)])

#surface area of bumblebee
SA<-5 #cm2

#calculate ingestion/contact based eecs
onfield_tox_thresh<- onfield_tox_thresh %>% #calculate ingestion/contact based eecs
 mutate(EEC = movingavg * case_when(
    MediaSub == "Air"  ~ SA/10000,
    MediaSub == "Dust" ~ SA/10000,
    MediaSub == "Soil" ~ 1/SA,
    MediaSub == "Nectar" ~ 0.400,
    MediaSub == "Pollen" ~  0.030))


offfield_tox_thresh<- offfield_tox_thresh %>%  mutate(EEC = movingavg * case_when(
  MediaSub == "Air"  ~ SA/10000,
  MediaSub == "Dust" ~ SA/10000,
  MediaSub == "Soil" ~ 1/SA,
  MediaSub == "Nectar" ~ 0.400,
  MediaSub == "Pollen" ~  0.030))

#remove rows where the condition for concentration is expressed twice
onfield_tox_thresh<-gather(onfield_tox_thresh,"ExposureLevel","Endpoint", 16:17)
onfield_tox_thresh<-onfield_tox_thresh[!( onfield_tox_thresh$MediaSub == "Air" & onfield_tox_thresh$ExposureLevel == "Oral_LD50_ug_bee" |
                                            onfield_tox_thresh$MediaSub == "Dust" & onfield_tox_thresh$ExposureLevel == "Oral_LD50_ug_bee" |
                                            onfield_tox_thresh$MediaSub == "Soil" & onfield_tox_thresh$ExposureLevel == "Oral_LD50_ug_bee"|
                                            onfield_tox_thresh$MediaSub == "Nectar" & onfield_tox_thresh$ExposureLevel == "Contact_LD50_ug_bee"|
                                            onfield_tox_thresh$MediaSub == "Pollen" & onfield_tox_thresh$ExposureLevel == "Contact_LD50_ug_bee") ,]
#sub_onfield<-onfield_tox_thresh[onfield_tox_thresh$EEC >= onfield_tox_thresh$Contact_LD50_ug_bee | onfield_tox_thresh$EEC >= onfield_tox_thresh$Oral_LD50_ug_bee , ]





offfield_tox_thresh<-gather(offfield_tox_thresh,"ExposureLevel","Endpoint", 17:18)
offfield_tox_thresh<-offfield_tox_thresh[!( offfield_tox_thresh$MediaSub == "Air" & offfield_tox_thresh$ExposureLevel == "Oral_LD50_ug_bee" |
                                            offfield_tox_thresh$MediaSub == "Dust" & offfield_tox_thresh$ExposureLevel == "Oral_LD50_ug_bee" |
                                            offfield_tox_thresh$MediaSub == "Soil" & offfield_tox_thresh$ExposureLevel == "Oral_LD50_ug_bee"|
                                            offfield_tox_thresh$MediaSub == "Nectar" & offfield_tox_thresh$ExposureLevel == "Contact_LD50_ug_bee"|
                                            offfield_tox_thresh$MediaSub == "Pollen" & offfield_tox_thresh$ExposureLevel == "Contact_LD50_ug_bee") ,]
#sub_offfield<-offfield_tox_thresh[offfield_tox_thresh$EEC >= offfield_tox_thresh$Contact_LD50_ug_bee | offfield_tox_thresh$EEC >= offfield_tox_thresh$Oral_LD50_ug_bee , ]




#### Examine and plan maps ----
library(tmaptools)
tmap_options(check.and.fix = TRUE)
tmap_mode("view")

## Map just for looking at fields and buffer zones
nlcdpalette<-c("cornflowerblue","bisque","lightsalmon","firebrick","darkred","darkolivegreen3","darkgreen","darkolivegreen2","darkkhaki","lemonchiffon3","darkseagreen3","khaki1","chocolate4","cadetblue2","cadetblue4")


#seed
sub_off<-offfield_tox_thresh[offfield_tox_thresh$ApplicationType== "Seed",]
sub_on<-onfield_tox_thresh[onfield_tox_thresh$ApplicationType== "Seed",]

tm_shape(habitat) +
  tm_raster(style = "cat", palette = nlcdpalette, legend.show=FALSE)+
  #tm_polygons(fill="Compound", col="Commodity", palette= c( "gold", "darkgreen"),)+
tm_shape(buf_hab)+
  tm_fill(col="lightgrey",alpha=0.2)+
tm_shape(sub_off)+
  tm_polygons(col="EEC",style="cont", palette=get_brewer_pal(palette="GnBu", n=5, plot=FALSE), alpha=0.3) +
tm_shape(sub_on)+
  tm_polygons(col="EEC",style="cont", palette=get_brewer_pal(palette="OrRd", n=5, plot=FALSE), alpha=0.3) 


#foliar
sub_off<-offfield_tox_thresh[offfield_tox_thresh$Compound == "CHLORPYRIFOS",]
sub_on<-onfield_tox_thresh[onfield_tox_thresh$Compound == "CHLORPYRIFOS",]

tm_shape(habitat) +
  tm_raster(style = "cat", palette = cdlpalette, legend.show=FALSE)+
  #tm_polygons(fill="Compound", col="Commodity", palette= c( "gold", "darkgreen"),)+
  tm_shape(buf_hab)+
  tm_fill(col="lightgrey",alpha=0.2)+
  tm_shape(sub_off)+
  tm_polygons(col="EEC",style="cont", palette=get_brewer_pal(palette="GnBu", n=5, plot=FALSE), alpha=0.3) +
  tm_shape(sub_on)+
  tm_polygons(col="EEC",style="cont", palette=get_brewer_pal(palette="OrRd", n=5, plot=FALSE), alpha=0.3) 


#soil
sub_off<-offfield_tox_thresh[offfield_tox_thresh$Compound == "BIFENTHRIN" & offfield_tox_thresh$ApplicationType == "Soil",]
sub_on<-onfield_tox_thresh[onfield_tox_thresh$Compound == "BIFENTHRIN" & onfield_tox_thresh$ApplicationType == "Soil",]

tm_shape(habitat) +
  tm_raster(style = "cat", palette = cdlpalette, legend.show=FALSE)+
  #tm_polygons(fill="Compound", col="Commodity", palette= c( "gold", "darkgreen"),)+
  tm_shape(buf_hab)+
  tm_fill(col="lightgrey",alpha=0.2)+
  tm_shape(sub_off)+
  tm_polygons(col="EEC",style="cont", palette=get_brewer_pal(palette="GnBu", n=5, plot=FALSE), alpha=0.3) +
  tm_shape(sub_on)+
  tm_polygons(col="EEC",style="cont", palette=get_brewer_pal(palette="OrRd", n=5, plot=FALSE), alpha=0.3) 


  
#### Grouped maps by application and media ----
offfield_tox_thresh$Location<-"Off-field"
onfield_tox_thresh$Location<-"On-field"
offfield_tox_thresh<-offfield_tox_thresh[ ,-c(14)] #remove random extra column



# tmap_mode("plot")

#select a subset of habitat for each application type; we'll pick one compound per application type
buf_hab_sub<-buf_hab

off_field<-st_intersection(offfield_tox_thresh, buf_hab_sub)
on_field<-st_intersection(onfield_tox_thresh, buf_hab_sub)

plot(off_field["MediaSub"])
plot(on_field["MediaSub"])

nlcdpalette<-c("cornflowerblue","bisque","lightsalmon","firebrick","darkred","darkolivegreen3","darkgreen","darkolivegreen2","darkkhaki","lemonchiffon3","khaki1","chocolate4","cadetblue2","cadetblue4")

nlcdlayer<-as.data.frame(habitat)
namey<-sort(unique(nlcdlayer$Layer_1))
names(nlcdpalette)<-(namey)

colScale<- scale_fill_manual(
  values=nlcdpalette, name="NLCD",
  labels = c('Water',
             'Developed, Open Space ',
             'Developed, Low Intensity',
             'Developed, Medium Intensity',
             "Developed, High Intensity",
             "Deciduous Forest", 
             "Evergreen Forest",
             "Mixed Forest",
             "Shrub",
             "Grassland",
             "Pasture/Hay",
             "Cultivated",
             "Woody Wetlands",
             "Herbaceous Wetlands"),
  drop = FALSE) 


# comps<-c("IMIDACLOPRID","BIFENTHRIN","CARBARYL","GLYPHOSATE","CHLORPYRIFOS","THIAMETHOXAM")
# offfield_tox_thresh<-offfield_tox_thresh[offfield_tox_thresh$Compound %in% comps, ]
# onfield_tox_thresh<-onfield_tox_thresh[onfield_tox_thresh$Compound %in% comps, ]

subset_off<-offfield_tox_thresh[offfield_tox_thresh$Compound == "IMIDACLOPRID" & offfield_tox_thresh$ApplicationType == "Seed"| offfield_tox_thresh$Compound == "CARBARYL" | offfield_tox_thresh$Compound == "BIFENTHRIN" & offfield_tox_thresh$ApplicationType == "Soil" ,]
subset_on <-onfield_tox_thresh[onfield_tox_thresh$Compound == "IMIDACLOPRID" & onfield_tox_thresh$ApplicationType == "Seed"| onfield_tox_thresh$Compound == "CARBARYL" | onfield_tox_thresh$Compound == "BIFENTHRIN" & onfield_tox_thresh$ApplicationType == "Soil" ,]
# #st_write(sub_seedon , paste0(root_data_out, "/all_bombus/outputforshiny/imidaclopridref.shp"),overwrite=T, driver = "ESRI Shapefile")



############################################### Plots grouped on application type ##################################
#### Agricultural ----
hab_ag<-buf_hab_sub[buf_hab_sub$id == 1,]
plot(hab_ag)

off_field<-st_intersection(subset_off, hab_ag)   
plot(st_geometry(off_field))

on_field<-st_intersection(subset_on, hab_ag)
plot(st_geometry(on_field))


on_field<-on_field[!on_field$MediaSub=="Air",]
off_field<-off_field[!off_field$MediaSub=="Air",]

#get visual buffer
hab_buff<-st_buffer(hab_ag, 10)
habitat<-mask(crop(nlcd, hab_ag), hab_ag) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### First, get Habitat tile 
habitat$title<-"Agriculturally Intensive Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )
habplot
habitat<-habitat[,-4]


split_off<-split(off_field, list(off_field$Compound))
split_on<-split(on_field, list(on_field$Compound))

list_of_plots<-list()
for(compound in 1:length(split_off)){

off_field_n<-split_off[[compound]]
on_field_n<-split_on[[compound]]

mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = off_field_n, aes(fill = EEC)) +
  geom_sf(data = on_field_n, aes(fill = EEC),colour="white") +
  scale_fill_viridis_b(option = "D", 
                       name= "EED [ug/bee;ug/g]",
                       
                       # limits=lims,
                       # breaks=sc_fill_breaks
  )+
  facet_grid(.~MediaSub) +
  geom_sf(data = hab_buff, fill = NA) +
  theme_bw() +
  ylab(paste0(unique(off_field_n$ApplicationType)," Application"))+
  xlab("")+
  ggtitle(paste0(str_to_title(unique(off_field_n$Compound)),"-",off_field_n$Commodity))+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    
    axis.title.x=element_blank()
  )
list_of_plots[[compound]]<-mediaplot


}

media_plots<-ggpubr::ggarrange(plotlist = list_of_plots, ncol=1)
media_plots

ag<-plot_grid(
  habplot,
  media_plots,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(2,4))
ag



#Ag calculations

#get area by total habitat
just_area_on<-on_field[on_field$MediaSub=="Soil",]
#just_area_on<-on_field[on_field$Compound == "IMIDACLOPRID",]
just_area_on$area <- st_area(just_area_on)

area_out_on<-just_area_on %>%
  st_drop_geometry() %>%
  group_by(Compound) %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_on$area_per))

just_area_off<-off_field[off_field$MediaSub=="Soil",]
#just_area_off<-off_field[off_field$Compound == "IMIDACLOPRID",]
just_area_off$area <- st_area(just_area_off)

area_out_off<-just_area_off %>%
  st_drop_geometry() %>%
  group_by(Compound) %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_off$area_per)-sum(area_out_on$area_per))


#get area by specific compound
by_compound<-on_field[on_field$Compound=="CARBARYL",]
by_compound$area <- st_area(by_compound)
by_compound<-by_compound[1,]
area_out_on<-by_compound %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_on$area_per))


by_compound<-off_field[off_field$Compound=="CARBARYL",]
by_compound$area <- st_area(by_compound)
by_compound<-by_compound[1,]
area_out_off<-by_compound %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_off$area_per))




#### Mixed Natural ----
hab_n<-buf_hab_sub[buf_hab_sub$id == 2,]
plot(hab_n)

off_field<-st_intersection(subset_off, hab_n)   
plot(st_geometry(off_field))

on_field<-st_intersection(subset_on, hab_n)
plot(st_geometry(on_field))


on_field<-on_field[!on_field$MediaSub=="Air",]
off_field<-off_field[!off_field$MediaSub=="Air",]

#get visual buffer
hab_buff<-st_buffer(hab_n, 10)
habitat<-mask(crop(nlcd, hab_n), hab_n) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### First, get Habitat tile 
habitat$title<-"Mixed Natural Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )
habplot
habitat<-habitat[,-4]


split_off<-split(off_field, list(off_field$Compound))
split_on<-split(on_field, list(on_field$Compound))

list_of_plots<-list()
for(compound in 1:length(split_off)){
  
  off_field_n<-split_off[[compound]]
  on_field_n<-split_on[[compound]]
  
  mediaplot<-ggplot()+
    geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
    scale_fill_grey(guide = "none") +
    ggnewscale:::new_scale_fill() +
    geom_sf(data = off_field_n, aes(fill = EEC)) +
    geom_sf(data = on_field_n, aes(fill = EEC),colour="white") +
    scale_fill_viridis_b(option = "D", 
                         name= "EED [ug/bee;ug/g]",
                         
                         # limits=lims,
                         # breaks=sc_fill_breaks
    )+
    facet_grid(.~MediaSub) +
    geom_sf(data = hab_buff, fill = NA) +
    theme_bw() +
    ylab(paste0(unique(off_field_n$ApplicationType)," Application"))+
    xlab("")+
    ggtitle(paste0(str_to_title(unique(off_field_n$Compound)),"-",off_field_n$Commodity))+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title.align=0.5,
      strip.text.x = element_text(size = 14, colour = "black", angle = 0),
      legend.position = "right",
      axis.text.x=element_blank(), 
      axis.ticks.x=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.y=element_blank(),
      
      axis.title.x=element_blank()
    )
  list_of_plots[[compound]]<-mediaplot
  
  
}

media_plots<-ggpubr::ggarrange(plotlist = list_of_plots, ncol=1)
media_plots

nat<-plot_grid(
  habplot,
  media_plots,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(2,4))
nat


#mixed calculations
#get area by total habitat
just_area_on<-on_field[on_field$MediaSub=="Soil",]
#just_area_on<-on_field[on_field$Compound == "IMIDACLOPRID",]
just_area_on$area <- st_area(just_area_on)

area_out_on<-just_area_on %>%
  st_drop_geometry() %>%
  group_by(Compound) %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_on$area_per))

just_area_off<-off_field[off_field$MediaSub=="Soil",]
#just_area_off<-off_field[off_field$Compound == "IMIDACLOPRID",]
just_area_off$area <- st_area(just_area_off)

area_out_off<-just_area_off %>%
  st_drop_geometry() %>%
  group_by(Compound) %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_off$area_per)-sum(area_out_on$area_per))


#get area by specific compound
by_compound<-on_field[on_field$Compound=="CARBARYL",]
by_compound$area <- st_area(by_compound)
by_compound<-by_compound[1,]
area_out_on<-by_compound %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_on$area_per))


by_compound<-off_field[off_field$Compound=="CARBARYL",]
by_compound$area <- st_area(by_compound)
by_compound<-by_compound[1,]
area_out_off<-by_compound %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_off$area_per))










#### Mixed Developed ----
hab_d<-buf_hab_sub[buf_hab_sub$id == 3,]
plot(hab_d)

off_field<-st_intersection(subset_off, hab_d)   
plot(st_geometry(off_field))

on_field<-st_intersection(subset_on, hab_d)
plot(st_geometry(on_field))


on_field<-on_field[!on_field$MediaSub=="Air",]
off_field<-off_field[!off_field$MediaSub=="Air",]

#get visual buffer
hab_buff<-st_buffer(hab_d, 10)
habitat<-mask(crop(nlcd, hab_d), hab_d) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### First, get Habitat tile 
habitat$title<-"Mixed Developed Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )
habplot
habitat<-habitat[,-4]


split_off<-split(off_field, list(off_field$Compound))
split_on<-split(on_field, list(on_field$Compound))

list_of_plots<-list()
for(compound in 1:length(split_off)){
  
  off_field_n<-split_off[[compound]]
  on_field_n<-split_on[[compound]]
  
  mediaplot<-ggplot()+
    geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
    scale_fill_grey(guide = "none") +
    ggnewscale:::new_scale_fill() +
    geom_sf(data = off_field_n, aes(fill = EEC)) +
    geom_sf(data = on_field_n, aes(fill = EEC),colour="white") +
    scale_fill_viridis_b(option = "D", 
                         name= "EED [ug/bee;ug/g]",
                         
                         # limits=lims,
                         # breaks=sc_fill_breaks
    )+
    facet_grid(.~MediaSub) +
    geom_sf(data = hab_buff, fill = NA) +
    theme_bw() +
    ylab(paste0(unique(off_field_n$ApplicationType)," Application"))+
    xlab("")+
    ggtitle(paste0(str_to_title(unique(off_field_n$Compound)),"-",off_field_n$Commodity))+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title.align=0.5,
      strip.text.x = element_text(size = 14, colour = "black", angle = 0),
      legend.position = "right",
      axis.text.x=element_blank(), 
      axis.ticks.x=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.y=element_blank(),
      
      axis.title.x=element_blank()
    )
  list_of_plots[[compound]]<-mediaplot
  
  
}

media_plots<-ggpubr::ggarrange(plotlist = list_of_plots, ncol=1)
media_plots

dev<-plot_grid(
  habplot,
  media_plots,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(2,4))
dev



#dev calculations
#get area by total habitat
just_area_on<-on_field[on_field$MediaSub=="Soil",]
#just_area_on<-on_field[on_field$Compound == "IMIDACLOPRID",]
just_area_on$area <- st_area(just_area_on)

area_out_on<-just_area_on %>%
  st_drop_geometry() %>%
  group_by(Compound) %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_on$area_per))

just_area_off<-off_field[off_field$MediaSub=="Soil",]
#just_area_off<-off_field[off_field$Compound == "IMIDACLOPRID",]
just_area_off$area <- st_area(just_area_off)

area_out_off<-just_area_off %>%
  st_drop_geometry() %>%
  group_by(Compound) %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_off$area_per)-sum(area_out_on$area_per))


#get area by specific compound
by_compound<-on_field[on_field$Compound=="IMIDACLOPRID",]
by_compound$area <- st_area(by_compound)
by_compound<-by_compound[1,]
area_out_on<-by_compound %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_on$area_per))


by_compound<-off_field[off_field$Compound=="IMIDACLOPRID",]
by_compound$area <- st_area(by_compound)
by_compound<-by_compound[1,]
area_out_off<-by_compound %>%
  mutate(area_per = (area/st_area(hab_ag))*100 )

print(sum(area_out_off$area_per))




#### Put together ----


get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 



gc()

nlcd<-raster(paste0(root_data_in, "/MapData/NLCD/Illinois/nlcd2013.tiff"))
crs(nlcd)<-crs(buf_hab)
habitat <- mask(crop(nlcd, buf_hab), buf_hab) #get nlcd within habitat
habitat<-rast(habitat)
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitatnlcd<-na.omit(habitat)


nlcdplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  
  scale_fill_manual(
    values=nlcdpalette, name="NLCD",
    labels = c('Water',
               'Developed, Open Space ',
               'Developed, Low Intensity',
               'Developed, Medium Intensity',
               "Developed, High Intensity",
               "Deciduous Forest", 
               "Evergreen Forest",
               "Mixed Forest",
               "Barren",
               "Shrub",
               "Grassland", 
               "Pasture/Hay",
               "Cultivated",
               "Woody Wetlands",
               "Herbaceous Wetlands"),
    drop = FALSE) +
  
  
  
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )

legend<-get_only_legend(nlcdplot)

agf<-plot_grid(
  legend,ag,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align="hv",  ncol=2, rel_widths = c(1,7))
agf

devf<-plot_grid(
  legend,dev,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align="hv",  ncol=2, rel_widths = c(1,7))
devf

natf<-plot_grid(
  legend,nat,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align="hv",  ncol=2, rel_widths = c(1,4))
natf


#need to fix the classes so cultivated is correct
#need to adjust nlcd legend 
#### Calculations ----
#get area


#foliar
folon<-st_intersection(sub_folon, hab_fol)   
folon<-folon[folon$MediaSub=="Soil",]
sum(st_area(folon))/st_area(hab_fol)*100

foloff<-st_intersection(sub_foloff, hab_fol)   
foloff<-foloff[foloff$MediaSub=="Soil",]
sum(st_area(foloff))/st_area(hab_fol)*100 - sum(st_area(folon))/st_area(hab_fol)*100

#soil
soilon<-st_intersection(sub_son, hab_s)   
soilon<-soilon[soilon$MediaSub=="Soil",]
sum(st_area(soilon))/st_area(hab_s)*100

soiloff<-st_intersection(sub_soff, hab_s)   
soiloff<-soiloff[soiloff$MediaSub=="Soil",]
sum(st_area(soiloff))/st_area(hab_s)*100 -sum(st_area(soilon))/st_area(hab_s)*100






############################################### Plots grouped on location ##################################
### Agricultural Habitat----
# sub_seedoff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "CLOTHIANIDIN",]
# sub_seedon<-onfield_tox_thresh[onfield_tox_thresh$Compound == "CLOTHIANIDIN",]

hab_ag<-buf_hab[buf_hab$id == 2,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(offfield_tox_thresh, hab_ag)   
plot(off_field["Compound"])
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA
  
on_field<-st_intersection(onfield_tox_thresh, hab_ag)
plot(on_field["Compound"])
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA


# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_ag, 10)
habitat<-mask(crop(nlcd, hab_ag), hab_ag) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Agriculturally Intensive Habitat"
#habitat<-as.factor(habitat$Layer_1, levels=namey)
habplot<-ggplot()+
  geom_raster(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale +
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )



habitat<-habitat[,-4]

off_field$ApplicationType<-ifelse(off_field$ApplicationType == "FoliarI" |off_field$ApplicationType == "FoliarH","Foliar", off_field$ApplicationType)
on_field$ApplicationType<-ifelse(on_field$ApplicationType == "FoliarI" |on_field$ApplicationType == "FoliarH","Foliar", on_field$ApplicationType)

off_field<-off_field[!off_field$MediaSub =="Air" |off_field$MediaSub =="Dust",  ]
on_field<-on_field[!on_field$MediaSub =="Air" |on_field$MediaSub =="Dust",  ]

off_by_app<-split(off_field, list(off_field$ApplicationType))
on_by_app<-split(on_field, list(on_field$ApplicationType))



plots_by_application<-list()
for(app in 1:length(off_by_app)){
  
  off_fielda<-off_by_app[[app]]
  on_fielda<-on_by_app[[app]]
  
 mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = off_fielda, aes(fill = EEC)) +
  geom_sf(data = on_fielda, aes(fill = EEC),colour="white") +
  scale_fill_viridis_b(option = "D", 
                       name= "EEC [ug/bee]",
                       labels = scales::scientific,
                       #limits = c(min(off_fielda$EEC), max(on_fielda$EEC))
                       # limits=lims,
                       breaks=seq(min(off_fielda$EEC), max(on_fielda$EEC), length.out=8 )
               
                      )+
  # facet_grid(.~MediaSub) +
   facet_grid(rows = vars(MediaSub), cols = vars(Compound))+
   #facet_wrap(~ Compound + MediaSub)+
  geom_sf(data = hab_buff, fill = NA) +
  theme_bw() +
   ylab("")+
   ggtitle(paste0(names(off_by_app[app]), " Application"))+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),

    axis.title.x=element_blank()
  )
 
 plots_by_application[[app]]<-mediaplot
}


app_plots<-ggpubr::ggarrange(plotlist =  plots_by_application, ncol=1)
ag_intense<-plot_grid(
   habplot,
   app_plots,
   # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
   hjust=0, vjust=0, align= "h",   nrow=1, rel_widths = c(1,4))
ag_intense
 

### Forested Habitat ----
# sub_seedoff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "CLOTHIANIDIN",]
# sub_seedon<-onfield_tox_thresh[onfield_tox_thresh$Compound == "CLOTHIANIDIN",]

hab_for<-buf_hab[buf_hab$id == 3,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(offfield_tox_thresh, hab_for)   
plot(off_field["Compound"])
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(onfield_tox_thresh, hab_for)
plot(on_field["Compound"])
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA


# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_for, 10)
habitat<-mask(crop(nlcd, hab_for), hab_for) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Forest Habitat"
#habitat<-as.factor(habitat$Layer_1, levels=namey)
habplot<-ggplot()+
  geom_raster(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale +
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )



habitat<-habitat[,-4]

off_field$ApplicationType<-ifelse(off_field$ApplicationType == "FoliarI" |off_field$ApplicationType == "FoliarH","Foliar", off_field$ApplicationType)
on_field$ApplicationType<-ifelse(on_field$ApplicationType == "FoliarI" |on_field$ApplicationType == "FoliarH","Foliar", on_field$ApplicationType)

off_field<-off_field[!off_field$MediaSub =="Air" |off_field$MediaSub =="Dust",  ]
on_field<-on_field[!on_field$MediaSub =="Air" |on_field$MediaSub =="Dust",  ]

off_by_app<-split(off_field, list(off_field$ApplicationType))
on_by_app<-split(on_field, list(on_field$ApplicationType))



plots_by_application<-list()
for(app in 1:length(off_by_app)){
  
  off_fielda<-off_by_app[[app]]
  on_fielda<-on_by_app[[app]]
  
  mediaplot<-ggplot()+
    geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
    scale_fill_grey(guide = "none") +
    ggnewscale:::new_scale_fill() +
    geom_sf(data = off_fielda, aes(fill = EEC)) +
    geom_sf(data = on_fielda, aes(fill = EEC),colour="white") +
    scale_fill_viridis_b(option = "D", 
                         name= "EEC [ug/bee]",
                         labels = scales::scientific,
                         #limits = c(min(off_fielda$EEC), max(on_fielda$EEC))
                         # limits=lims,
                         breaks=seq(min(off_fielda$EEC), max(on_fielda$EEC), length.out=8 )
                         
    )+
    # facet_grid(.~MediaSub) +
    facet_grid(rows = vars(MediaSub), cols = vars(Compound))+
    #facet_wrap(~ Compound + MediaSub)+
    geom_sf(data = hab_buff, fill = NA) +
    theme_bw() +
    ylab("")+
    ggtitle(paste0(names(off_by_app[app]), " Application"))+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title.align=0.5,
      strip.text.x = element_text(size = 14, colour = "black", angle = 0),
      legend.position = "right",
      axis.text.x=element_blank(), 
      axis.ticks.x=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.y=element_blank(),
      
      axis.title.x=element_blank()
    )
  
  plots_by_application[[app]]<-mediaplot
}


app_plots<-ggpubr::ggarrange(plotlist =  plots_by_application, ncol=2)
forest<-plot_grid(
  habplot,
  app_plots,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",   nrow=1, rel_widths = c(1,4))
forest



### Developed Habitat ----
# sub_seedoff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "CLOTHIANIDIN",]
# sub_seedon<-onfield_tox_thresh[onfield_tox_thresh$Compound == "CLOTHIANIDIN",]

hab_dev<-buf_hab[buf_hab$id == 1,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(offfield_tox_thresh, hab_dev)   
plot(off_field["Compound"])
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(onfield_tox_thresh, hab_dev)
plot(on_field["Compound"])
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA


# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_dev, 10)
habitat<-mask(crop(nlcd, hab_dev), hab_dev) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Developed, Open Space Habitat"
#habitat<-as.factor(habitat$Layer_1, levels=namey)
habplot<-ggplot()+
  geom_raster(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale +
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )



habitat<-habitat[,-4]

off_field$ApplicationType<-ifelse(off_field$ApplicationType == "FoliarI" |off_field$ApplicationType == "FoliarH","Foliar", off_field$ApplicationType)
on_field$ApplicationType<-ifelse(on_field$ApplicationType == "FoliarI" |on_field$ApplicationType == "FoliarH","Foliar", on_field$ApplicationType)

off_field<-off_field[!off_field$MediaSub =="Air" |off_field$MediaSub =="Dust",  ]
on_field<-on_field[!on_field$MediaSub =="Air" |on_field$MediaSub =="Dust",  ]

#this particular spot doesn't add much to the plot
off_field<-off_field[!off_field$ApplicationType == "Seed",]
on_field<-on_field[!on_field$ApplicationType == "Seed",]

off_by_app<-split(off_field, list(off_field$ApplicationType))
on_by_app<-split(on_field, list(on_field$ApplicationType))



plots_by_application<-list()
for(app in 1:length(off_by_app)){
  
  off_fielda<-off_by_app[[app]]
  on_fielda<-on_by_app[[app]]
  
  mediaplot<-ggplot()+
    geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
    scale_fill_grey(guide = "none") +
    ggnewscale:::new_scale_fill() +
    geom_sf(data = off_fielda, aes(fill = EEC)) +
    geom_sf(data = on_fielda, aes(fill = EEC),colour="white") +
    scale_fill_viridis_b(option = "D", 
                         name= "EEC [ug/bee]",
                         labels = scales::scientific,
                         #limits = c(min(off_fielda$EEC), max(on_fielda$EEC))
                         # limits=lims,
                         breaks=seq(min(off_fielda$EEC), max(on_fielda$EEC), length.out=8 )
                         
    )+
    # facet_grid(.~MediaSub) +
    facet_grid(rows = vars(MediaSub), cols = vars(Compound))+
    #facet_wrap(~ Compound + MediaSub)+
    geom_sf(data = hab_buff, fill = NA) +
    theme_bw() +
    ylab("")+
    ggtitle(paste0(names(off_by_app[app]), " Application"))+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title.align=0.5,
      strip.text.x = element_text(size = 14, colour = "black", angle = 0),
      legend.position = "right",
      axis.text.x=element_blank(), 
      axis.ticks.x=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.y=element_blank(),
      
      axis.title.x=element_blank()
    )
  
  plots_by_application[[app]]<-mediaplot
}


app_plots<-ggpubr::ggarrange(plotlist =  plots_by_application, ncol=1)
dev<-plot_grid(
  habplot,
  app_plots,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",   nrow=1, rel_widths = c(1,4))
dev

### Mixed Habitat ----
# sub_seedoff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "CLOTHIANIDIN",]
# sub_seedon<-onfield_tox_thresh[onfield_tox_thresh$Compound == "CLOTHIANIDIN",]

hab_mix<-buf_hab[buf_hab$id == 7,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon

off_field<-st_intersection(offfield_tox_thresh, hab_mix)  
st_write(off_field, paste0(root_data_out, "/off_field_area.shp"), driver = "ESRI Shapefile")
plot(off_field["Compound"])
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(onfield_tox_thresh, hab_mix)
st_write(on_field, paste0(root_data_out, "/on_field_area.shp"), driver = "ESRI Shapefile")
plot(on_field["Compound"])
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA


# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_mix, 10)
habitat<-mask(crop(nlcd, hab_mix), hab_mix) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Mixed Habitat"
#habitat<-as.factor(habitat$Layer_1, levels=namey)
habplot<-ggplot()+
  geom_raster(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale +
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )



habitat<-habitat[,-4]

off_field$ApplicationType<-ifelse(off_field$ApplicationType == "FoliarI" |off_field$ApplicationType == "FoliarH","Foliar", off_field$ApplicationType)
on_field$ApplicationType<-ifelse(on_field$ApplicationType == "FoliarI" |on_field$ApplicationType == "FoliarH","Foliar", on_field$ApplicationType)

off_field<-off_field[!off_field$MediaSub =="Air" |off_field$MediaSub =="Dust",  ]
on_field<-on_field[!on_field$MediaSub =="Air" |on_field$MediaSub =="Dust",  ]

off_field<-off_field[!off_field$Compound == "CHLORPYRIFOS", ]
on_field<-on_field[!on_field$Compound == "CHLORPYRIFOS", ]

off_by_app<-split(off_field, list(off_field$ApplicationType))
on_by_app<-split(on_field, list(on_field$ApplicationType))



plots_by_application<-list()
for(app in 1:length(off_by_app)){
  app=1
  off_fielda<-off_by_app[[app]]
  on_fielda<-on_by_app[[app]]
  
  mediaplot<-ggplot()+
    geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
    scale_fill_grey(guide = "none") +
    ggnewscale:::new_scale_fill() +
    geom_sf(data = off_fielda, aes(fill = EEC)) +
    geom_sf(data = on_fielda, aes(fill = EEC),colour="white") +
    scale_fill_viridis_b(option = "D", 
                         name= "EEC [ug/bee]",
                         labels = scales::scientific,
                         #limits = c(min(off_fielda$EEC), max(on_fielda$EEC))
                         # limits=lims,
                         breaks=seq(min(off_fielda$EEC), max(on_fielda$EEC), length.out=8 )
                         
    )+
    # facet_grid(.~MediaSub) +
    facet_grid(rows = vars(MediaSub), cols = vars(Compound))+
    #facet_wrap(~ Compound + MediaSub)+
    geom_sf(data = hab_buff, fill = NA) +
    theme_bw() +
    ylab("")+
    ggtitle(paste0(names(off_by_app[app]), " Application"))+
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title.align=0.5,
      strip.text.x = element_text(size = 14, colour = "black", angle = 0),
      legend.position = "right",
      axis.text.x=element_blank(), 
      axis.ticks.x=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.y=element_blank(),
      
      axis.title.x=element_blank()
    )
  
  plots_by_application[[app]]<-mediaplot
}


app_plots<-ggpubr::ggarrange(plotlist =  plots_by_application, ncol=3)
mix<-plot_grid(
  habplot,
  app_plots,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",   nrow=1, rel_widths = c(1,4))
mix

#### Put together ----

all<-plot_grid(
  ag_intense,
  forest,
  mix,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align="hv",  ncol=1, nrow=4, rel_widths = c(3,2,1))
all


#grid.arrange(seed, foliar, soil, ncol = 1)



get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 



gc()

nlcd<-raster(paste0(root_data_in, "/MapData/NLCD/Illinois/nlcd2013.tiff"))
crs(nlcd)<-crs(buf_hab)
habitat <- mask(crop(nlcd, buf_hab), buf_hab) #get nlcd within habitat
habitat<-rast(habitat)
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitatnlcd<-na.omit(habitat)


nlcdplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+

  scale_fill_manual(
    values=nlcdpalette, name="NLCD",
    labels = c('Water',
               'Developed, Open Space ',
               'Developed, Low Intensity',
               'Developed, Medium Intensity',
               "Developed, High Intensity",
               "Deciduous Forest", 
               "Evergreen Forest",
               "Mixed Forest",
               "Barren",
               "Shrub",
               "Grassland", 
               "Pasture/Hay",
               "Cultivated",
               "Woody Wetlands",
               "Herbaceous Wetlands"),
    drop = FALSE) +



  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )

legend<-get_only_legend(nlcdplot)



final<-plot_grid(
  legend,
  all,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h", nrow=1, rel_widths = c(1.3,7))
final

#need to fix the classes so cultivated is correct
#need to adjust nlcd legend 

#### Calculations ----
#get area

#seed
seedon<-st_intersection(sub_seedon, hab_seed)   
seedon<-seedon[seedon$MediaSub=="Soil",]
sum(st_area(seedon))/st_area(hab_seed)*100

seedoff<-st_intersection(sub_seedoff, hab_seed)   
seedoff<-seedoff[seedoff$MediaSub=="Soil",]
sum(st_area(seedoff))/st_area(hab_seed)*100 -sum(st_area(seedon))/st_area(hab_seed)*100

#foliar
folon<-st_intersection(sub_folon, hab_fol)   
folon<-folon[folon$MediaSub=="Soil",]
sum(st_area(folon))/st_area(hab_fol)*100

foloff<-st_intersection(sub_foloff, hab_fol)   
foloff<-foloff[foloff$MediaSub=="Soil",]
sum(st_area(foloff))/st_area(hab_fol)*100 - sum(st_area(folon))/st_area(hab_fol)*100

#soil
soilon<-st_intersection(sub_son, hab_s)   
soilon<-soilon[soilon$MediaSub=="Soil",]
sum(st_area(soilon))/st_area(hab_s)*100

soiloff<-st_intersection(sub_soff, hab_s)   
soiloff<-soiloff[soiloff$MediaSub=="Soil",]
sum(st_area(soiloff))/st_area(hab_s)*100 -sum(st_area(soilon))/st_area(hab_s)*100






### Get total area percentages----

habitat_area<-sum(st_area(bomb_h))
on_field_areac<-sum(st_area(on_field_moving_averages))
off_field_areac<-sum(st_area(off_field_moving_averages))

total_area_zone<-off_field_areac/habitat_area
TOTAL_AREA<-total_area_zone*100
TOTAL_AREA

#what percentage is foliar?
on_field_area_foliar<-on_field_moving_averages[on_field_moving_averages$ApplicationType == "FoliarH" |on_field_moving_averages$ApplicationType == "FoliarI" ,]
(sum(st_area(on_field_area_foliar))/habitat_area)*100

off_field_area_foliar<-off_field_moving_averages[off_field_moving_averages$ApplicationType == "FoliarH" |off_field_moving_averages$ApplicationType == "FoliarI" ,]
(sum(st_area(off_field_area_foliar))/habitat_area)*100 - (sum(st_area(on_field_area_foliar))/habitat_area)*100

#what percentage is seed?
on_field_area_seed<-on_field_moving_averages[on_field_moving_averages$ApplicationType == "Seed" ,]
(sum(st_area(on_field_area_seed))/habitat_area)*100

off_field_area_seed<-off_field_moving_averages[off_field_moving_averages$ApplicationType == "Seed" ,]
(sum(st_area(off_field_area_seed))/habitat_area)*100 - (sum(st_area(on_field_area_seed))/habitat_area)*100

#what percentage is soil?
on_field_area_soil<-on_field_moving_averages[on_field_moving_averages$ApplicationType == "Soil" ,]
(sum(st_area(on_field_area_soil))/habitat_area)*100

off_field_area_soil<-off_field_moving_averages[off_field_moving_averages$ApplicationType == "Soil" ,]
(sum(st_area(off_field_area_soil))/habitat_area)*100 - (sum(st_area(on_field_area_soil))/habitat_area)*100
##### Scraps with tmap ----
habitat<-mask(crop(nlcd, hab_seed), hab_seed) 
  tm_shape(habitat, bbox = bbox_new)+
  tm_raster(style = "cat", palette = cdlpalette, legend.show=FALSE)+
  #tm_raster(style = "cat", palette = cdlpalette, title="NLCD")+
  tm_shape(off_field, bbox = bbox_new)+
  tm_polygons(fill="EEC",
              col="EEC",
              style="cont",
              title = "Off-field EEC [ug/bee]",
              scientific=T,digits=5,
              palette=get_brewer_pal(palette="GnBu", n=5, plot=FALSE), aplha=0.3)+
  
  tm_shape(on_field, bbox = bbox_new)+
  tm_polygons(fill="EEC",
              col="EEC",
              style="cont",
              title = "On-field EEC [ug/bee]",
              palette=get_brewer_pal(palette="OrRd", n=5, plot=FALSE), alpha=0.3)+
    tm_facets(by = "MediaSub",  nrow = 1,  free.coords = TRUE, sync = TRUE, scale.factor = 10)+  # set free.coords to TRUE+
  
  tm_shape(hab_buff, bbox = bbox_new)+
  tm_polygons(alpha=0.1)+
  
  
  tm_legend(position=c("right", "top"), bg.color="grey95", frame=TRUE)+
  tm_layout(title= '      Seed',
            panel.labels = c("Pollen", "Soil"),
            title.position = c('right', 'top'), frame = F, frame.lwd = NA, panel.label.bg.color = NA)+
  tm_add_legend('fill', 
                col = cdlpalette,
                border.col = "grey40",
                labels = c('Water','Developed, Open Space ','Developed, Low Intensity','Developed, Medium Intensity', "Developed, High Intensity",
                           "Barren", "Deciduous Forest", "Evergreen Forest", "Mixed Forest","Shrub","Grassland", "Pasture/Hay","Cultivated","Woody Wetlands", "Herbaceous Wetlands"),
                title="NLCD")

  
  
  
  
  
  
nectar<-tmap_grob(nectar)
pollen<-tmap_grob(pollen)

example<-plot_grid(
  pollen,
  nectar,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(3,3))
example



# tm_shape(habitat)+
#   tm_raster(style = "cat", palette = cdlpalette, legend.show=FALSE)+
#   #tm_raster(style = "cat", palette = cdlpalette, title="NLCD")+
#   tm_shape(offfield_nect)+
#   tm_polygons(fill="EEC",
#               col="EEC",
#               style="cont",
#               title = "Off-field EEC [ug/bee]",
#               palette=get_brewer_pal(palette="GnBu", n=5, plot=FALSE), aplha=0.3)+
#   
#   tm_shape(onfield_nect)+
#   tm_polygons(fill="EEC",
#               col="EEC",
#               style="cont",
#               title = "On-field EEC [ug/bee]",
#               palette=get_brewer_pal(palette="OrRd", n=5, plot=FALSE), alpha=0.3)+
#   
#   tm_shape(buf_hab_sub_buffer)+
#   tm_polygons(alpha=0.1)+
#   
#   tm_facets(by = "id",  nrow = 3,  free.coords = TRUE, inside.original.bbox=T, sync = TRUE, scale.factor = 3)+  # set free.coords to TRUE+
#   
#   tm_legend(position=c("right", "top"), bg.color="grey95", frame=TRUE)+
#   tm_layout(title= '      Nectar', 
#             panel.labels = c("Colony 1", "Colony 2", "Colony 3"),
#             title.position = c('right', 'top'), frame = F, frame.lwd = NA, panel.label.bg.color = NA)+
#   tm_add_legend('fill', 
#                 col = cdlpalette,
#                 border.col = "grey40",
#                 labels = c('Water','Developed, Open Space ','Developed, Low Intensity','Developed, Medium Intensity', "Developed, High Intensity",
#                            "Barren", "Deciduous Forest", "Evergreen Forest", "Mixed Forest","Shrub","Grassland", "Pasture/Hay","Cultivated","Woody Wetlands", "Herbaceous Wetlands"),
#                 title="NLCD")


# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)
# 
# folplot<-ggplot()+
#   geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
#   colScale+
#   scale_fill_manual(values=c(nlcdpalette), name="NLCD",
#                     labels = c('Water',
#                                'Developed, Open Space ',
#                                'Developed, Low Intensity',
#                                'Developed, Medium Intensity',
#                                "Developed, High Intensity",
#                                "Deciduous Forest", 
#                                "Evergreen Forest",
#                                "Mixed Forest",
#                                "Barren",
#                                "Shrub",
#                                "Grassland", 
#                                "Pasture/Hay","Cultivated",
#                                "Woody Wetlands",
#                                "Herbaceous Wetlands")) +
#   ggnewscale:::new_scale_fill() +
#   geom_sf(data = off_field, aes(fill = EEC), alpha=0.5) +
#   geom_sf(data = on_field, aes(fill = EEC)) +
#   scale_fill_viridis_c(option = "B", 
#                        name= "EEC [ug/bee]"
#                        # limits=lims,
#                        # breaks=sc_fill_breaks
#   )+
#   geom_sf(data = hab_buff, fill = NA) +
#   facet_grid(MediaSub ~ ., ncol=4) +
#   theme_bw() +
#   theme(
#     legend.title.align=0.5,
#     strip.text.x = element_text(size = 14, colour = "black", angle = 0),
#     legend.position = "right",
#     axis.text.x=element_blank(), 
#     axis.ticks.x=element_blank(), 
#     axis.text.y=element_blank(), 
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank()
#   )
# 
# seed
# 
# example<-plot_grid(
#   pollen,
#   nectar,
#   # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
#   hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(3,3))
# example
# 




#### Animations ----
  
  # onfield<-st_read( dsn=paste0(root_data_out, "/all_bombus/outputforshiny/on_field_movingavg.shp"))
  # on_field_sub<-onfield[onfield$MediaSb == "Pollen",]
  # 
  # 
  # 
  # 
  # ui <- fluidPage(
  #   plotOutput("map"),
  #   selectInput("var", "Variable", on_field_sub)
  #  # selectInput(inputId = "inputCompound", label = "Select Compound:", multiple = TRUE, choices = sort(on_field_sub$Compond), selected = "GLYPHOSATE")
  # )
  # 
  # server <- function(input, output, session) {
  #   
  #   
  #   
  #   output$map <- renderPlot({
  #     #tmap_mode("view")
  #     tm_shape(on_field_sub) +
  #       tm_polygons(input$var)
  #   })
  #   
  #   
  # }
  # 
  # shinyApp(ui, server)
  # 
  # 
  
  
  
  
  
  
  
  
  
  
  
