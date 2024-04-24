### Foraging Landscape

### 05 visualize exposure landscape

print("stepping into 05: visualizing exposure landscape")

##### First get habitat layer ----

library(purrr)
library(magick)

nlcd<-rast(paste0(root_data_in, "/MapData/NLCD/Illinois/nlcd2013.tiff"))

point<- st_read(paste0(bombus_dir,"/foraging/"), layer = "colonylocation")

colony<- st_read(paste0(bombus_dir,"/foraging/"), layer = "colonylocation")
colony<- st_transform(colony, crs(nlcd)) #reproject to match extent of DF
colony<-st_buffer(colony, 1000)

habitat <- mask(crop(nlcd, colony), colony) #get nlcd within habitat


nlcdpalette<-c("cornflowerblue","bisque","lightsalmon","firebrick","darkred","darkolivegreen3","darkkhaki","lemonchiffon3","khaki1","chocolate4","cadetblue2","cadetblue4")
#namey<-c(11,21,22,23,24,41,42,43,52,71,81,82,90,95)


nlcdlayer<-as.data.frame(habitat)
names(nlcdpalette)<-sort(unique(nlcdlayer$Layer_1))

colScale<- scale_fill_manual(
  values=nlcdpalette, name="NLCD",
  labels = c('Water',
             'Developed, Open Space ',
             'Developed, Low Intensity',
             'Developed, Medium Intensity',
             "Developed, High Intensity",
             "Deciduous Forest", 
             "Shrub",
             "Grassland",
             "Pasture/Hay",
             "Cultivated",
             "Woody Wetlands",
             "Herbaceous Wetlands"),
  drop = FALSE) 



#get visual buffer
hab_buff<-st_buffer(colony, 20)
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-ceiling(as.numeric(habitat$Layer_1))
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### First, get Habitat tile 
habitat$title<-"B. affinis Colony Location"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  geom_sf(data = hab_buff, fill = NA) +
  geom_sf(data = point, aes(size=1.8),fill = "grey",show.legend = FALSE)+
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "left",
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


########Now get simulation map ----

nlcd<-rast(paste0(root_data_in, "/MapData/NLCD/Illinois/nlcd2013_f.tiff"))

point<- st_read(paste0(bombus_dir,"/foraging/"), layer = "colonylocation")

colony<- st_read(paste0(bombus_dir,"/foraging/"), layer = "colonylocation")
colony<- st_transform(colony, crs(scenarios_f)) #reproject to match extent of DF
colony<-st_buffer(colony, 1000)

habitat <- mask(crop(nlcd, colony), colony) #get nlcd within habitat

#get visual buffer
hab_buff<-st_buffer(colony, 20)
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-ceiling(as.numeric(habitat$Layer_1))
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


print(list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014/sub"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
scenarios<- file.path(paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014/sub"), list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014/sub"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
scenarios<-setNames(lapply(scenarios, st_read), tools::file_path_sans_ext(basename(scenarios)))
scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]

sim<-scenarios[[2]]


sim <- sim %>% 
  st_transform(., crs = 26916) %>% 
  st_make_valid()

#here, we'll pull out the 2014 data sets and extract that layer to a single colony location. then all code that follows will just be the field histories in that spot. 
colony<- st_read(paste0(bombus_dir,"/foraging/"), layer = "colonylocation")
colony<- st_transform(colony, crs(sim)) #reproject to match extent of DF
colony<-st_buffer(colony, 1000)

scenarios_f<-st_intersection(sim,colony)

#this addresses any polygons that are of the same field but were separated due to area calculations in the intersection analysis; 
 x<-scenarios_f
  output<- x %>%
    group_by(id) %>%
    filter(n()>1) %>%
    group_by(id,Compond,  Commdty,  Year,     ApplctT,  crop,     area,    plntddt,  applctn) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup()
  
  x<-x[,-c(8,11:13)]
  
  originaldf<- x %>%
    group_by(id) %>%
    filter(!n()>1)
  originaldf<-originaldf[,c(7,1:6,8:10)]
  #names(originaldf)
  
  finaldf<-rbind(originaldf,output)
  sim<-finaldf
  sim<-na.omit(sim)
  sim$Compond<-str_to_title(sim$Compond)


  
  
  
  
  
  
#### Now put together ----
  #sim$title<-"Example Exposure Landscape"
mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = sim, aes(fill = Compond), alpha=0.8) +
 # scale_fill_brewer(palette = "Set1")+
  scale_fill_manual(values = c("goldenrod2", "darkcyan", "darkorange3", "seagreen4"))+

  #facet_grid(.~title) +
  theme_bw() +
  xlab("")+
  guides(fill=guide_legend(title="Compound"))+
  ggtitle("Example Scenario")+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_blank())

mediaplot




colonyplot<-plot_grid(
  habplot,
  mediaplot,
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(3,4))
colonyplot










### pull out a single scenario to demonstrate both a static map for the manuscript and a gif for the defense ----

sim<-5
# #### BETA TESTING CODE
final_on_field_history<-final_on_field_history_list[[sim]]
final_off_field_history_30m<-final_off_field_history_30m_list[[sim]]
final_off_field_history_60m<-final_off_field_history_60m_list[[sim]]
final_off_field_history_90m<-final_off_field_history_90m_list[[sim]]

# #this removes any scenarios for which we don't have data, thus it throws off the function
final_on_field_history      <-keep(final_on_field_history, ~all(ncol(.x) >= 14))
final_off_field_history_30m <-keep(final_off_field_history_30m, ~all(ncol(.x) >= 14))
final_off_field_history_60m      <-keep(final_off_field_history_60m, ~all(ncol(.x) >= 14))
final_off_field_history_90m      <-keep(final_off_field_history_90m, ~all(ncol(.x) >= 14))

 
# # #we're not going to include any soil applications; hence we remove them. in the future this will likely need to be modified
final_on_field_history<-purrr::discard(final_on_field_history, ~any(.x$ApplicationType == "Soil"))
final_off_field_history_30m <-purrr::discard(final_off_field_history_30m , ~any(.x$ApplicationType == "Soil"))
final_off_field_history_60m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))
final_off_field_history_90m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))


### For this section, we'll just take a neonic
scenario_clip_on<-final_on_field_history[[1]]
scenario_clip_off30<-final_off_field_history_30m[[1]]
scenario_clip_off60<-final_off_field_history_60m[[1]]
scenario_clip_off90<-final_off_field_history_90m[[1]]


scenario_clip_on$loc<-"On"
scenario_clip_off30$loc<-"Off30"
scenario_clip_off60$loc<-"Off60"
scenario_clip_off90$loc<-"Off90"

scenario_clip_on<-scenario_clip_on[,c(1:13,ncol(scenario_clip_on),14:(ncol(scenario_clip_on)-1))]
scenario_clip_off30<-scenario_clip_off30[,c(1:13,ncol(scenario_clip_off30),14:(ncol(scenario_clip_off30)-1))]
scenario_clip_off60<-scenario_clip_off60[,c(1:13,ncol(scenario_clip_off60),14:(ncol(scenario_clip_off60)-1))]
scenario_clip_off90<-scenario_clip_off90[,c(1:13,ncol(scenario_clip_off90),14:(ncol(scenario_clip_off90)-1))]



#remove any rows that are filler from the multi-year run
remove_filler<- function(x) {x %>% group_by(id) %>%
    filter(!(is.na(Soil) & sum(is.na(Soil)) > 300)) %>%
    ungroup}

scenario_clip_on<-remove_filler(scenario_clip_on)
scenario_clip_off30<-remove_filler(scenario_clip_off30)
scenario_clip_off60<-remove_filler(scenario_clip_off60)
scenario_clip_off90<-remove_filler(scenario_clip_off90)


scenario_clip_on<-     gather(scenario_clip_on, "Media", "Value", 15:ncol(scenario_clip_on))
scenario_clip_off30<-  gather(scenario_clip_off30, "Media", "Value", 15:ncol(scenario_clip_off30))
scenario_clip_off60<-  gather(scenario_clip_off60, "Media", "Value", 15:ncol(scenario_clip_off60))
scenario_clip_off90<-  gather(scenario_clip_off90, "Media", "Value", 15:ncol(scenario_clip_off90))


scenario_clip<-rbind(scenario_clip_on,scenario_clip_off30,scenario_clip_off60,scenario_clip_off90)


#Split by media
#by_media_list<-split(scenario_clip,list(scenario_clip$Media))


by_media<-scenario_clip


       #this function will allow us to erase each subsequent polygon from its matched buffer at the appropriate size (i.e., distance from field)
        erase_poly<-function(x,y){
          buffer <-st_union(x)
          og <-st_union(y)
          output <- ms_erase(as_Spatial(buffer),as_Spatial(og)) #subtract original polygon
          getbuf<-st_as_sfc(output) # set the sf geometry as the new buffer
          outputf <- st_set_geometry(y, rep(getbuf, length=nrow(y)))# set geometry in old df, return sf
          outputf
        }
        
          onfield<-by_media[by_media$loc == "On",]
          offfield30<-by_media[by_media$loc == "Off30",]
          offfield60<-by_media[by_media$loc == "Off60",]
          offfield90<-by_media[by_media$loc == "Off90",]
          
          #set up each on scenario as an sf, and set raster conditions based on that sf, on-field
          ondf<-st_as_sf(onfield)
          plot(ondf$geometry,col="grey")
          
          #set up each off scenario as an sf, and set raster conditions based on that sf, 30m
          offdf30<-st_as_sf(offfield30)
          buffer30<-st_buffer(offdf30,30)
          #split
          buffer30<-split(buffer30,list(buffer30$id))
          offdf30<-split(offdf30,list(offdf30$id))
          buf30<-mapply(erase_poly,buffer30,offdf30,SIMPLIFY=FALSE) #subtract from OG field
          buf30<- st_as_sf(do.call(rbind,buf30))
          #plot
          plot(buf30$geometry,add=T,col="red")

          #set up each off scenario as an sf, and set raster conditions based on that sf, 60m
          offdf60<-st_as_sf(offfield60)
          buffer60<-st_buffer(offdf60,60)
          #split
          buffer60<-split(buffer60,list(buffer60$id))
          buf60<-mapply(erase_poly,buffer60,buffer30,SIMPLIFY=FALSE) #subtract 30m from field
          buf60<- st_as_sf(do.call(rbind,buf60))
          #plot
          plot(buf60$geometry, add=T, col="purple")

          #set up each off scenario as an sf, and set raster conditions based on that sf, 90m
          offdf90<-st_as_sf(offfield90)
          buffer90<-st_buffer(offdf90,90)
          #split
          buffer90<-split(buffer90,list(buffer90$id))
          buf90<-mapply(erase_poly,buffer90,buffer60,SIMPLIFY=FALSE) #subtract 60m from field
          buf90<- st_as_sf(do.call(rbind,buf90))
          #plot
          plot(buf90$geometry, add=T, col="green")
          
          
          
          
          

          ##let's get the base layer 
          hab_buff<-st_buffer(colony, 100)
          habitat<-mask(crop(nlcd, hab_buff), hab_buff) 
          habitat<-as.data.frame(habitat,xy = TRUE)
          habitat$Layer_1<-as.factor(habitat$Layer_1)
          habitat<-na.omit(habitat)
          
          basemap<-
            ggplot()+
            geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
            scale_fill_grey(guide = "none") 
          basemap
          
          
          
          hist(ondfx$Value)
          ondfx<-ondfx[!ondfx$Media == "Dust",]
      
           limits<-c(0,6.075007)

           nbreaks<- seq(from = 0.00005, to = 0.30, length=11)
   
        conc_map<-function(day){    
          breaks<-c(min(buf30$Value), median(buf60$Value), max(ondf$Value))
         
          ggplot() +
            geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
            scale_fill_grey(guide = "none") +
            ggnewscale:::new_scale_fill() +
            theme_bw()+
            geom_sf(data = ondf[ondf$Day ==day, ], aes(fill=Value)) +
            geom_sf(data = buf30[buf30$Day ==day,], aes(fill = Value),colour="lightgrey") +
            geom_sf(data = buf60[buf60$Day ==day,], aes(fill = Value),colour="lightgrey") +
            geom_sf(data = buf90[buf90$Day ==day,], aes(fill = Value),colour="lightgrey") +
            # scale_fill_viridis_b(option = "D", 
            #                      limits=limits,
            #                      breaks=breaks,
            #                      name="EEC")+
            #theme(legend.position="none")+
            ggtitle(paste0(ondf$date[ondf$Day == day][1]))+
                                 
          facet_wrap(~Media, ncol = 2,nrow=2)
          
          print(paste0("saving plot ", day))
          ggsave(filename = paste0(root_data_out,"/animations/",day+1000,".png"),
                 width = 8,height=8,dpi = 150)             

        }
          
          
          
          seq(from = 187, to=197, by=1) %>% 
            map_df(conc_map)
    
          
          
          
list.files(path = paste0(root_data_out,"/animations/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=10) %>% # animates, can opt for number of loops
  image_write(paste0(root_data_out,"/animations/gif", "conc_clothianidin_fastest.gif")) # write to current dir

