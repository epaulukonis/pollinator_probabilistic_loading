helpy<-merge(x=chem, y = matching[ , c("Compound","ApplicationType","Commodity","Value","MediaSub","day" )],
             by.x = c("Compound","ApplicationType","Commodity","dayn"), 
             by.y = c("Compound","ApplicationType","Commodity","day"), all.x=TRUE)


help<-right_join(testy,dust,by=c("Compound","ApplicationType","Commodity"))


help<-help[help$Compound == "CLOTHIANIDIN",]
help$day<-help$day.x+help$day.y
helpy<-merge(x =chem, y = help[ , c("Compound","ApplicationType","Commodity","Value","MediaSub","day" )], by = c("Compound","ApplicationType","Commodity","day"), all.x=TRUE)


helpy<-helpy[with(helpy, order(MediaSub,day)), ]






####Process the off-field model outputs and rasterize them into a stack organized by, in layers, 1.Day, 2.Medium, 3.Field ----
#pull out clip
scenario_clip<-final_on_field_history[[1]]
rasterize_by_day<-function(x){
  
  scenario_clip<-x
  
  scenario_clip<-scenario_clip %>% group_by(Compound) %>%
    filter(!(is.na(Soil) & sum(is.na(Soil)) > 300)) %>%
    ungroup
  
  #set up each scenario as an sf, and set raster conditions based on that sf
  scenario_clip<-st_as_sf(scenario_clip)
  r.raster <- raster()
  extent(r.raster) <- extent(scenario_clip)
  crs(r.raster)<-crs(scenario_clip)
  res(r.raster) <- 30
  
  list_by_media<-list()
  for(medium in 14:ncol(scenario_clip)){ 
    medium<-14
    scenario_clip_m<-scenario_clip[,c(1:13,medium)]
    
    # rasterize by day and stack, then read to an additional list
    daily_scenario<-list()
    for(day in 1:365){
      day<-1
      date<-scenario_clip_m[day,]
      output<- rast(rasterize(x = date, y = r.raster, field = names(date[,14])[1])) #spatraster
      m <- merge(output,habitat)
      daily_scenario[[day]] <- m
    }
    
    
    daily_scenario_stack<-rast(daily_scenario) #stack it into one file
    list_by_media[[medium-13]]<- daily_scenario_stack
    
  }
  
  
  
  set_of_names<-head(names(scenario_clip[,14:ncol(scenario_clip)]),-1)
  names(list_by_media)<-set_of_names
  
  list_by_media
  
}

start.time<-Sys.time()
on_field_rasters<-lapply(final_on_field_history, rasterize_by_day)
end.time<-Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

#about 12 minutes to run





# ggplot()+
#   geom_sf(data=offdf30,color="grey40")+
#   geom_sf_text(data=offdf30,aes(label=id))
#  
# ggplot()+
#   geom_sf(data=buffer30,color="grey40")+
#   geom_sf_text(data=buffer30,aes(label=id))




}else{ #if there are not on-field values (i.e., nectar only off-field...)
  offfield30<-by_media[by_media$loc == "Off30",]
  offfield60<-by_media[by_media$loc == "Off60",]
  offfield90<-by_media[by_media$loc == "Off90",]
  
  
  #set up each off scenario as an sf, and set raster conditions based on that sf, 30m
  offdf30<-st_as_sf(offfield30)
  buffer30<-st_buffer(offdf30,30)
  system.time(buf30 <- ms_erase(as_Spatial(buffer30),as_Spatial(offdf30))) #subtract original polygon
  buf30<-st_as_sf(buf30)
  plot(buf30$geometry,col="red")
  roff.raster30 <- raster()
  extent(roff.raster30) <- extent(buf30)
  crs(roff.raster30)<-crs(buf30)
  res(roff.raster30) <- 30
  
  #set up each off scenario as an sf, and set raster conditions based on that sf, 60m
  offdf60<-st_as_sf(offfield60)
  buffer60<-st_buffer(offdf60,60)
  system.time(buf60 <- ms_erase(as_Spatial(buffer60),as_Spatial(buffer30))) # subtract buf 30
  buf60<-st_as_sf(buf60)
  plot(buf60$geometry,add=T, col="purple")
  roff.raster60 <- raster()
  extent(roff.raster60) <- extent(buf60)
  crs(roff.raster60)<-crs(buf60)
  res(roff.raster60) <- 30
  
  #set up each off scenario as an sf, and set raster conditions based on that sf, 90m
  offdf90<-st_as_sf(offfield90)
  buffer90<-st_buffer(offdf90,90)
  system.time(buf90 <- ms_erase(as_Spatial(buffer90),as_Spatial(buffer60))) #subtract buf 60
  buf90<-st_as_sf(buf90)
  plot(buf90$geometry, add=T, col="green")
  roff.raster90 <- raster()
  extent(roff.raster90) <- extent(buf90)
  crs(roff.raster90)<-crs(buf90)
  res(roff.raster90) <- 30
  
  # rasterize by day and stack, then read to an additional list
  daily_scenario<-list()
  for(day in 1:365){
    day<-195
    
    offdf30<-buf30[day,]
    offdf30$Value<-as.numeric(offdf30$Value)
    
    offdf60<-buf60[day,]
    offdf60$Value<-as.numeric(offdf60$Value)
    
    offdf90<-buf90[day,]
    offdf90$Value<-as.numeric(offdf90$Value)
    
    output_off30<- rast(rasterize(x = offdf30, y = roff.raster30, field = "Value")) #spatraster
    output_off60<- rast(rasterize(x = offdf60, y = roff.raster60, field = "Value")) #spatraster
    output_off90<- rast(rasterize(x = offdf90, y = roff.raster90, field = "Value")) #spatraster
    
    # plot(output_on)
    plot(output_off30)
    plot(output_off60,add=T)
    plot(output_off90,add=T)
    
    
    m <-merge(output_off30,habitat) #merge off
    plot(m)
    m <-merge(output_off60,m) #merge off
    plot(m)
    m <-merge(output_off90,m) #merge off
    #plot(m)
    
    plot(m)
    
    mrec_weight <- c(1e-60,1011,0.5,
                     1011,1012,0,
                     1012,1024,0.5,
                     1024,1025, 0.1,
                     1025,1032, 0,
                     1032,1042,1,
                     1042,1043,0.5,
                     1043,1044,0.75,
                     1044,1073,1,
                     1073,1083,0.5,
                     1083,1096, 0.25
    )
    
    rclmat_weight <- matrix(mrec_weight, ncol=3, byrow=TRUE)
    rweight<-terra::classify(x=m, rcl=rclmat_weight, include.lowest=TRUE)
    plot(rweight)
    
    
    weight_df<-as.data.frame(values(rweight))
    sums<-as.data.frame(table(weight_df))
    sums$value_of_weights<-as.numeric(levels(sums$layer))*sums$Freq
    class_total<-sum(sums$value_of_weights)
    
    rweight<-rweight/class_total
    plot(rweight)
    
    
    mrec_conc <- c(
      1011,1096, 0)
    rclmat_conc <- matrix(mrec_conc, ncol=3, byrow=TRUE)
    rconc<-terra::classify(x=m, rcl=rclmat_conc, include.lowest=TRUE)
    plot(rconc)
    
    
    rweightedmean<- rweight*rconc #multiply the weight value times the concentrations 
    rweightedmean <- mask(crop(rweightedmean, colony), colony)
    plot(rweightedmean)
    
    weightmean_df<-as.data.frame(values(rweightedmean))
    sumswm<-as.data.frame(table(weightmean_df))
    sumswm$sum_of_conc<-as.numeric(levels(sumswm$layer))*sumswm$Freq
    weightedmean<-sum(sumswm$sum_of_conc)
    weightedmean
    
    
    
    
    
    
    daily_scenario[[day]] <- m
  }
}

daily_scenario_stack<-rast(daily_scenario) #stack it into one file
names(daily_scenario_stack)<-1:365

terra::writeRaster(daily_scenario_stack, paste0(root_data_out, "/all_forage/raster_field_layers/",unique(by_media$id),"/",unique(by_media$Compound),"_",unique(by_media$Media),"_", names(daily_scenario_stack), ".tif"), overwrite=TRUE)
daily_scenario_stack

} 

by_media_list<-lapply(by_media_list,process_by_media)

#} 


for(i in 1:length(on_fieldrasters)){
  st_write(habitat_zone_fields[[i]], paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/",names(habitat_zone_fields)[[i]],"_hab.shp"), driver = "ESRI Shapefile")
}







#this version of the reclassification of the nlcd when calculating the weighted mean resulted in fuckery!
###     # mrec_conc <- c(
#   1011,1096, 0)
# rclmat_conc <- matrix(mrec_conc, ncol=3, byrow=TRUE)
# rconc<-terra::classify(x=m, rcl=rclmat_conc, include.lowest=TRUE)
# plot(rconc) #plot original concentration


#### Plot example ----
# testy<-list_by_media[[2]]
# testx<-testy[[200]]
# m<-testx
# m<-as.data.frame(m,xy = TRUE)
# m$Layer_1<-as.factor(m$layer)
# m<-na.omit(m)
# 
# 
# nlcdpalette<-c("darkgrey","cornflowerblue","bisque","lightsalmon","firebrick","darkred","darkolivegreen3","darkkhaki","lemonchiffon3","khaki1","chocolate4","cadetblue2","cadetblue4")
# 
# namey<-sort(unique(m$Layer_1))
# names(nlcdpalette)<-(namey)
# 
# colScale<- scale_fill_manual(
#   values=nlcdpalette, name="NLCD",
#   labels = c('Pesticide',
#              'Water',
#              'Developed, Open Space ',
#              'Developed, Low Intensity',
#              'Developed, Medium Intensity',
#              "Developed, High Intensity",
#              "Deciduous Forest",
#              "Shrub",
#              "Grassland",
#              "Pasture/Hay",
#              "Cultivated",
#              "Woody Wetlands",
#              "Herbaceous Wetlands"),
#   drop = FALSE)
# 
# 
# 
# m$title<-"Example Exposure Landsca[e"
# habplot<-ggplot()+
#   geom_tile(data=m, aes(x=x,y=y,fill=Layer_1))+
#   colScale+
#   theme_bw() +
#   theme(
#     legend.title.align=0.5,
#     strip.text.x = element_text(size = 14, colour = "black", angle = 0),
#     legend.position = "Right",
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     legend.background = element_rect(fill="lightgrey",
#                                      size=0.5, linetype="solid",
#                                      colour ="black")
#   )
# habplot























#old code for raster math


daily_scenario<-list()
for(day in 1:365){
  
  offdf30<-buf30[buf30$Day ==day,]
  offdf30$Value<-as.numeric(offdf30$Value)
  
  offdf60<-buf60[buf60$Day ==day,]
  offdf60$Value<-as.numeric(offdf60$Value)
  
  offdf90<-buf90[buf90$Day ==day,]
  offdf90$Value<-as.numeric(offdf90$Value)
  
  
  output_off30<- rast(rasterize(x = offdf30, y = roff.raster30, field = "Value")) #spatraster
  output_off60<- rast(rasterize(x = offdf60, y = roff.raster60, field = "Value")) #spatraster
  output_off90<- rast(rasterize(x = offdf90, y = roff.raster90, field = "Value")) #spatraster
  
  
  # plot(output_off30)
  # plot(output_off60)
  # plot(output_off90)
  
  m <-merge(output_off30,habitat) #merge off
  #plot(m)
  m <-merge(output_off60,m) #merge off
  #plot(m)
  m <-merge(output_off90,m) #merge off
  #plot(m)
  
  mrec_weight <- c(1e-60,1011,0.5,
                   1011,1012,0,
                   1012,1024,0.5,
                   1024,1025, 0.1,
                   1025,1032, 0,
                   1032,1042,1,
                   1042,1043,0.5,
                   1043,1044,0.75,
                   1044,1073,1,
                   1073,1083,0.5,
                   1083,1096, 0.25
  )
  
  rclmat_weight <- matrix(mrec_weight, ncol=3, byrow=TRUE)
  rweight<-terra::classify(x=m, rcl=rclmat_weight, include.lowest=TRUE)
  rweight<- mask(crop(rweight, colony), colony)
  #plot(rweight)
  
  weight_df<-as.data.frame(values(rweight))
  sums<-as.data.frame(table(weight_df))
  sums$value_of_weights<-as.numeric(levels(sums$layer))*sums$Freq
  class_total<-sum(sums$value_of_weights)
  
  rweight<-rweight/class_total
  #plot(rweight)
  
  #convert it so that any ncld values are not included in the calculation of concentration
  habarea<-habitat
  habarea[habarea>1]<-0
  
  
  m <-merge(output_off30,habarea) #merge off
  #plot(m)
  m <-merge(output_off60,m) #merge off
  #plot(m)
  m <-merge(output_off90,m) #merge off
  #plot(m)
  
  rconc<- mask(crop(m, colony), colony)
  
  rweightedmean<- rweight*rconc #multiply the weight value times the concentrations 
  #plot(rweightedmean)
  
  weightmean_df<-as.data.frame(values(rweightedmean))
  sumswm<-as.data.frame(table(weightmean_df))
  sumswm$sum_of_conc<-as.numeric(levels(sumswm$layer))*sumswm$Freq
  weightedmean<-sum(sumswm$sum_of_conc)
  weightedmean #ug/g
  
  df<-as.data.frame(cbind(weightedmean,day))
  colnames(df)<-c("Conc","Day")
  df$Media<-paste0(unique(by_media$Media))
  df$Compound<-unique(by_media$Compound)
  
  daily_scenario[[day]] <- df } #end for loop

output<-do.call(rbind,daily_scenario)


