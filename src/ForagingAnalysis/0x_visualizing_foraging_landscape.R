### Foraging Landscape

### 0X setting up gif of landscape

sim<-1
final_on_field_history<-final_on_field_history_list[[sim]]
final_off_field_history_30m<-final_off_field_history_30m_list[[sim]]
final_off_field_history_60m<-final_off_field_history_60m_list[[sim]]
final_off_field_history_90m<-final_off_field_history_90m_list[[sim]]

#this removes any scenarios for which we don't have data, thus it throws off the function
final_on_field_history      <-keep(final_on_field_history, ~all(ncol(.x) >= 14))
final_off_field_history_30m <-keep(final_off_field_history_30m, ~all(ncol(.x) >= 14))
final_off_field_history_60m      <-keep(final_off_field_history_60m, ~all(ncol(.x) >= 14))
final_off_field_history_90m      <-keep(final_off_field_history_90m, ~all(ncol(.x) >= 14))

scenario_clip_on<-final_on_field_history[[2]]
scenario_clip_off30<-final_off_field_history_30m[[2]]
scenario_clip_off60<-final_off_field_history_60m[[2]]
scenario_clip_off90<-final_off_field_history_90m[[2]]



#rasterize_by_day<-function(a,x,y,z){
  
  
  scenario_clip_on$loc<-"On"
  scenario_clip_off30$loc<-"Off30"
  scenario_clip_off60$loc<-"Off60"
  scenario_clip_off90$loc<-"Off90"
  
  scenario_clip_on<-scenario_clip_on[,c(1:13,ncol(scenario_clip_on),14:(ncol(scenario_clip_on)-1))]
  scenario_clip_off30<-scenario_clip_off30[,c(1:13,ncol(scenario_clip_off30),14:(ncol(scenario_clip_off30)-1))]
  scenario_clip_off60<-scenario_clip_off60[,c(1:13,ncol(scenario_clip_off60),14:(ncol(scenario_clip_off60)-1))]
  scenario_clip_off90<-scenario_clip_off90[,c(1:13,ncol(scenario_clip_off90),14:(ncol(scenario_clip_off90)-1))]
  
  print(paste0("this is simulation ", sim))
  print(paste0("this is compound ",unique(scenario_clip_on$Compound) ))
  
  
  #remove any rows that are filler from the multi-year run
  # scenario_clip_on<-scenario_clip_on %>% group_by(Compound) %>%
  #   filter(!(is.na(Soil) & sum(is.na(Soil)) > 300)) %>%
  #   ungroup
  
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
  
  
  print(paste0("this is compound ",unique(scenario_clip_on$Compound) ))
  
  # head(scenario_clip_off)
  # head(scenario_clip_on)
  
  
  scenario_clip<-rbind(scenario_clip_on,scenario_clip_off30,scenario_clip_off60,scenario_clip_off90)
  
  
  #LIST FOR MEDIA
  by_media_list<-split(scenario_clip,list(scenario_clip$Media))
  
  #function by each media 
  process_by_media<-function(media){
    #media<-by_media_list[[3]]
    by_media<-media
    
    print(paste0("this media is ",unique(by_media$Media) ))
    
    #this function will allow us to erase each subsequent polygon from its matched buffer at the appropriate size (i.e., distance from field)
    erase_poly<-function(x,y){
      buffer <-x
      og <-y
      output <- ms_erase(as_Spatial(buffer),as_Spatial(og)) #subtract original polygon
      output
    }
    
    if (any(by_media$loc == "On") ){ #if there are on-field values...
      onfield<-by_media[by_media$loc == "On",]
      offfield30<-by_media[by_media$loc == "Off30",]
      offfield60<-by_media[by_media$loc == "Off60",]
      offfield90<-by_media[by_media$loc == "Off90",]
      
      #set up each on scenario as an sf, and set raster conditions based on that sf, on-field
      ondf<-st_as_sf(onfield)
      # plot(ondf$geometry,col="grey")
      ron.raster <- raster()
      extent(ron.raster) <- extent(ondf)
      crs(ron.raster)<-crs(ondf)
      res(ron.raster) <- 30
      
      #set up each off scenario as an sf, and set raster conditions based on that sf, 30m
      offdf30<-st_as_sf(offfield30)
      buffer30<-st_buffer(offdf30,30)
      #split
      buffer30<-split(buffer30,list(buffer30$id))
      offdf30<-split(offdf30,list(offdf30$id))
      buf30<-mapply(erase_poly,buffer30,offdf30) #subtract from OG field
      buf30<- st_as_sf(do.call(rbind,buf30))
      #plot
      #plot(buf30$geometry,add=T,col="red")
      roff.raster30 <- raster()
      extent(roff.raster30) <- extent(buf30)
      crs(roff.raster30)<-crs(buf30)
      res(roff.raster30) <- 30
      
      #set up each off scenario as an sf, and set raster conditions based on that sf, 60m
      offdf60<-st_as_sf(offfield60)
      buffer60<-st_buffer(offdf60,60)
      #split
      buffer60<-split(buffer60,list(buffer60$id))
      buf60<-mapply(erase_poly,buffer60,buffer30) #subtract 30m from field
      buf60<- st_as_sf(do.call(rbind,buf60))
      #plot
      #plot(buf60$geometry, add=T, col="purple")
      roff.raster60 <- raster()
      extent(roff.raster60) <- extent(buf60)
      crs(roff.raster60)<-crs(buf60)
      res(roff.raster60) <- 30
      
      #set up each off scenario as an sf, and set raster conditions based on that sf, 90m
      offdf90<-st_as_sf(offfield90)
      buffer90<-st_buffer(offdf90,90)
      #split
      buffer90<-split(buffer90,list(buffer90$id))
      buf90<-mapply(erase_poly,buffer90,buffer60) #subtract 60m from field
      buf90<- st_as_sf(do.call(rbind,buf90))
      #plot
      #plot(buf90$geometry, add=T, col="green")
      roff.raster90 <- raster()
      extent(roff.raster90) <- extent(buf90)
      crs(roff.raster90)<-crs(buf90)
      res(roff.raster90) <- 30
      
      # rasterize by day and stack, then read to an additional list
      daily_scenario<-list()
      for(day in 1:365){
        #day<-195
        dateon<-ondf[ondf$Day ==day,]
        
        offdf30<-buf30[buf30$Day ==day,]
        offdf30$Value<-as.numeric(offdf30$Value)
        
        offdf60<-buf60[buf60$Day ==day,]
        offdf60$Value<-as.numeric(offdf60$Value)
        
        offdf90<-buf90[buf90$Day ==day,]
        offdf90$Value<-as.numeric(offdf90$Value)
        
        
        output_on<- rast(rasterize(x = dateon, y = ron.raster, field = "Value")) #spatraster
        output_off30<- rast(rasterize(x = offdf30, y = roff.raster30, field = "Value")) #spatraster
        output_off60<- rast(rasterize(x = offdf60, y = roff.raster60, field = "Value")) #spatraster
        output_off90<- rast(rasterize(x = offdf90, y = roff.raster90, field = "Value")) #spatraster
        
        # plot(output_on)
        # plot(output_off30)
        # plot(output_off60)
        # plot(output_off90)
        
        m <-merge(output_on,habitat) #merge on
        #plot(m)
        m <-merge(output_off30,m) #merge off
        #plot(m)
        m <-merge(output_off60,m) #merge off
        #plot(m)
        m <-merge(output_off90,m) #merge off
        #plot(m)
        
        m<- mask(crop(m, colony), colony)
        
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
        #plot(rweight)
        
        weight_df<-as.data.frame(values(rweight))
        sums<-as.data.frame(table(weight_df))
        sums$value_of_weights<-as.numeric(levels(sums$layer))*sums$Freq
        class_total<-sum(sums$value_of_weights)
        
        #quick code to make sure the 
        sums$test<-sums$value_of_weights/class_total
        sum(sums$test)
        
        rweight<-rweight/class_total
        #plot(rweight) #plot class-weighted habitat values
        
        
        mrec_conc <- c(
          1011,1096, 0)
        rclmat_conc <- matrix(mrec_conc, ncol=3, byrow=TRUE)
        rconc<-terra::classify(x=m, rcl=rclmat_conc, include.lowest=TRUE)
        #plot(rconc) #plot original concentration
        
        
        rweightedmean<- rweight*rconc #multiply the weight value times the concentrations 
        #plot(rweightedmean) #plot the area weighted concentration values 
        
        weightmean_df<-as.data.frame(values(rweightedmean))
        sumswm<-as.data.frame(table(weightmean_df))
        sumswm$sum_of_conc<-as.numeric(levels(sumswm$layer))*sumswm$Freq
        weightedmean<-sum(sumswm$sum_of_conc)
        #weightedmean #ug/g
        
        df<-as.data.frame(cbind(weightedmean,day))
        colnames(df)<-c("Conc","Day")
        df$Media<-paste0(unique(by_media$Media))
        df$Compound<-unique(by_media$Compound)
        
        daily_scenario[[day]] <- df
        
      }
      
      output<-do.call(rbind,daily_scenario)
      
    }else{ #if there are not on-field values... (i.e., nectar only off-field)
      offfield30<-by_media[by_media$loc == "Off30",]
      offfield60<-by_media[by_media$loc == "Off60",]
      offfield90<-by_media[by_media$loc == "Off90",]
      
      
      #set up each off scenario as an sf, and set raster conditions based on that sf, 30m
      offdf30<-st_as_sf(offfield30)
      buffer30<-st_buffer(offdf30,30)
      #split
      buffer30<-split(buffer30,list(buffer30$id))
      offdf30<-split(offdf30,list(offdf30$id))
      buf30<-mapply(erase_poly,buffer30,offdf30) #subtract from OG field
      buf30<- st_as_sf(do.call(rbind,buf30))
      #plot
      # plot(buf30$geometry,add=T,col="red")
      roff.raster30 <- raster()
      extent(roff.raster30) <- extent(buf30)
      crs(roff.raster30)<-crs(buf30)
      res(roff.raster30) <- 30
      
      #set up each off scenario as an sf, and set raster conditions based on that sf, 60m
      offdf60<-st_as_sf(offfield60)
      buffer60<-st_buffer(offdf60,60)
      #split
      buffer60<-split(buffer60,list(buffer60$id))
      buf60<-mapply(erase_poly,buffer60,buffer30) #subtract 30m from field
      buf60<- st_as_sf(do.call(rbind,buf60))
      #plot
      #plot(buf60$geometry, add=T, col="purple") 
      roff.raster60 <- raster()
      extent(roff.raster60) <- extent(buf60)
      crs(roff.raster60)<-crs(buf60)
      res(roff.raster60) <- 30
      
      #set up each off scenario as an sf, and set raster conditions based on that sf, 90m
      offdf90<-st_as_sf(offfield90)
      buffer90<-st_buffer(offdf90,90)
      #split
      buffer90<-split(buffer90,list(buffer90$id)) 
      buf90<-mapply(erase_poly,buffer90,buffer60) #subtract 60m from field
      buf90<- st_as_sf(do.call(rbind,buf90))
      #plot
      #plot(buf90$geometry, add=T, col="green")
      roff.raster90 <- raster()
      extent(roff.raster90) <- extent(buf90)
      crs(roff.raster90)<-crs(buf90)
      res(roff.raster90) <- 30
      
      # rasterize by day and stack, then read to an additional list
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
        
        mrec_conc <- c(
          1011,1096, 0)
        rclmat_conc <- matrix(mrec_conc, ncol=3, byrow=TRUE)
        rconc<-terra::classify(x=m, rcl=rclmat_conc, include.lowest=TRUE)
        rconc<- mask(crop(rconc, colony), colony)
        #plot(rconc)
        
        
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
      
    }#end ifelse
    
    output
  } #end process_by_media function
  
  by_media_list_fin<-lapply(by_media_list,process_by_media) #apply over all media
  
  by_media_and_compound <-do.call(rbind,by_media_list_fin)
  by_media_and_compound
  
#} #end rasterize_by_day
