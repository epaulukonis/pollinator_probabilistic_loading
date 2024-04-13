### Foraging Landscape

### 04 rasterize the field scenario outputs and get weighted mean based on foraging quality

print("stepping into 04: processing and rasterizing data")


# Edited by E. Paulukonis Feb 2024

#### Rasterize the datasets ----
#### Get NLCD and mask to colony
nlcd<-rast(paste0(root_data_in, "/MapData/NLCD/Illinois/nlcd2013_f.tiff"))
# plot(nlcd)
# nlcd<-project(nlcd,crs(colony))
# rr <- rast(crs=crs(nlcd), resolution=30, xmin=73730.46, ymin=4068662, xmax=521811.7, ymax=4749575)
# nlcd<-resample(nlcd,rr,method="ngb")
# writeRaster(nlcd,paste0(root_data_in, "/MapData/NLCD/Illinois/nlcd2013_f.tiff"), filetype = "GTiff", overwrite = TRUE)

habitat <- mask(crop(nlcd, colony), colony) #get nlcd within habitat
#plot(habitat)

values(habitat)<-values(habitat)+1000 #add high values to CDL hab classes to differentiate between actual concentrations


# plot(habitat)
# plot(scenario_clip$geometry,add=T)

#scores<-read.csv(paste0(bombus_dir, "/foraging/ForagingScores.csv"))
#scores$Value<-scores$Value+1000



#### Process BOTH on and off-field model outputs and rasterize them into a stack organized by, in layers, 1.Day, 2.Medium, 3.Field ---- 

#### BETA TESTING CODE
# sim<-9
# final_on_field_history<-final_on_field_history_list[[sim]]
# final_off_field_history_30m<-final_off_field_history_30m_list[[sim]]
# final_off_field_history_60m<-final_off_field_history_60m_list[[sim]]
# final_off_field_history_90m<-final_off_field_history_90m_list[[sim]]
# 
# #this removes any scenarios for which we don't have data, thus it throws off the function
# final_on_field_history      <-keep(final_on_field_history, ~all(ncol(.x) >= 14))
# final_off_field_history_30m <-keep(final_off_field_history_30m, ~all(ncol(.x) >= 14))
# final_off_field_history_60m      <-keep(final_off_field_history_60m, ~all(ncol(.x) >= 14))
# final_off_field_history_90m      <-keep(final_off_field_history_90m, ~all(ncol(.x) >= 14))
# 
# 
# # #we're not going to include any soil applications; hence we remove them. in the future this will likely need to be modified
# final_on_field_history<-purrr::discard(final_on_field_history, ~any(.x$ApplicationType == "Soil"))
# final_off_field_history_30m <-purrr::discard(final_off_field_history_30m , ~any(.x$ApplicationType == "Soil"))
# final_off_field_history_60m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))
# final_off_field_history_90m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))
# 
# #we also don't haev DR data for this compound
# final_on_field_history<-purrr::discard(final_on_field_history, ~any(.x$Compound == "GLYPHOSATE"))
# final_off_field_history_30m <-purrr::discard(final_off_field_history_30m , ~any(.x$Compound == "GLYPHOSATE"))
# final_off_field_history_60m <-purrr::discard(final_off_field_history_90m , ~any(.x$Compound == "GLYPHOSATE"))
# final_off_field_history_90m <-purrr::discard(final_off_field_history_90m , ~any(.x$Compound == "GLYPHOSATE"))
# 
# #pull out clip
# scenario_clip_on<-final_on_field_history[[4]]
# scenario_clip_off30<-final_off_field_history_30m[[4]]
# scenario_clip_off60<-final_off_field_history_60m[[4]]
# scenario_clip_off90<-final_off_field_history_90m[[4]]
#
# # unique(scenario_clip_on$Compound)




#This function processes the list for each scenario and splits by media
rasterize_by_day<-function(a,x,y,z){

  
scenario_clip_on<-a
scenario_clip_off30<-x
scenario_clip_off60<-y
scenario_clip_off90<-z

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
          buffer <-st_union(x)
          og <-st_union(y)
          output <- ms_erase(as_Spatial(buffer),as_Spatial(og)) #subtract original polygon
          getbuf<-st_as_sfc(output) # set the sf geometry as the new buffer
          outputf <- st_set_geometry(y, rep(getbuf, length=nrow(y)))# set geometry in old df, return sf
          outputf
        }

        if (any(by_media$loc == "On") ){ #if there are on-field values...
                    onfield<-by_media[by_media$loc == "On",]
                    offfield30<-by_media[by_media$loc == "Off30",]
                    offfield60<-by_media[by_media$loc == "Off60",]
                    offfield90<-by_media[by_media$loc == "Off90",]
                  
                    #set up each on scenario as an sf, and set raster conditions based on that sf, on-field
                    ondf<-st_as_sf(onfield)
                    #plot(ondf$geometry,col="grey")
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
                    buf30<-mapply(erase_poly,buffer30,offdf30,SIMPLIFY=FALSE) #subtract from OG field
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
                    buf60<-mapply(erase_poly,buffer60,buffer30,SIMPLIFY=FALSE) #subtract 30m from field
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
                    buf90<-mapply(erase_poly,buffer90,buffer60,SIMPLIFY=FALSE) #subtract 60m from field
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
                 
                    #### Habitat and on/off-field weights
                      #create weight matrices for on-field; off-field will be multiplied by the habitat class it is associated with
                      onfield_weight<- c(1e-60,150,0.005) #value reflects the proportion of species that were social bees found in corn/soybeans 
                      onfield_weight <- matrix(onfield_weight, ncol=3, byrow=TRUE)
                      onweight<-terra::classify(x=output_on, rcl=onfield_weight, include.lowest=TRUE)
                      #plot(onweight)
 
                      #reweight all other habitat types
                      mrec_weight <- c(
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
                      habweight<-terra::classify(x=habitat, rcl=rclmat_weight, include.lowest=TRUE)
                     # plot(habweight)

                      rweight<-merge(onweight,habweight)
                     # plot(rweight)

                      
                      #here, we're calculating the weighted mean: here are the steps
                      weight_df<-as.data.frame(values(rweight)) #first, convert the raster into a dataframe so we can get the sum of the weights
                      sums<-as.data.frame(table(weight_df)) #the frequency of each time of classification
                      sums$value_of_weights<-as.numeric(levels(sums$layer))*sums$Freq #get the individual values associated with the frequency
                      class_total<-sum(sums$value_of_weights) #this represents the total sum of the weights!
                      
                      #quick code to make sure the total is 1
                      sums$test<-sums$value_of_weights/class_total
                      sum(sums$test)
                      
                      #this gives us a raster that accurately reflects the individual raster weights associated with that habitat class
                      rweight<-rweight/class_total
                      rweight<- mask(crop(rweight, colony), colony)
                      #plot(rweight) #plot class-weighted habitat values
                      
                    #### Raw Concentrations
                      #convert it so that any ncld values are not included in the calculation of concentration
                      habarea<-habitat
                      habarea[habarea>1]<-0

                      m <-merge(output_on,habarea) #merge on
                      #plot(m)
                      m <-merge(output_off30,m) #merge off
                      #plot(m)
                      m <-merge(output_off60,m) #merge off
                      #plot(m)
                      m <-merge(output_off90,m) #merge off
        
                      rconc<- mask(crop(m, colony), colony)
                      rconc<-resample(rconc,rweight) #match extents
   
                      
                      rweightedmean<- rweight*rconc #multiply the weight value times the concentrations 
                      #plot(rweightedmean) #plot the area weighted concentration values 
    
                      weightmean_df<-as.data.frame(values(rweightedmean))
                      sumswm<-as.data.frame(table(weightmean_df))
                      sumswm$sum_of_conc<-as.numeric(levels(sumswm$layer))*sumswm$Freq #get the total sum by weight
                      
                      weightedmean<-sum(sumswm$sum_of_conc) #this reflects the total weighted mean exposure dose once habitat preferences are factored in. 
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
              buf30<-mapply(erase_poly,buffer30,offdf30,SIMPLIFY=FALSE) #subtract from OG field
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
              buf60<-mapply(erase_poly,buffer60,buffer30,SIMPLIFY=FALSE) #subtract 30m from field
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
              buf90<-mapply(erase_poly,buffer90,buffer60,SIMPLIFY=FALSE) #subtract 60m from field
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
               
             }#end ifelse
                  
        output
              } #end process_by_media function
      
      by_media_list_fin<-lapply(by_media_list,process_by_media) #apply over all media

      by_media_and_compound <-do.call(rbind,by_media_list_fin)
      by_media_and_compound

} #end rasterize_by_day


for(sim in 1:length(final_on_field_history_list)){

  final_on_field_history<-final_on_field_history_list[[sim]]
  final_off_field_history_30m<-final_off_field_history_30m_list[[sim]]
  final_off_field_history_60m<-final_off_field_history_60m_list[[sim]]
  final_off_field_history_90m<-final_off_field_history_90m_list[[sim]]
  
  #this removes any scenarios for which we don't have data, thus it throws off the function
  final_on_field_history      <-keep(final_on_field_history, ~all(ncol(.x) >= 14))
  final_off_field_history_30m <-keep(final_off_field_history_30m, ~all(ncol(.x) >= 14))
  final_off_field_history_60m      <-keep(final_off_field_history_60m, ~all(ncol(.x) >= 14))
  final_off_field_history_90m      <-keep(final_off_field_history_90m, ~all(ncol(.x) >= 14))
  
  final_on_field_history<-purrr::discard(final_on_field_history, ~any(.x$ApplicationType == "Soil"))
  final_off_field_history_30m <-purrr::discard(final_off_field_history_30m , ~any(.x$ApplicationType == "Soil"))
  final_off_field_history_60m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))
  final_off_field_history_90m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))
  
  final_on_field_history<-purrr::discard(final_on_field_history, ~any(.x$Compound == "GLYPHOSATE"))
  final_off_field_history_30m <-purrr::discard(final_off_field_history_30m , ~any(.x$Compound == "GLYPHOSATE"))
  final_off_field_history_60m <-purrr::discard(final_off_field_history_90m , ~any(.x$Compound == "GLYPHOSATE"))
  final_off_field_history_90m <-purrr::discard(final_off_field_history_90m , ~any(.x$Compound == "GLYPHOSATE"))
  
    #takes approximately 30 minutes to run
    system.time(dailymediasets_by_compound<-mapply(rasterize_by_day, final_on_field_history, final_off_field_history_30m,final_off_field_history_60m,final_off_field_history_90m, SIMPLIFY = FALSE))
    
    # dailymediasets_by_compound_fin<-list()
    # for(n in 1:ncol(dailymediasets_by_compound)){
    #   dailymediasets_by_compound_fin[[n]]<-as.data.frame(do.call(cbind,dailymediasets_by_compound[,n]))
    #   
    # }
    
    dailymediasets_by_compound_fin<-dailymediasets_by_compound
    
    compounds<-unlist(lapply(dailymediasets_by_compound_fin,function(x) unique(x$Compound)))
    names(dailymediasets_by_compound_fin)<-compounds


      for(n in 1:length(dailymediasets_by_compound_fin)){
        write.csv(dailymediasets_by_compound_fin[[n]], paste0(root_data_out,'/all_forage/fixed_media_tables/adjusted/',names(dailymediasets_by_compound_fin[n]),sim, '.csv')  , row.names=F)
        print(paste0("simulation ", names(dailymediasets_by_compound_fin[n])," ", sim," is done"))
        
      }

}




