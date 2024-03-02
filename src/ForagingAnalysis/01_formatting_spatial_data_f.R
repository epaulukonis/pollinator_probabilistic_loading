### Foraging Landscape Analysis

### 01 Investigating and setting up spatial data 

# Edited by E. Paulukonis January 2024


#scenario<-paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/only_active_appssampled_fields_1999_apps.shp") original
scenario<-paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014/sampledfields_2014_1.shp")

print("stepping into 01: formatting spatial date")


if(file.exists(scenario)){
  # print(list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/only_active_apps"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  # scenarios<- file.path(paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/only_active_apps"), list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/only_active_apps"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  # scenarios<-setNames(lapply(scenarios, st_read), tools::file_path_sans_ext(basename(scenarios)))
  # scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]
  
  

  print(list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  scenarios<- file.path(paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014"), list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  scenarios<-setNames(lapply(scenarios, st_read), tools::file_path_sans_ext(basename(scenarios)))
  scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]

  scenario_daily_list<-list()
  

  
  for(scene in 1:length(scenarios)){
    sim<-scenarios[[scene]]
  
    sim <- sim %>% 
    st_transform(., crs = 26916) %>% 
    st_make_valid()
    
    #here, we'll pull out the 2012 data sets and extract that layer to a single colony location. then all code that follows will just be the field histories in that spot. 
    colony<- st_read(paste0(bombus_dir,"/foraging/"), layer = "colonylocation")
    colony<- st_transform(colony, crs(sim)) #reproject to match extent of DF
    colony<-st_buffer(colony, 1000)
  
  
  scenarios_f<-st_intersection(sim,colony)
  
  # plot(colony$geometry,col="red")
  #plot(scenarios_f$geometry)
    
    #this function addresses any polygons that are of the same field but were separated due to area calculations in the intersection analysis; 
    combine_by_id_if_needed<-function(x){
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
      finaldf
      
    }
    
    
    scenarios_edited<-combine_by_id_if_needed(scenarios_f)
   # scenarios_edited<-lapply(scenarios,combine_by_id_if_needed) #if all years and fields
  
  
    
    
   #for each individual field, let's group together and organize the land-use history by year
   group_by_compound <-bind_rows(scenarios_edited, .id = 'grp')%>% group_split(Compond) #replace with ID if needed
    
   #create a function to insert years where needed; leave most things blank so if needed, we can back fill
    insert_years_if_needed<-function(x){
      x$Year<-as.numeric(x$Year)
      output<-x %>%
        complete(id, Year = 1999:2021, 
                 fill = list(
                   ApplctT= "None"
                             )) %>%
        as.data.frame()
      output$Compond<-unique(zoo::na.locf(output$Compond)) #this adds the ID as a filled value #replace with ID if needed
      output
    }
    
  
  #x<-group_by_compound[[4]]
    
    
  individual_field_histories  <-lapply(group_by_compound,insert_years_if_needed)
    
  
  #finally, for each individual year and compound combo, divide in 365 days and add as date
  dailylist<-list()
  for(n in 1:length(individual_field_histories)){
   x<-individual_field_histories[[n]]
   
   #pull out only 2014 for now, though you can delete this line of code and get all years if needed
   x<-x[x$Year ==2014,]
   
  #specific condition for imidacloprid/bifenthrin, which has more than one application type by ID
   x<-x %>% 
     group_by(id) %>% 
     filter(if(n() > 1) ApplctT != 'None' else TRUE) %>%
     filter(if(n() > 1) ApplctT != 'Soil' else TRUE) %>%
     ungroup
   
   x<- x%>% group_by(id) %>% slice(rep(1:n(),each=365))
  # test<-x[x$id==5517 & x$Year==1999,]
   
   
   #x<-x[rep(seq_len(nrow(x)), each = 365), ]
   out<-x %>% group_by(id,Year) %>% mutate(day=0:364) %>% mutate(date = as.Date(day, origin=paste0(Year,'-01-01'))) %>% mutate(day=1:365)
   dailylist[[n]]<-out
  }
  
  print(paste0("Formatting of simulation ", scene, " done"))
  
  scenario_daily_list[[scene]]<-dailylist
  
  
  } #end for_loop by scenario




}else{
  
  ### Read in simulated scenarios; we're going to focus on 2021 for this chapter. 
  print(list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  scenarios<- file.path(paste0(root_data_out, "/all_bombus/modified_sampled_fields"), list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  scenarios<-setNames(lapply(scenarios, st_read), tools::file_path_sans_ext(basename(scenarios)))
  scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]
  
  #change column names to fix 'emerged date' as application date
  colnamesog <- colnames(scenarios[[1]])
  colnamesnew<-c("Compound","Commodity","Year","ApplicationType", "crop","area",  "id", "planteddates","applicationday", "AvgRt_k","geometry")
  scenarios<- lapply(scenarios, setNames, colnamesnew)
  
  
  ### the first thing we want to do is extract the fields that are within the RPBB habitat zones. So we'll need to read those in first. 
  bomb_h <- st_read(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
  bomb_h<- st_transform(bomb_h, crs(scenarios[[1]])) #reproject to match extent of DF
  
  
  #Let's clip the field scenario data within the bombus affinis habitat
  habitat_zone_fields<-lapply(scenarios, function(x) st_intersection(x,bomb_h) )
  rm(scenarios)
  
  #write to new folder
  for(i in 1:length(habitat_zone_fields)){
    st_write(habitat_zone_fields[[i]], paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/",names(habitat_zone_fields)[[i]],"_hab.shp"), driver = "ESRI Shapefile")
  }



}




