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


on_field_area<-on_field_area[,c(1:10,18,19)]
off_field_area<-off_field_area[,c(1:10,18,19)]

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



x<-individual_fields_by_group_onfield[[155]]

##### ONSITE OUTPUTS BY FIELD
#alter the gather function slightly
gather_data<-function(x){
  df<-x
  df<-gather(df, "MediaSub", "Value", 4:ncol(df))
  df
  
}
modeled_onsite_outputs_by_field<-list()
for(field in 1:length(individual_fields_by_group_onfield)){
  field<-155
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




# examine<-modeled_onsite_outputs_by_field[sapply(modeled_onsite_outputs_by_field, nrow) == 755]
# testy<-examine[[1]]
# 
# 
# mylist_sub <- modeled_onsite_outputs_by_field[grep('1498', names(modeled_onsite_outputs_by_field))]

##### OFFSITE OUTPUTS BY FIELD
#alter the gather function slightly
gather_data<-function(x){
  df<-x
  df<-gather(df, "MediaSub", "Value", 5:ncol(df))
  df
  
}

modeled_offsite_outputs_by_field<-list()
for(field in 1:length(individual_fields_by_group_offfield)){
  
  scenario_df<-individual_fields_by_group_offfield[[field]]
  
  #get name of modeled data that we need to match to the field scenario
  pulldf<-names(off_field_residue_list)[amatch(names(individual_fields_by_group_offfield[field]), names(off_field_residue_list), maxDist = Inf)]
  
  #match on_field_residue list of modeled outputs
  model_df<-off_field_residue_list[names(off_field_residue_list) %in% pulldf]
  model_df<- map_df(model_df, ~as.data.frame((.)))
  
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


testy<-modeled_onsite_outputs_by_field[[155]]
x<-testy

get_rolling_average<-function(x){
# 
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
 x<-x %>% 
   group_by(MediaSub) %>% 
   slice(1:7) 
 
 y<- x %>% 
    group_by(MediaSub) %>%
    mutate(movingavg = zoo::rollmean(x=Value,7))
 y<-y[y$day==6,]
 
 ##sliding window of 7 days then plot one or two compounds
 
y<-rbind(depo,y)
# why_no_work[[field]]<-y
# 
# 
#   }
}


moving_avg_list<-lapply(modeled_onsite_outputs_by_field,get_rolling_average)

on_field_moving_averages <- do.call("rbind", moving_avg_list)

cloth<-on_field_moving_averages[on_field_moving_averages$Compound == "CLOTHIANIDAN",]
plot(cloth$Value)
