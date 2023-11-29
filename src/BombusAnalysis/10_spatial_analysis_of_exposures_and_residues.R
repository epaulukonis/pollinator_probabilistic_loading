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

#x<-individual_fields_by_group[[1]]

#here, we create function to treat each of these individual dataframes as its own unique set of residue information
prepare_data_frames<-function(x){
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
 modeled_onsite_outputs_by_field[[field]]<-on_field_scenario
  
}

#this is the scenario_specific output for on-site models
names(modeled_onsite_outputs_by_field)<-names(individual_fields_by_group_onfield)








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
  modeled_offsite_outputs_by_field[[field]]<-off_field_scenario
  
}

#this is the scenario_specific output for on-site models
names(modeled_offsite_outputs_by_field)<-names(individual_fields_by_group_offfield)



