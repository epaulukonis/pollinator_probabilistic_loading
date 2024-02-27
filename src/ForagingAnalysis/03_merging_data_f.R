### Foraging Landscape

### 03 merging spatial and model data

# Edited by E. Paulukonis Feb 2024

#for a single field:
#ex<-dailylist[[4]]
# ex<-ex[ex$Year == 2012,]


#### this will loop through and merge the on and off field outputs to the model scenarios

#### On-field ----

final_output_by_field_history_func_ON<-function(x){

ex<-x
ex<-ex[ex$Year == 2012,]
colnames(ex)[4]<-"Compound"
colnames(ex)[5]<-"Commodity"
colnames(ex)[6]<-"ApplicationType"


list_by_id<-split(ex,list(ex$id))

#### This function matches the model compound, app type, and commodity with each individual scenario by compound and year per field. The function 
#test_list<-list(list_by_compound_and_year[[1]], list_by_compound_and_year[["CLOTHIANIDIN.2012"]])



join_media_data_by_application_date<- function(chem){
# x<-list_by_id[[1]]
# chem<-x

if(is.na(chem$applctn)[1] == T){

chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)
chem   
  
}else{
  
#chem<-list_by_compound[[1]]

  
#change the foliar to general media
chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)

matching<- on_model_data[on_model_data$Compound %in% chem$Compound & on_model_data$ApplicationType %in% chem$ApplicationType & on_model_data$Commodity %in% chem$Commodity,] #match the compound, application type, and commodity to the scenario
matching<- subset(matching, select = -c(Media)) #drop extra 'media' column 
matching<- spread(matching,MediaSub,Value) #spread out by day so that the 151 days post app are organized by media 

chem$daym<-ifelse(chem$applctn == chem$date,0,chem$day) #where the application day matches the date, add as 0
chem$daym<-chem$day - chem$day[chem$daym == 0 ] #this will allow us to subtract the specific day index value to match the model days
chem$daym<-ifelse(chem$daym > 150 | chem$daym < 0, NA,chem$daym ) #convert all other values back to NA

matching$daym<-matching$day #add column to match join
df_merge<-left_join(chem,matching,by=c("daym","Compound","ApplicationType","Commodity")) #join by daym, compound, application type, and commodity

df_merge<- subset(df_merge, select = -c(daym,day.y)) #drop extra day columns
names(df_merge)[names(df_merge) == 'day.x'] <- 'Day' #rename to day

df_merge
}


}

example_dataframe<-lapply(list_by_id,join_media_data_by_application_date)
example_grouped <-do.call(rbind,example_dataframe)
example_grouped
#final_output_by_field_history<-example_grouped[with(example_grouped, order(Compound,Year,day)), ]

}

start.time<-Sys.time()
final_on_field_history<-lapply(dailylist, final_output_by_field_history_func_ON)
end.time<-Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


#### Off-field 30m ----
final_output_by_field_history_func_OFF<-function(x){
  
  ex<-x
  ex<-ex[ex$Year == 2012,]
  colnames(ex)[4]<-"Compound"
  colnames(ex)[5]<-"Commodity"
  colnames(ex)[6]<-"ApplicationType"
  
  
  list_by_id<-split(ex,list(ex$id))
  

  
  join_media_data_by_application_date<- function(chem){
    #chem<-list_by_compound[[1]]
  
    
    if(is.na(chem$applctn)[1] == T){
      
      chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)
      chem   
      
    }else{
      
      #change the foliar to general media
      chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)
      
      #test<-off_model_data_30m[off_model_data_30m$Compound == "CARBARYL",]
      
      matching<- off_model_data_30m[off_model_data_30m$Compound %in% chem$Compound & off_model_data_30m$ApplicationType %in% chem$ApplicationType & off_model_data_30m$Commodity %in% chem$Commodity,] #match the compound, application type, and commodity to the scenario
      colnames(matching)[6]<-"Value"
      #matching<- subset(matching, select = -c(Media)) #drop extra 'media' column 
      matching<- spread(matching,MediaSub,Value) #spread out by day so that the 151 days post app are organized by media 
      
      chem$daym<-ifelse(chem$applctn == chem$date,0,chem$day) #where the application day matches the date, add as 0
      chem$daym<-chem$day - chem$day[chem$daym == 0 ] #this will allow us to subtract the specific day index value to match the model days
      chem$daym<-ifelse(chem$daym > 150 | chem$daym < 0, NA,chem$daym ) #convert all other values back to NA
      
      matching$daym<-matching$day #add column to match join
      df_merge<-left_join(chem,matching,by=c("daym","Compound","ApplicationType","Commodity")) #join by daym, compound, application type, and commodity
      
      df_merge<- subset(df_merge, select = -c(daym,day.y)) #drop extra day columns
      names(df_merge)[names(df_merge) == 'day.x'] <- 'Day' #rename to day
      
      df_merge
    }
    
    
  }
  
  example_dataframe<-lapply(list_by_id,join_media_data_by_application_date)
  example_grouped <-do.call(rbind,example_dataframe)
  example_grouped
  
  #final_output_by_field_history<-example_grouped[with(example_grouped, order(Compound,Year,day)), ]
  
}

start.time<-Sys.time()
final_off_field_history_30m<-lapply(dailylist, final_output_by_field_history_func_OFF)
end.time<-Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken



#### Off-field 60m ----
final_output_by_field_history_func_OFF<-function(x){
  
  ex<-x
  ex<-ex[ex$Year == 2012,]
  colnames(ex)[4]<-"Compound"
  colnames(ex)[5]<-"Commodity"
  colnames(ex)[6]<-"ApplicationType"
  
  
  list_by_id<-split(ex,list(ex$Compound))
  #list_by_compound<-split(ex,list(ex$Compound))
  
  
  join_media_data_by_application_date<- function(chem){
    #chem<-list_by_compound[[1]]
    
    
    if(is.na(chem$applctn)[1] == T){
      
      chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)
      chem   
      
    }else{
      
      #change the foliar to general media
      chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)
      
      #test<-off_model_data_30m[off_model_data_30m$Compound == "CARBARYL",]
      
      matching<- off_model_data_60m[off_model_data_60m$Compound %in% chem$Compound & off_model_data_60m$ApplicationType %in% chem$ApplicationType & off_model_data_60m$Commodity %in% chem$Commodity,] #match the compound, application type, and commodity to the scenario
      colnames(matching)[6]<-"Value"
      #matching<- subset(matching, select = -c(Media)) #drop extra 'media' column 
      matching<- spread(matching,MediaSub,Value) #spread out by day so that the 151 days post app are organized by media 
      
      chem$daym<-ifelse(chem$applctn == chem$date,0,chem$day) #where the application day matches the date, add as 0
      chem$daym<-chem$day - chem$day[chem$daym == 0 ] #this will allow us to subtract the specific day index value to match the model days
      chem$daym<-ifelse(chem$daym > 150 | chem$daym < 0, NA,chem$daym ) #convert all other values back to NA
      
      matching$daym<-matching$day #add column to match join
      df_merge<-left_join(chem,matching,by=c("daym","Compound","ApplicationType","Commodity")) #join by daym, compound, application type, and commodity
      
      df_merge<- subset(df_merge, select = -c(daym,day.y)) #drop extra day columns
      names(df_merge)[names(df_merge) == 'day.x'] <- 'Day' #rename to day
      
      df_merge
    }
    
    
  }
  
  example_dataframe<-lapply(list_by_id,join_media_data_by_application_date)
  example_grouped <-do.call(rbind,example_dataframe)
  example_grouped
  
  #final_output_by_field_history<-example_grouped[with(example_grouped, order(Compound,Year,day)), ]
  
}

start.time<-Sys.time()
final_off_field_history_60m<-lapply(dailylist, final_output_by_field_history_func_OFF)
end.time<-Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

#### Off-field 90m ----
final_output_by_field_history_func_OFF<-function(x){
  
  ex<-x
  ex<-ex[ex$Year == 2012,]
  colnames(ex)[4]<-"Compound"
  colnames(ex)[5]<-"Commodity"
  colnames(ex)[6]<-"ApplicationType"
  
  
  list_by_id<-split(ex,list(ex$id))
  
  
  
  join_media_data_by_application_date<- function(chem){
    #chem<-list_by_compound[[1]]
    
    
    if(is.na(chem$applctn)[1] == T){
      
      chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)
      chem   
      
    }else{
      
      #change the foliar to general media
      chem$ApplicationType<-ifelse(chem$ApplicationType == "FoliarI" | chem$ApplicationType == "FoliarH", "Foliar",chem$ApplicationType)
      
      #test<-off_model_data_30m[off_model_data_30m$Compound == "CARBARYL",]
      
      matching<- off_model_data_90m[off_model_data_90m$Compound %in% chem$Compound & off_model_data_90m$ApplicationType %in% chem$ApplicationType & off_model_data_90m$Commodity %in% chem$Commodity,] #match the compound, application type, and commodity to the scenario
      colnames(matching)[6]<-"Value"
      #matching<- subset(matching, select = -c(Media)) #drop extra 'media' column 
      matching<- spread(matching,MediaSub,Value) #spread out by day so that the 151 days post app are organized by media 
      
      chem$daym<-ifelse(chem$applctn == chem$date,0,chem$day) #where the application day matches the date, add as 0
      chem$daym<-chem$day - chem$day[chem$daym == 0 ] #this will allow us to subtract the specific day index value to match the model days
      chem$daym<-ifelse(chem$daym > 150 | chem$daym < 0, NA,chem$daym ) #convert all other values back to NA
      
      matching$daym<-matching$day #add column to match join
      df_merge<-left_join(chem,matching,by=c("daym","Compound","ApplicationType","Commodity")) #join by daym, compound, application type, and commodity
      
      df_merge<- subset(df_merge, select = -c(daym,day.y)) #drop extra day columns
      names(df_merge)[names(df_merge) == 'day.x'] <- 'Day' #rename to day
      
      df_merge
    }
    
    
  }
  
  example_dataframe<-lapply(list_by_id,join_media_data_by_application_date)
  example_grouped <-do.call(rbind,example_dataframe)
  example_grouped
  
  #final_output_by_field_history<-example_grouped[with(example_grouped, order(Compound,Year,day)), ]
  
}

start.time<-Sys.time()
final_off_field_history_90m<-lapply(dailylist, final_output_by_field_history_func_OFF)
end.time<-Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
