### this is older code that I am keeping to track for the time being
# treatment_probabilities_corn<-as.data.frame(cbind(status,corn_seed_probs, corn_fol_probs,corn_soil_probs))
# treatment_probabilities_soy<-as.data.frame(cbind(status,soy_seed_probs, soy_fol_probs,soy_soil_probs))
# treatment_probabilities_soy$t_id<-1:4
# treatment_probabilities_corn$t_id<-1:4
# cols.num <- names(treatment_probabilities_corn[,2:4])
# treatment_probabilities_corn[cols.num] <- sapply(treatment_probabilities_corn[cols.num],as.numeric)
# 
# cols.num <- names(treatment_probabilities_soy[,2:4])
# treatment_probabilities_soy[cols.num] <- sapply(treatment_probabilities_soy[cols.num],as.numeric)


# treatment_probabilities_soy$year<-names(fv[year])
# treatment_probabilities_corn$year<-names(fv[year])

# corn_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==1,]),replace=T, prob=treatment_probabilities_corn$probs_S1)
# soy_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==5,]),replace=T, prob=treatment_probabilities_soy$probs_S1)

# corn_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==1,]),replace=T, prob=treatment_probabilities_corn$probs_S1)
# soy_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==5,]),replace=T, prob=treatment_probabilities_soy$probs_S1)


#let's create a function to assign new columns, add back to fields, specify application type

#corn_samp$crop<-1
# corn_samp<-corn_samp[,c(ncol(corn_samp),1:ncol(corn_samp)-1)]
# corn_samp<-corn_samp %>% gather("Compound", "ID", 2:ncol(corn_samp))

#soy_samp$crop<-5
# soy_samp<-soy_samp[,c(ncol(soy_samp),1:ncol(soy_samp)-1)]
# soy_samp<-soy_samp %>% gather("Compound", "ID", 2:ncol(soy_samp))



#let's create a function to assign new columns, add back to fields, specify application type

#corn_samp$crop<-1
# corn_samp<-corn_samp[,c(ncol(corn_samp),1:ncol(corn_samp)-1)]
# corn_samp<-corn_samp %>% gather("Compound", "ID", 2:ncol(corn_samp))

#soy_samp$crop<-5
# soy_samp<-soy_samp[,c(ncol(soy_samp),1:ncol(soy_samp)-1)]
# soy_samp<-soy_samp %>% gather("Compound", "ID", 2:ncol(soy_samp))

#fcs$treatment_type<-treatment_probabilities_corn$ApplicationType[match(fcs$streatment, treatmentid$ID)]
#ifelse(field_cornsoy$crop ==1, corn_fields, soy_fields)


#sample foliar
# corn_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==1,]),replace=T, prob=treatment_probabilities_corn$corn_fol_probs)
# soy_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==5,]),replace=T, prob=treatment_probabilities_soy$soy_fol_probs)
# #add back to fields, specify application type
# field_cornsoy$ftreatment<-ifelse(field_cornsoy$crop ==1, corn_fields, soy_fields)
# field_cornsoy$ftreatment_type<-treatment_probabilities_corn$status[match(field_cornsoy$ftreatment, treatment_probabilities_corn$t_id)]
# 
# #sample in soil
# corn_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==1,]),replace=T, prob=treatment_probabilities_corn$corn_soil_probs)
# soy_fields<-sample(1:4,size=nrow(field_cornsoy[field_cornsoy$crop==5,]),replace=T, prob=treatment_probabilities_soy$soy_soil_probs)
# #add back to fields, specify application type
# field_cornsoy$istreatment<-ifelse(field_cornsoy$crop ==1, corn_fields, soy_fields)
# field_cornsoy$istreatment_type<-treatment_probabilities_corn$status[match(field_cornsoy$istreatment, treatment_probabilities_corn$t_id)]

#function to target columns with compound data
# names_between <- function(col_1, col_2, data){
#   names(data)[(which(names(data) == col_1) +1) : (which(names(data) == col_2)-1)]
# }
# 
# #how to use:
# #("year","geometry",data=fcs)
# 


# treatment_probabilities_soy$year<-names(fv[year])
# treatment_probabilities_corn$year<-names(fv[year])

# crop_probs_corn<-split(crop_probs_corn,crop_probs_corn$Status)
# crop_probs_soy<-split(crop_probs_soy,crop_probs_soy$Status)


# field_other$Sample<-paste0("NotAssigned")
# field_f<-rbind(field_cornsoy,field_other)
# field_f <- field_f[order(field_f$id),]
# split_by_week<-split(field_f, f = field_f$Sample)

#### Proportional data by crop ----
## first, let's calculate the probability that corn or soy will be assigned one of 3 compounds over the years, 
## based on the proportion of total N kg in a given year the compound represents

#first, assign 'type', glyphosate or neonicotinoid
pest_prop_by_crop$type<-ifelse(pest_prop_by_crop$Compound == 'GLYPHOSATE',"G","N")

#then calculate the amount of each compound represented as a whole (% imidacloprid, %clothianidan, etc)
pest_prop_by_crop_agg<-pest_prop_by_crop %>% group_by(type,Year, Commodity) %>% mutate(prop = kg/(sum(kg)))

#verify it looks good
#testy<-pest_prop_by_crop_agg %>% filter(Year==2005)

#a challenge is that prior to 2004, not many compounds were being used and data reported in Douglas and Tooker 2015 shows that 2004/2006 were first major years for corn/soy
# and we don't want to back fill data to years where probably the compounds truly were not being used
# so one solution is just to remove rows prior to 2004 and 2006 for appropriate crops
pest_prop_by_crop_agg<-pest_prop_by_crop_agg[!(pest_prop_by_crop_agg$type == "N" & pest_prop_by_crop_agg$Commodity == "CORN" & pest_prop_by_crop_agg$Year < 2004 | pest_prop_by_crop_agg$type == "N" & pest_prop_by_crop_agg$Commodity == "SOYBEANS" & pest_prop_by_crop_agg$Year < 2006),]

#backfill to nearest neighbor for the years we don't have data that are appropriate
pest_prop_by_crop_agg <-pest_prop_by_crop_agg %>% group_by(Compound) %>% mutate(prop_final =f1(prop))

#finally, assign ID to match for loop
pest_prop_by_crop_agg$crop<-ifelse(pest_prop_by_crop_agg$Commodity == "CORN",1,5)

treated_fields<-list()
for(year in 1:16){
  field_data<-list_of_sampled_fields_by_year[[year+5]]
  #here, for soy, convert any seed treatments to notreatment prior to 2006
  field_data$ntreatment_type<- ifelse(field_data$crop ==5 & field_data$year > 2006, field_data$ntreatment_type == "notreatment", field_data$ntreatment_type)
  
  proportion_df<-pest_prop_by_crop_agg[pest_prop_by_crop_agg$Year == names(list_of_sampled_fields_by_year[year+5]),]
  corn<-field_data[field_data$crop == 1,]
  soy<-field_data[field_data$crop == 5,]
  
  
  #function to sample neonics based on proportional use by crop across Illinois
  sample_neo<-function(x){
    x<-corn
    props<-proportion_df[proportion_df$crop %in% x$crop & proportion_df$type == "N",]
    sample(props$g_per_m2,nrow(x[x$ntreatment_type == 'seed',]),
           replace=T,prob= props$prop_final)
  }
  
  #need to assign compound specific info
  #here, the function samples individual ARs (appplication rate) depending on the percent of individual neonic used in a given year by crop
  treatmentnc<-sample_neo(corn)
  corn$nloading_gm2<-ifelse(corn$ntreatment_type == 'seed',treatmentnc,0)
  corn$ncompound<-proportion_df$Compound[match(corn$nloading_gm2, proportion_df$g_per_m2)]
  corn$ncompound[is.na(corn$ncompound)] <- "NONE"
  
  #soy needs an extra step because there are no neonics until 2006
  if(soy$year>=2006){
    treatmentns<-sample_neo(soy)
    soy$nloading_gm2<-ifelse(soy$ntreatment_type == 'seed',treatmentns,0)
    soy$ncompound<-proportion_df$Compound[match(soy$nloading_gm2, proportion_df$g_per_m2)]
    soy$ncompound[is.na(soy$ncompound)] <- "NONE"
  }else{
    soy$nloading_gm2<-0
    soy$ncompound<-"NONE"
  }
  
  #glyphosate is easier, we only need to to sample a single AR
  corn$gloading_gm2<- ifelse(corn$gtreatment_type == "foliar",proportion_df$g_per_m2[proportion_df$crop %in% soy$crop & proportion_df$type == "G"],0)
  soy$gloading_gm2<- ifelse(soy$gtreatment_type == "foliar",proportion_df$g_per_m2[proportion_df$crop %in% soy$crop & proportion_df$type == "G"],0)
  corn$gcompound<-"GLYPHOSATE"
  soy$gcompound<-"GLYPHOSATE"
  
  names(soy)
  names(corn)
  #rbind the data back together
  field_data_f<-rbind(corn,soy)
  #calculate the area
  field_data_f$area<-st_area(field_data_f)
  
  #calculate the total amount (in grams) per field based on area 
  field_data_f$totalloadingN_g<-field_data_f$nloading_gm2*field_data_f$area
  field_data_f$totalloadingG_g<-field_data_f$gloading_gm2*field_data_f$area
  
  #put into list
  treated_fields[[year]]<-field_data_f
  
}

#### Sampling treatment application rates ----
### in this section, I'll assign the loading for each compound based on proportional representation in Illinois by crop 

names(treated_fields)<-2004:2019

plot(field_data_f['totalloadingN_g'])
plot(field_data_f['totalloadingG_g'])


#st_write(treated_fields[[1]], paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), layer=paste0("test_vector_layer"), driver = "ESRI Shapefile")


#### Comparison to stuff, move later---- 
###Later on, we can compare the results to the reported totals by county
#now, let's assign the type the county total pesticides
county_pest$type<-ifelse(county_pest$COMPOUND == 'GLYPHOSATE',"G","N")
#aggregate by year
county_pest_agg<-county_pest %>% group_by(COMPOUND, YEAR) %>% mutate(sum = sum(EPEST_HIGH_KG))
#calculate total sum of compoud
county_pest_agg<-county_pest_agg %>% group_by(type,YEAR) %>% mutate(total = sum(EPEST_HIGH_KG))
#this provides use with a way to compare the probability that a field is assigned a particular compound within our 3 county area, against the pest_by_crop_agg
county_pest_agg$prop<-county_pest_agg$sum/county_pest_agg$total

#create codes within the county crop dataframe for later use
countyname<-as.data.frame(cbind(c(7,11,201),c("BOONE","MCHENRY","WINNEBAGO")))
names(countyname)<-c("COUNTY_FIPS_CODE", "County")
crop_by_county<-left_join(crop_by_county,countyname,by="County")



####################################line 222-235
#######OLD
### SEED (imidacloprid for soy, clothianidan for corn, thiamethoxam for both)
# Take from Hitaj et al. Figure 2; this represents the proportion of ALL corn/soy that is treated with neonicotinoids
# corn_seed_probs<-c(32,45,55,60,68,72,75,80,90,90,90,90,90,90,90,90)/100
# soy_seed_probs<- c(25,25,25,35,50,60,70,70,75,75,75,75,75,75,75,75 )/100
# yearsseed<-2006:2021

# cornp<-as.data.frame(cbind(corn_seed_probs,"CORN",yearsseed))
# soyp<-as.data.frame(cbind(soy_seed_probs,"SOYBEANS",yearsseed))
# 
# names(cornp)<-c("probs","COMMODITY","Year")
# names(soyp)<-c("probs","COMMODITY","Year")
# seeddat<-rbind(cornp,soyp)
# seeddat$ApplicationType<-"Seed"

#join together the seed data pros 
pest_crop<-merge(pest_crop, seeddat, by.x = c("Year", "Commodity","ApplicationType"), by.y = c("Year", "COMMODITY","ApplicationType"), 
                 all.x = TRUE, all.y = TRUE)

pest_cropf$probs_S1[is.na(pest_cropf$probs_S1)]<-sample(c(0.25,0.50,0.75),sum(is.na(pest_cropf$probs_S1)), replace=T)


#for foliar compounds, adjusted date to 49 or 60 depending on chemical
# fcs$emergeddates<-if(fcs$PERMETHRIN >=1){
#        as.Date(fcs$planteddates) + 49
#        }else if(fcs$CARBARYL >=1){
#          as.Date(fcs$planteddates) + 60
#        }else{NA} #

#for each unique ID, if an application type is used twice, randomly drop one. 
sample_for_application <- function(data, app_to_drop, n) {
  data %>%
    filter(ApplicationType %in% app_to_drop) %>%
    group_by(id) %>%
    slice_sample(n = n) %>%
    ungroup %>%
    bind_rows(data %>% filter(!ApplicationType %in% app_to_drop))
}


##won't work, need to sample based on proportion  
fcs<-sample_for_application(fcs,"Foliar",1)

#if 2006 or later, randomly sample seed type 
fcsf<-if(Year>=8){
  sample_for_application(fcs,'Seed', 1)
}else{fcs}

#don't sample if probabilities are 1
# treatment_probabilities_corn<-treatment_probabilities_corn[!treatment_probabilities_corn$probs_treated>=1,]
# treatment_probabilities_soy<-treatment_probabilities_soy[!treatment_probabilities_soy$probs_treated>=1,]

# #### here we sample for which compound we'll be assigning as our treatment of that application type
# application_type_sample_corn<- treatment_probabilities_corn %>% group_by(ApplicationType) %>%  sample_n(1) %>% pull(Compound)
# application_type_sample_soy<- treatment_probabilities_soy %>% group_by(ApplicationType) %>%  sample_n(1) %>% pull(Compound)
# 
# # subset to the compounds sampled by application type
# treatment_probabilities_corn<-treatment_probabilities_corn[treatment_probabilities_corn$Compound %in% application_type_sample_corn,]
# treatment_probabilities_soy<-treatment_probabilities_soy[treatment_probabilities_soy$Compound %in% application_type_sample_soy,]


