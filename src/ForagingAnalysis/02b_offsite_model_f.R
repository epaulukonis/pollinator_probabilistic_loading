### Foraging Landscape

### 02b Initial Offsite Model Outputs

# Edited by E. Paulukonis Feb 2024
library(stringdist)
library(cowplot)
library(tidyverse)
library(ggh4x)
##### Read in data for models ----
#### App rates and timing:
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)


## Because we're dealing with off-field plants that have both nectar and pollen, here, we'll impute 'pollen and nectar' for each combo of compounds
library(tidyr)
apprates<-crossing(apprates, type=c("Pollen","Nectar"))

## It's important to remember that offsite, we are basically assuming wildflower/foraging habitat


#### Set up and read in variables for models:
## Li
Li<-read.csv(paste0(pest_dir,"/Models/LiParam.csv"))
Li<-Li %>% spread(Parameter, Value)
Li$Compound<-toupper(Li$Compound)


## Briggs
#defaults
theta<-0.2
foc<-0.01
bulkdensity<-1.5

## Purucker and Paulukonis
#elimination rate via growth
kel_grow<-0.035
#relative growth rate
rgr_corn<-0.066
rgr_soy<-0.06
#mass of seed (g)
m0_corn<-0.25
m0_soy<-0.15
#partition coeff
Fpl<-c(0.35,0.69, 0.69)
Fnl<-c(0.017,0.05, 0.05)
partcoeffn<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM"), (Fnl),paste0("Nectar")))
names(partcoeffn)<-c("Compound","Fr","Type")
partcoeffp<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM"), (Fpl),paste0("Pollen")))
names(partcoeffp)<-c("Compound","Fr","Type")
part_coeff<-rbind(partcoeffp, partcoeffn)


#### Set up empty matrix to hold outputs; these will auto-populate as you go through each application type
#first, we will create our empty df to populate the on-field concentrations by day
off_field_residues<-as.data.frame(matrix(NA,nrow = 151, ncol = 0))
off_field_residues[,1]<-0:150
colnames(off_field_residues)[1]<-"day"

#put all combos into a list where the name represents the scenario
off_field_residue_list<-rep(list(off_field_residues), 18L)
unique_names<-apprates %>% distinct(Compound,ApplicationType, Commodity)
names(off_field_residue_list)<-with(unique_names, paste0(Compound,"_",ApplicationType,"_",Commodity))




######## Analysis ###########
#### Seed Applications ----
## Dust ----
#these are the only residues we'll measure as ug/m2 
#First, we will derive the fraction of dust moving across and off the field 
d<-1:90 # first, convert the distance to meters
x<-exp(1.68-0.0905*log(d))-1 # estimate the fraction 
orig_conc_at_distance<-(x/1855)*1000 #ug/m2


#original conc in field, from Krupke
conc_in_field<-((2.06+11.92)/2)*1000#(ug/m2) # average application rate

#to translate this relationship to other in-field concentrations, 
#I assume that the fraction of off-site deposition based on the original concentration is reflective of all seed treatments
deposition<-(orig_conc_at_distance/conc_in_field) #fraction of off
air_krupke<-as.data.frame(cbind(d,orig_conc_at_distance, deposition))
avg_frac<-(0.001849+0.000319)/2

#add in the initial estimate of the concentration at 0 (in-field)
air_krupke<-rbind(c(0,3.81,avg_frac), air_krupke)

#air krupke represents a dataframe with information about the on and off-field estimate of deposition from seeds, based on the relationship
#between the concentration in field in ug/m2 and the amount reported offsite. we then use these depostion fractions to estimate the same relationship for other compounds

#visualize the original concentration, if desired
# p <- ggplot(air_krupke, aes(d, deposition*100)) +
#   geom_point()+
#   geom_line()+
#   ylab("Deposition (% of In-Field Concentration)")+
#   xlab("Distance[m]")+
#   theme_bw()
# p


#get the seed treatment data and estimate concentration in dust for each seed type
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]

#for dust, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
seed_treatments<-seed_treatments[!seed_treatments$type == "Nectar",]
seed_treatments<-seed_treatments[ , !(names(seed_treatments) %in% "type")]

seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_dust_conc<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_dust_conc<-seed_dust_conc[with(seed_dust_conc, order(Compound,Commodity)), ] # order by compound, commodity
seed_dust_conc$dust_drift_conc<-seed_dust_conc$ug_m2*seed_dust_conc$deposition #multiple application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
seed_dust_datasets<-split(seed_dust_conc, list(seed_dust_conc$Compound, seed_dust_conc$ApplicationType, seed_dust_conc$Commodity), drop=T) 

#here, we will estimate the concentration in ug per g
daily_conc_off_field<-list()
for(compound_and_crop in 1:length(seed_dust_datasets)){
  seed_dust_data_off_fieldf<-seed_dust_datasets[[compound_and_crop]]
  
  distance_sets<-list()
  for(distance in 1:90){
    
    #remove on-field
    seed_dust_data_off_field<-  seed_dust_data_off_fieldf[!seed_dust_data_off_fieldf$d == 0,]
    seed_dust_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_dust_data_off_field$dust_drift_conc #retain as ug/m2
    #extract unique distance, turn into own data-frame
    seed_dust_data_off_field<-seed_dust_data_off_field[seed_dust_data_off_field$d == distance,]
    
    seed_dust_data_off_field<-seed_dust_data_off_field[rep(seq_len(nrow(seed_dust_data_off_field)), 151),]
    seed_dust_data_off_field[2:151, 18]<-0
    seed_dust_data_off_field$day<-0:150
    seed_dust_data_off_field$distance<-distance
    distance_sets[[distance]]<-seed_dust_data_off_field[,c(1,5,6,19,20,18)] 
    
  }
  
  evaluate_quantiles<-do.call(rbind,distance_sets)
  
  daily_conc_off_field[[compound_and_crop]]<-evaluate_quantiles
  
}

#first put OG names 
names(daily_conc_off_field)<-names(seed_dust_datasets)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)

## Soil----
#get the seed treatment data and estimate soil concentration in dust for each seed type
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]

#for soil, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
seed_treatments<-seed_treatments[!seed_treatments$type == "Nectar",]
seed_treatments<-seed_treatments[ , !(names(seed_treatments) %in% "type")]

seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_soil_conc<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_soil_conc<-merge(seed_soil_conc, Li[ , c("Compound","KdissSoil")], by = "Compound", all.x=TRUE) # join specifically to get dissipation rate of compound in soil
seed_soil_conc<-seed_soil_conc[with(seed_soil_conc, order(Compound,Commodity)), ] # order by compound, commodity
seed_soil_conc$dust_drift_conc<-seed_soil_conc$ug_m2*seed_soil_conc$deposition #multiply application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
seed_soil_datasets<-split(seed_soil_conc, list(seed_soil_conc$Compound, seed_soil_conc$Commodity), drop=T) 


#here, we will estimate the concentration in ug per g, using both dust and amount contained in soil
daily_conc_off_field<-list()
for(compound_and_crop in 1:length(seed_soil_datasets)){
  seed_soil_data_off_fieldf<-seed_soil_datasets[[compound_and_crop]]
  seed_soil_data_off_fieldf<-seed_soil_data_off_fieldf[with(seed_soil_data_off_fieldf, order(d)), ]
  
  distance_sets<-list()
  for(distance in 1:90){
    #remove on-field
    seed_soil_data_off_field<- seed_soil_data_off_fieldf[!seed_soil_data_off_fieldf$d == 0,]
    seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
    #extract unique distance, turn into own data-frame
    seed_soil_data_off_field<-seed_soil_data_off_field[seed_soil_data_off_field$d == distance,]
    
    seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
    seed_soil_data_off_field$day<-0:150
    seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
    
    #first, get the concentration from the average deposition
    conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95)*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
    
    seed_soil_data_off_field$Soil_concentration_ug_g_from_seed <- conc_from_dust 
    seed_soil_data_off_field$distance<-distance
    distance_sets[[distance]]<-seed_soil_data_off_field[,c(1,2,3,6:8)] 
    
  }
  
  evaluate_quantiles<-do.call(rbind,distance_sets)
  
  
  daily_conc_off_field[[compound_and_crop]]<-evaluate_quantiles
}


#first put OG names 
names(daily_conc_off_field)<-names(seed_soil_datasets)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)




## Pollen and Nectar----
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]
seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_treatments<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_treatments<-seed_treatments[with(seed_treatments, order(Compound,Commodity)), ] # order by compound, commodity
seed_treatments$dust_drift_conc<-seed_treatments$ug_m2*seed_treatments$deposition #multiple application rate times deposition curve 

seed_treatments<-seed_treatments[,!names(seed_treatments) %in% "Type"]
#seed_treatments<-merge(seed_treatments, part_coeff, by="Compound")
seed_nectarpollen_conc<-merge(seed_treatments,Li) #merge with the fraction estimated via Krupke


##because we have both nectar and pollen, we need to split up the sets into two batches
seed_treatments_pollen<-seed_nectarpollen_conc[seed_nectarpollen_conc$type == "Pollen",]
seed_treatments_nectar<-seed_nectarpollen_conc[seed_nectarpollen_conc$type == "Nectar",]



## Pollen
#split by compound, crop, and type (nectar or pollen)
seed_pollen_datasets<-split(seed_treatments_pollen, list(seed_treatments_pollen$Compound, seed_treatments_pollen$ApplicationType, seed_treatments_pollen$Commodity), drop=T)

#x<-seed_pollen_datasets[[1]]

residues_in_nectar_and_pollen_from_seed<-function(x){
  
  distance_sets<-list()
  for(distance in 1:90){
    #remove on-field
    seed_soil_data_off_field<- x[!x$d == 0,]
    seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
    #extract unique distance, turn into own data-frame
    seed_soil_data_off_field<-seed_soil_data_off_field[seed_soil_data_off_field$d == distance,]
    
    seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
    seed_soil_data_off_field$day<-0:150
    
    # seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
    
    #first, get the concentration from the average deposition
    conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95)#*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
    
    #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
    seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-conc_from_dust 
    
    
    output<- matrix(data=0, nrow=151, ncol=3)
    #n<-ifelse(x$Commodity == "CORN", 67,56)
    t<-1:150
    output[2:151,2]<-t
    for(i in 1:nrow(output)){
      # units will be ug per g of pollen
      # first get uptake from soil
      pol<-seed_soil_data_off_field[i,]
      uptakesoil <-( (pol$`Kp-L`* pol$Soil_concentration_ug_g_from_seed) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
      
      #then estimate RUD due to leaf deposition
      uptakeleaf<-( ((pol$`Kp-L`*pol$fsurface*pol$cf1)/(pol$LAI*pol$LMA*(1/(1-pol$Wleaf))*1000)) * (pol$KupSurface/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSurface)) * ( exp(-pol$KdissSurface*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) * (pol$Dust_concentration_ug_m2_from_seed/100000) # sub dust as AR
      output[i,1]<- (uptakesoil + uptakeleaf) 
      
    }
    output<-as.data.frame(output)
    output[,3]<-distance
    names(output)<-c(paste0(pol$type,'_concentration_ug_g_from_seed'),"day","distance")
    distance_sets[[distance]]<-output
    
  }
  
  do.call(rbind,distance_sets)
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(seed_pollen_datasets,residues_in_nectar_and_pollen_from_seed)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)


## Nectar
#split by compound, crop, and type (nectar or pollen)
seed_nectar_datasets<-split(seed_treatments_nectar, list(seed_treatments_nectar$Compound, seed_treatments_nectar$ApplicationType, seed_treatments_nectar$Commodity), drop=T)

#x<-seed_nectar_datasets[[1]]

residues_in_nectar_and_pollen_from_seed<-function(x){
  
  distance_sets<-list()
  for(distance in 1:90){
    #remove on-field
    seed_soil_data_off_field<- x[!x$d == 0,]
    seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
    #extract unique distance, turn into own data-frame
    seed_soil_data_off_field<-seed_soil_data_off_field[seed_soil_data_off_field$d == distance,]
    
    seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
    seed_soil_data_off_field$day<-0:150
    
    # seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
    
    #first, get the concentration from the average deposition
    conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95)#*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
    
    seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-conc_from_dust 
    
    
    output<- matrix(data=0, nrow=151, ncol=3)
    #n<-ifelse(x$Commodity == "CORN", 67,56)
    t<-1:150
    output[2:151,2]<-t
    for(i in 1:nrow(output)){
      # units will be ug per g of pollen
      # first get uptake from soil
      pol<-seed_soil_data_off_field[i,]
      uptakesoil <-( (pol$`Kn-L`* pol$Soil_concentration_ug_g_from_seed) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
      
      #then estimate RUD due to leaf deposition
      uptakeleaf<-( ((pol$`Kn-L`*pol$fsurface*pol$cf1)/(pol$LAI*pol$LMA*(1/(1-pol$Wleaf))*1000)) * (pol$KupSurface/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSurface)) * ( exp(-pol$KdissSurface*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) * (pol$Dust_concentration_ug_m2_from_seed/100000) # sub dust as AR
      output[i,1]<- (uptakesoil + uptakeleaf) 
      
    }
    output<-as.data.frame(output)
    output[,3]<-distance
    names(output)<-c(paste0(pol$type,'_concentration_ug_g_from_seed'),"day","distance")
    distance_sets[[distance]]<-output
    
  }
  
  do.call(rbind,distance_sets)
  
}




#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(seed_nectar_datasets,residues_in_nectar_and_pollen_from_seed)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)





#### Foliar Applications----
## Air ----
#these are the only residues we'll measure as ug/m2
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))
#convert to meters
ag_drift$distance<-round(ag_drift$distance*0.3048,1)

#reduce to only 90 meter intervals of 1
new_ag<-list()
ag_drift<-for(num in 1:90){
  new_ag[[num]]<-ag_drift[which.min(abs(ag_drift$distance  - num)),]
}

ag_drift<-do.call(rbind,new_ag)
agdrift_data<-ag_drift[,c(1,7)] #use ground spray, very fine to fine

#get the seed treatment data and estimate concentration in dust for each seed type
foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]

#for drift, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
foliar_treatments<-foliar_treatments[!foliar_treatments$type == "Nectar",]
foliar_treatments<-foliar_treatments[ , !(names(foliar_treatments) %in% "type")]

foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
foliar_air_conc<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
foliar_air_conc<-foliar_air_conc[with(foliar_air_conc, order(Compound,Commodity)), ] # order by compound, commodity
foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_ground_low_vf2f #multiple application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
foliar_air_datasets<-split(foliar_air_conc, list(foliar_air_conc$Compound,foliar_air_conc$ApplicationType, foliar_air_conc$Commodity), drop=T) 

daily_conc_off_field<-list()
for(compound_and_crop in 1:length(foliar_air_datasets)){
  foliar_air_dataf<-foliar_air_datasets[[compound_and_crop]]
  
  distance_sets<-list()
  for(distance in 1:90){
    #remove on-field
    foliar_air_data_off_field<-  foliar_air_dataf[!foliar_air_dataf$d == 0,]
    foliar_air_data_off_field$distance<-1:90
    foliar_air_data_off_field$Air_concentration_ug_m2_from_foliar<-foliar_air_data_off_field$air_drift_conc #retain as ug/m2
    
    #extract unique distance, turn into own data-frame
    foliar_air_data_off_field<-foliar_air_data_off_field[foliar_air_data_off_field$d == distance,]
    
    foliar_air_data_off_field<- foliar_air_data_off_field[rep(seq_len(nrow(foliar_air_data_off_field)), 151),]
    foliar_air_data_off_field[2:151, 17]<-0
    foliar_air_data_off_field$day<-0:150
    foliar_air_data_off_field$distance<-distance
    
    distance_sets[[distance]]<- foliar_air_data_off_field[,c(1,5,6,14,17:18)]
  }
  
  daily_conc_off_field[[compound_and_crop]]<-do.call(rbind,distance_sets)
  
}

#first put OG names 
names(daily_conc_off_field)<-names(foliar_air_datasets)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list) 



## Soil ----
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))
#convert to meters
ag_drift$distance<-round(ag_drift$distance*0.3048,1)

#reduce to only 90 meter intervals of 1
new_ag<-list()
ag_drift<-for(num in 1:90){
  new_ag[[num]]<-ag_drift[which.min(abs(ag_drift$distance  - num)),]
}

ag_drift<-do.call(rbind,new_ag)
agdrift_data<-ag_drift[,c(1,7)] #use ground spray, very fine to fine

#get the seed treatment data and estimate concentration in dust for each seed type
foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]

#for soil, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
foliar_treatments<-foliar_treatments[!foliar_treatments$type == "Nectar",]
foliar_treatments<-foliar_treatments[ , !(names(foliar_treatments) %in% "type")]

foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
foliar_air_conc<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
foliar_air_conc<-foliar_air_conc[with(foliar_air_conc, order(Compound,Commodity)), ] # order by compound, commodity
foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_ground_low_vf2f #multiple application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
foliar_soil_datasets<-split(foliar_air_conc, list(foliar_air_conc$Compound,foliar_air_conc$ApplicationType, foliar_air_conc$Commodity), drop=T) 

daily_conc_off_field<-list()
for(compound_and_crop in 1:length(foliar_soil_datasets)){
  foliar_soil_dataf<-foliar_soil_datasets[[compound_and_crop]]
  
  distance_sets<-list()
  for(distance in 1:90){
    #remove on-field
    foliar_soil_data_off_field<-  foliar_soil_dataf[!foliar_soil_dataf$d == 0,]
    foliar_soil_data_off_field$distance<-1:90
    foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-foliar_soil_data_off_field$air_drift_conc #retain as ug/m2
    
    #extract unique distance, turn into own data-frame
    foliar_soil_data_off_field<-foliar_soil_data_off_field[foliar_soil_data_off_field$d == distance,]
    
    
    foliar_soil_data_off_field<- foliar_soil_data_off_field[rep(seq_len(nrow(foliar_soil_data_off_field)), 151),]
    #foliar_soil_data_off_field[2:151, 17]<-0
    foliar_soil_data_off_field$day<-0:150
    foliar_soil_data_off_field<-foliar_soil_data_off_field[,c(1,5,6,12, 17,18)]
    
    #first, get the concentration from the average deposition
    conc_from_air<-((foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar * (1/10000))/2 * 1/1.95)*exp(-(foliar_soil_data_off_field$k_values*foliar_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
    
  
    foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar <- conc_from_air 
    foliar_soil_data_off_field$distance<-distance
    distance_sets[[distance]]<-foliar_soil_data_off_field[,c(1:3,6:8)]
  }
  
  evaluate_quantiles<-do.call(rbind,distance_sets)
  
  daily_conc_off_field[[compound_and_crop]]<-evaluate_quantiles
  
  
}

test<-daily_conc_off_field[[1]]

#first put OG names 
names(daily_conc_off_field)<-names(foliar_soil_datasets)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)




## Pollen and Nectar----
#these are the only residues we'll measure as ug/m2
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))
#convert to meters
ag_drift$distance<-round(ag_drift$distance*0.3048,1)

#reduce to only 90 meter intervals of 1
new_ag<-list()
ag_drift<-for(num in 1:90){
  new_ag[[num]]<-ag_drift[which.min(abs(ag_drift$distance  - num)),]
}

ag_drift<-do.call(rbind,new_ag)
agdrift_data<-ag_drift[,c(1,7)] #use ground spray, very fine to fine

foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
foliar_treatments<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
foliar_treatments<- foliar_treatments[with( foliar_treatments, order(Compound,Commodity)), ] # order by compound, commodity
foliar_treatments$air_drift_conc<- foliar_treatments$ug_m2* foliar_treatments$pond_ground_low_vf2f #multiple application rate times deposition curve 

foliar_treatments<-foliar_treatments[,!names(foliar_treatments) %in% "Type"]
foliar_nectarpollen_conc<-merge(foliar_treatments,Li) #merge with the fraction estimated via Krupke

##because we have both nectar and pollen, we need to split up the sets into two batches
foliar_treatments_pollen<-foliar_nectarpollen_conc[foliar_nectarpollen_conc$type == "Pollen",]
foliar_treatments_nectar<-foliar_nectarpollen_conc[foliar_nectarpollen_conc$type == "Nectar",]



## Pollen
#split by compound, crop, and type (nectar or pollen)
foliar_pollen_datasets<-split(foliar_treatments_pollen, list(foliar_treatments_pollen$Compound, foliar_treatments_pollen$ApplicationType,foliar_treatments_pollen$Commodity), drop=T)

residues_in_nectar_and_pollen_from_foliar<-function(x){
  
  distance_sets<-list()
  for(distance in 1:90){
    #remove on-field
    foliar_soil_data_off_field<- x[!x$d == 0,]
    foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-foliar_soil_data_off_field$air_drift_conc #retain as ug/m2
    foliar_soil_data_off_field$distance<-1:90
    #extract unique distance, turn into own data-frame
    foliar_soil_data_off_field<-foliar_soil_data_off_field[foliar_soil_data_off_field$d == distance,]
    
    foliar_soil_data_off_field<-foliar_soil_data_off_field[rep(seq_len(nrow(foliar_soil_data_off_field)), 151),]
    foliar_soil_data_off_field$day<-0:150
    
    #first, get the concentration from the average deposition
    conc_from_air<-((foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar * (1/10000))/2 * 1/1.95)#*exp(-(foliar_soil_data_off_field$k_values*foliar_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
    
    foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar <- conc_from_air 
    
    output<- matrix(data=0, nrow=151, ncol=2)
    #n<-ifelse(x$Commodity == "CORN", 67,56)
    t<-1:150
    output[2:151,2]<-t
    for(i in 1:nrow(output)){
      # units will be ug per g of pollen
      # first get uptake from soil
      pol<-foliar_soil_data_off_field[i,]
      uptakesoil <-( (pol$`Kp-L`* pol$Soil_concentration_ug_g_from_foliar) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*i) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
      
      #then estimate RUD due to leaf deposition
      uptakeleaf<-( ((pol$`Kp-L`*pol$fsurface*pol$cf1)/(pol$LAI*pol$LMA*(1/(1-pol$Wleaf))*1000)) * (pol$KupSurface/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSurface)) * ( exp(-pol$KdissSurface*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) * (pol$Air_concentration_ug_m2_from_foliar/100000) # sub air drift as AR in kg/ha
      
      output[i,1]<- (uptakesoil + uptakeleaf) 
      
    }
    output<-as.data.frame(output)
    output[,3]<-distance
    names(output)<-c(paste0(pol$type,'_concentration_ug_g_from_foliar'),"day","distance")
    distance_sets[[distance]]<-output
    
  }
  
  do.call(rbind,distance_sets)
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(foliar_pollen_datasets,residues_in_nectar_and_pollen_from_foliar)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)


## Nectar
#split by compound, crop, and type (nectar or pollen)
foliar_nectar_datasets<-split(foliar_treatments_nectar, list(foliar_treatments_nectar$Compound, foliar_treatments_nectar$ApplicationType, foliar_treatments_nectar$Commodity), drop=T)

#x<-foliar_nectar_datasets[[1]]
residues_in_nectar_and_pollen_from_foliar<-function(x){
  
  distance_sets<-list()
  for(distance in 1:90){
    #remove on-field
    foliar_soil_data_off_field<- x[!x$d == 0,]
    foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-foliar_soil_data_off_field$air_drift_conc #retain as ug/m2
    foliar_soil_data_off_field$distance<-1:90
    #extract unique distance, turn into own data-frame
    foliar_soil_data_off_field<-foliar_soil_data_off_field[foliar_soil_data_off_field$d == distance,]
    
    foliar_soil_data_off_field<-foliar_soil_data_off_field[rep(seq_len(nrow(foliar_soil_data_off_field)), 151),]
    foliar_soil_data_off_field$day<-0:150
    
    #first, get the concentration from the average deposition
    conc_from_air<-((foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar * (1/10000))/2 * 1/1.95)#*exp(-(foliar_soil_data_off_field$k_values*foliar_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
    
    foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar <- conc_from_air 
    
    output<- matrix(data=0, nrow=151, ncol=2)
    #n<-ifelse(x$Commodity == "CORN", 67,56)
    t<-1:150
    output[2:151,2]<-t
    for(i in 1:nrow(output)){
      # units will be ug per g of pollen
      # first get uptake from soil
      pol<-foliar_soil_data_off_field[i,]
      uptakesoil <-( (pol$`Kn-L`* pol$Soil_concentration_ug_g_from_foliar) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*i) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
      
      #then estimate RUD due to leaf deposition
      uptakeleaf<-( ((pol$`Kn-L`*pol$fsurface*pol$cf1)/(pol$LAI*pol$LMA*(1/(1-pol$Wleaf))*1000)) * (pol$KupSurface/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSurface)) * ( exp(-pol$KdissSurface*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) * (pol$Air_concentration_ug_m2_from_foliar/100000) # sub air drift as AR in kg/ha
      
      output[i,1]<- (uptakesoil + uptakeleaf) 
      
    }
    output<-as.data.frame(output)
    output[,3]<-distance
    names(output)<-c(paste0(pol$type,'_concentration_ug_g_from_foliar'),"day","distance")
    distance_sets[[distance]]<-output
    
  }
  
  do.call(rbind,distance_sets)
  
}



#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(foliar_nectar_datasets,residues_in_nectar_and_pollen_from_foliar)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)

off_field_residue_list<-off_field_residue_list[1:16]

test<-off_field_residue_list[[1]]

# combine data -----
#simple function to arrange data
gather_data<-function(x){
  df<-x
  df<-gather(df, "Media", "Value", 6:ncol(df))
  df
}

off_model_data<-lapply(off_field_residue_list,gather_data)
off_model_data<-do.call(rbind,off_model_data)
off_model_data$Value<-ifelse(off_model_data$Value == 0,NA,off_model_data$Value)

#`test<-off_model_data[off_model_data$Compound == "CARBARYL",]

#Add in specific media for grouping
off_model_data$MediaSub<- sub("_.*", "", off_model_data$Media)
off_model_data$ApplicationType<- str_to_title(sub('.*\\_', "",off_model_data$Media))



off_model_data<-off_model_data[,c(1:2,4:8,3)] #we will have one extra column to contend with that marks the distance off-field

#pull out average per 30m2 raster
off_model_data_30m<- off_model_data[off_model_data$distance <=30,]
off_model_data_60m<- off_model_data[off_model_data$distance >30 & off_model_data$distance <=60,]
off_model_data_90m<-off_model_data[off_model_data$distance >60 ,]


#let's do an average across each 30,60, and 90m location
sum_by_location<-function(x){x %>% group_by(Compound, Commodity, ApplicationType,MediaSub, day) %>% summarise(avg=mean(Value))}

off_model_data_30m<-sum_by_location(off_model_data_30m)
off_model_data_60m<-sum_by_location(off_model_data_60m)
off_model_data_90m<-sum_by_location(off_model_data_90m)
