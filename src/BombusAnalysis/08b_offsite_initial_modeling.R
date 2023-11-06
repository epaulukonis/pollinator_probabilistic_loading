### Probabilistic Crop Loading 

### 07b Initial Offsite Model Outputs

# Edited by E. Paulukonis August 2023
library(stringdist)
library(cowplot)
library(tidyverse)

##### Read in data for models ----
#### App rates and timing:
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)


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

## Nectar and Pollen assignments, if needed
Compound<-c("IMIDACLOPRID","IMIDACLOPRID","CHLORPYRIFOS","CHLORPYRIFOS","CHLORPYRIFOS", "CARBOFURAN","CARBARYL", "CARBARYL","CARBARYL", "BIFENTHRIN","BIFENTHRIN","BIFENTHRIN","GLYPHOSATE","GLYPHOSATE","GLYPHOSATE")
Type<-c("Pollen","Nectar","Pollen","Nectar","Pollen","Pollen","Pollen","Nectar","Pollen","Pollen","Nectar","Pollen","Pollen","Nectar","Pollen")
Commodity<-c("SOYBEANS","SOYBEANS","SOYBEANS","SOYBEANS","CORN","CORN","SOYBEANS","SOYBEANS","CORN","SOYBEANS","SOYBEANS","CORN","SOYBEANS","SOYBEANS","CORN")

foliar_type<-as.data.frame(cbind(Compound,Type,Commodity))

#### Set up empty matrix to hold outputs; these will auto-populate as you go through each application type
#first, we will create our empty df to populate the on-field concentrations by day
off_field_residues<-as.data.frame(matrix(NA,nrow = 151, ncol = 0))
off_field_residues[,1]<-0:150
colnames(off_field_residues)[1]<-"day"

#put all combos into a list where the name represents the scenario
off_field_residue_list<-rep(list(off_field_residues), 16L)
unique_names<-apprates %>% distinct(Compound,ApplicationType, Commodity)
names(off_field_residue_list)<-with(unique_names, paste0(Compound,"_",ApplicationType,"_",Commodity))



#### Seed Applications ----
## Dust ----
#these are the only residues we'll measure as ug/m2 
#First, we will derive the fraction of dust moving across and off the field 
d<-1:299*0.3048 # first, convert the distance to meters
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
seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_dust_conc<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_dust_conc<-seed_dust_conc[with(seed_dust_conc, order(Compound,Commodity)), ] # order by compound, commodity
seed_dust_conc$dust_drift_conc<-seed_dust_conc$ug_m2*seed_dust_conc$deposition #multiple application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
seed_dust_datasets<-split(seed_dust_conc, list(seed_dust_conc$Compound, seed_dust_conc$ApplicationType, seed_dust_conc$Commodity), drop=T) 

#here, we will estimate the concentration in ug per g

daily_conc_off_field<-list()
for(compound_and_crop in 1:length(seed_dust_datasets)){
  seed_dust_data_off_field<-seed_dust_datasets[[compound_and_crop]]
  seed_dust_data_off_field<- seed_dust_data_off_field[!seed_dust_data_off_field$d == 0,]
  seed_dust_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_dust_data_off_field$dust_drift_conc #retain as ug/m2
  seed_dust_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_dust_data_off_field$Dust_concentration_ug_m2_from_seed)
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  seed_dust_data_off_field<-  seed_dust_data_off_field[1,]
  seed_dust_data_off_field<-seed_dust_data_off_field[rep(seq_len(nrow(seed_dust_data_off_field)), 151),]
  seed_dust_data_off_field[2:151, 18]<-0
  seed_dust_data_off_field$day<-0:150
  daily_conc_off_field[[compound_and_crop]]<-seed_dust_data_off_field[,c(1,5,6,18,19)]
  
}

#first put OG names 
names(daily_conc_off_field)<-names(seed_dust_datasets)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
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
seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_soil_conc<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_soil_conc<-merge(seed_soil_conc, Li[ , c("Compound","KdissSoil")], by = "Compound", all.x=TRUE) # join specifically to get dissipation rate of compound in soil
seed_soil_conc<-seed_soil_conc[with(seed_soil_conc, order(Compound,Commodity)), ] # order by compound, commodity
seed_soil_conc$dust_drift_conc<-seed_soil_conc$ug_m2*seed_soil_conc$deposition #multiply application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
seed_soil_datasets<-split(seed_soil_conc, list(seed_soil_conc$Compound, seed_soil_conc$Commodity), drop=T) 


#get PWC set for seed
pwc_set<-PWC_data_list[grepl('seed', names(PWC_data_list))]
names(pwc_set)<-toupper(names(pwc_set))



#here, we will estimate the concentration in ug per g, using both dust and amount contained in soil
daily_conc_off_field<-list()
for(compound_and_crop in 1:length(seed_soil_datasets)){
  seed_soil_data_off_field<-seed_soil_datasets[[compound_and_crop]]
  seed_soil_data_off_field<- seed_soil_data_off_field[!seed_soil_data_off_field$d == 0,]
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed) # get average dust deposition
  
  #we can drop all rows except 1 because we've averaged the deposition
  seed_soil_data_off_field<-seed_soil_data_off_field[1,]
  seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
  seed_soil_data_off_field$day<-0:150
  seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
  
  #first, get the concentration from the average deposition
  conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95)*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(seed_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
   pwc[126:276,]
  } else {
     pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-pwc$soil_concentration_ug_g
  
for(i in 2:nrow(seed_soil_data_off_field)){
    seed_soil_data_off_field[i,7]<-seed_soil_data_off_field[i,7] + seed_soil_data_off_field[i-1,7]*exp(0-seed_soil_data_off_field[i,4] * seed_soil_data_off_field[i,6]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed <- conc_from_dust + seed_soil_data_off_field$Soil_concentration_ug_g_from_seed
  daily_conc_off_field[[compound_and_crop]]<-seed_soil_data_off_field[,c(1,2,3,6,7)] #add compound
  
}


#first put OG names 
names(daily_conc_off_field)<-names(seed_soil_datasets)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
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
seed_treatments<-merge(seed_treatments, part_coeff, by="Compound")
seed_nectarpollen_conc<-merge(seed_treatments,Li) #merge with the fraction estimated via Krupke

seed_nectarpollen_conc<-seed_nectarpollen_conc[!(seed_nectarpollen_conc$Commodity == "CORN" & seed_nectarpollen_conc$Type == "Nectar"),]

##because we have both nectar and pollen, we need to split up the sets into two batches
seed_treatments_pollen<-seed_nectarpollen_conc[seed_nectarpollen_conc$Type == "Pollen",]
seed_treatments_nectar<-seed_nectarpollen_conc[seed_nectarpollen_conc$Type == "Nectar",]


## Pollen
#split by compound, crop, and type (nectar or pollen)
seed_pollen_datasets<-split(seed_treatments_pollen, list(seed_treatments_pollen$Compound, seed_treatments_pollen$ApplicationType, seed_treatments_pollen$Commodity), drop=T)

residues_in_nectar_and_pollen_from_seed<-function(x){

# get the deposition and soil concentrations first
  seed_soil_data_off_field<- x[!x$d == 0,]
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed) # get average dust deposition
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  seed_soil_data_off_field<-seed_soil_data_off_field[1,]
  seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
  seed_soil_data_off_field$day<-0:150
  # seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
  
  #first, get the concentration from the average deposition
  conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95)#*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(seed_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-pwc$soil_concentration_ug_g
 
  for(i in 2:nrow(seed_soil_data_off_field)){
    seed_soil_data_off_field[i,39]<-seed_soil_data_off_field[i,39] + seed_soil_data_off_field[i-1,39] #*exp(0-seed_soil_data_off_field[i,11] * seed_soil_data_off_field[i,38]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-conc_from_dust + seed_soil_data_off_field$Soil_concentration_ug_g_from_seed
  
  
  output<- matrix(data=0, nrow=151, ncol=2)
  #n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-1:150
  output[2:151,2]<-t
  for(i in 1:nrow(output)){
    # units will be ug per g of pollen
    # first get uptake from soil
    pol<-seed_soil_data_off_field[i,]
    uptakesoil <-( (pol$`Kp-L`* pol$Soil_concentration_ug_g_from_seed) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
 
    #then estimate RUD due to leaf deposition
    uptakeleaf<-( ((pol$`Kp-L`*pol$fsurface*pol$cf1)/(pol$LAI*pol$LMA*(1/(1-pol$Wleaf))*1000)) * (pol$KupSurface/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSurface)) * ( exp(-pol$KdissSurface*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) * pol$Dust_concentration_ug_m2_from_seed
    output[i,1]<- (uptakesoil + uptakeleaf) 
    
  }
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(pol$Type,'_concentration_ug_g_from_seed'),"day")

  output
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(seed_pollen_datasets,residues_in_nectar_and_pollen_from_seed)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
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

residues_in_nectar_and_pollen_from_seed<-function(x){
  
  # get the deposition and soil concentrations first
  seed_soil_data_off_field<- x[!x$d == 0,]
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed) # get average dust deposition
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  seed_soil_data_off_field<-seed_soil_data_off_field[1,]
  seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
  seed_soil_data_off_field$day<-0:150
  # seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
  
  #first, get the concentration from the average deposition
  conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95) #*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(seed_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-pwc$soil_concentration_ug_g
  
  for(i in 2:nrow(seed_soil_data_off_field)){
    seed_soil_data_off_field[i,39]<-seed_soil_data_off_field[i,39] + seed_soil_data_off_field[i-1,39] #*exp(0-seed_soil_data_off_field[i,11] * seed_soil_data_off_field[i,38]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-conc_from_dust + seed_soil_data_off_field$Soil_concentration_ug_g_from_seed

  
  output<- matrix(data=0, nrow=151, ncol=2)
  #n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-1:150
  output[2:151,2]<-t
  for(i in 1:nrow(output)){
    # units will be ug per g of pollen
    # first get uptake from soil
    nec<-seed_soil_data_off_field[i,]
    uptakesoil <-( (nec$`Kn-L` * nec$Soil_concentration_ug_g_from_seed) * (nec$KupSoil/(nec$KelAir+nec$KelDeg+nec$KelGrow - nec$KdissSoil)) * (exp(-nec$KdissSurface*(i)) - exp(-(nec$KelAir+nec$KelDeg+nec$KelGrow)*i)) ) 
    
    #then estimate RUD due to leaf deposition
    uptakeleaf<-( ((nec$`Kn-L`*nec$fsurface*nec$cf1)/(nec$LAI*nec$LMA*(1/(1-nec$Wleaf))*1000)) * (nec$KupSurface/(nec$KelAir+nec$KelDeg+nec$KelGrow - nec$KdissSurface)) * ( exp(-nec$KdissSurface*(i)) - exp(-(nec$KelAir+nec$KelDeg+nec$KelGrow)*i)) ) * nec$Dust_concentration_ug_m2_from_seed
    output[i,1]<- (uptakesoil + uptakeleaf)
    
  }
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(nec$Type,'_concentration_ug_g_from_seed'),"day")
  
  output
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(seed_nectar_datasets,residues_in_nectar_and_pollen_from_seed)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
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


#### Soil Applications----
##Soil----
#get the seed treatment data and estimate soil concentration in dust for each seed type
soil_treatments<-apprates[apprates$ApplicationType == "Soil",]
soil_treatments<-soil_treatments[,!names(soil_treatments) %in% "Type"]
soil_treatments$kg_ha<-soil_treatments$AvgRate*1.12085 #convert original application rate to kg_ha for the soil uptake compartment 
soil_soil_datasets<-split(soil_treatments, list(soil_treatments$Compound, soil_treatments$ApplicationType,soil_treatments$Commodity), drop=T)


#get PWC set for seed
pwc_set<-PWC_data_list[grepl('soil', names(PWC_data_list))]
names(pwc_set)<-toupper(names(pwc_set))


#here, we will estimate the concentration in ug per g, using both dust and amount contained in soil
daily_conc_off_field<-list()
for(compound_and_crop in 1:length(soil_soil_datasets)){
  soil_soil_data_off_field<-soil_soil_datasets[[compound_and_crop]]
 
  #we can drop all rows except 1 because we've averaged the deposition
  soil_soil_data_off_field<-soil_soil_data_off_field[rep(seq_len(nrow(soil_soil_data_off_field)), 151),]
  soil_soil_data_off_field$day<-0:150
  soil_soil_data_off_field<-soil_soil_data_off_field[,c(1,4,5,11,13)]
  
   
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(soil_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  soil_soil_data_off_field$Soil_concentration_ug_g_from_soil<-pwc$soil_concentration_ug_g
  
  for(i in 2:nrow(soil_soil_data_off_field)){
    soil_soil_data_off_field[i,6]<-soil_soil_data_off_field[i,6] + soil_soil_data_off_field[i-1,6]*exp(0-soil_soil_data_off_field[i,4] * soil_soil_data_off_field[i,5]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff

  daily_conc_off_field[[compound_and_crop]]<-soil_soil_data_off_field[,c(1,2,3,5,6)] #add compound
  
}


#first put OG names 
names(daily_conc_off_field)<-names(soil_soil_datasets)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
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
#get the seed treatment data and estimate soil concentration in dust for each seed type
soil_treatments<-apprates[apprates$ApplicationType == "Soil",]
soil_treatments<-soil_treatments[,!names(soil_treatments) %in% "Type"]
soil_treatments$kg_ha<-soil_treatments$AvgRate*1.12085 #convert original application rate to kg_ha for the soil uptake compartment 
soil_pollen_datasets<-split(soil_treatments, list(soil_treatments$Compound, soil_treatments$ApplicationType,soil_treatments$Commodity), drop=T)


#get PWC set for seed
pwc_set<-PWC_data_list[grepl('soil', names(PWC_data_list))]
names(pwc_set)<-toupper(names(pwc_set))


x<- soil_soil_datasets[[1]]
 residues_in_nectar_and_pollen_from_soil<-function(x){

  #we can drop all rows except 1 because we've averaged the deposition
  soil_pollen_data_off_field<-x[rep(seq_len(nrow(x)), 151),]
  soil_pollen_data_off_field$day<-0:150
  soil_pollen_data_off_field<-soil_pollen_data_off_field[,c(1,4,5,11,13)]
  
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(soil_pollen_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  soil_pollen_data_off_field$Soil_concentration_ug_g_from_soil<-pwc$soil_concentration_ug_g
  
  for(i in 2:nrow(soil_pollen_data_off_field)){
    soil_pollen_data_off_field[i,6]<-soil_pollen_data_off_field[i,6] + soil_pollen_data_off_field[i-1,6]
  }
  
  
  output<- matrix(data=0, nrow=151, ncol=2)
  t<-1:150
  output[2:151,2]<-t
  for(i in 1:nrow(output)){
    # units will be ug per g of pollen
    # first get uptake from soil
    pol<-soil_pollen_data_off_field[i,]
    uptakesoil <-( (pol$`Kp-L`* pol$Soil_concentration_ug_g_from_seed) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
    
    output[i,1]<- uptakesoil 
    
  }
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(pol$Type,'_concentration_ug_g_from_seed'),"day")
  
  output
  
}

#### Foliar Applications----

