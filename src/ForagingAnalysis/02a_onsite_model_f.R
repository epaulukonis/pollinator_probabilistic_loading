### Foraging Landscape

### 02a Initial Onsite Model Outputs

# Edited by E. Paulukonis Feb 2024

print("stepping into 02: onsite modeling")
#Code for modeling on-site field concentrations from applications in 4 media types
library(stringdist)
library(cowplot)
library(tidyverse)
library(ggh4x)
##### Read in data for models ----
#### App rates and timing:
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)


#### Hazard quotient info
beetox <-read.csv(paste0(pest_dir, "/BeeTox.csv"))
beetox$Compound<-toupper(beetox$Compound)


#### Set up and read in variables for models:
## Li
Li<-read.csv(paste0(pest_dir,"/Models/LiParam.csv"))
Li<-Li %>% spread(Parameter, Value)
Li$Compound<-toupper(Li$Compound)

#write.csv(Li, file = paste0(pest_dir,"/Models/LiParams_ForTable.csv"), row.names = FALSE)

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
Fpl<-c(0.35,0.69, 0.69,0.69)
Fnl<-c(0.017,0.05, 0.05,0.05)
partcoeffn<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM","BIFENTHRIN"), (Fnl),paste0("Nectar")))
names(partcoeffn)<-c("Compound","Fr","Type")
partcoeffp<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM","BIFENTHRIN"), (Fpl),paste0("Pollen")))
names(partcoeffp)<-c("Compound","Fr","Type")
part_coeff<-rbind(partcoeffp, partcoeffn)

## Nectar and Pollen assignments, if needed
Compound<-c("IMIDACLOPRID","IMIDACLOPRID","CHLORPYRIFOS","CHLORPYRIFOS","CHLORPYRIFOS", "CARBOFURAN","CARBARYL", "CARBARYL","CARBARYL", "BIFENTHRIN","BIFENTHRIN","BIFENTHRIN","GLYPHOSATE","GLYPHOSATE","GLYPHOSATE")
Type<-c("Pollen","Nectar","Pollen","Nectar","Pollen","Pollen","Pollen","Nectar","Pollen","Pollen","Nectar","Pollen","Pollen","Nectar","Pollen")
Commodity<-c("SOYBEANS","SOYBEANS","SOYBEANS","SOYBEANS","CORN","CORN","SOYBEANS","SOYBEANS","CORN","SOYBEANS","SOYBEANS","CORN","SOYBEANS","SOYBEANS","CORN")

foliar_type<-as.data.frame(cbind(Compound,Type,Commodity))

#### Set up empty matrix to hold outputs; these will auto-populate as you go through each application type
#first, we will create our empty df to populate the on-field concentrations by day
on_field_residues<-as.data.frame(matrix(NA,nrow = 151, ncol = 0))
on_field_residues[,1]<-0:150
colnames(on_field_residues)[1]<-"day"

#put all combos into a list where the name represents the scenario
on_field_residue_list<-rep(list(on_field_residues), 18L)
unique_names<-apprates %>% distinct(Compound,ApplicationType, Commodity)
names(on_field_residue_list)<-with(unique_names, paste0(Compound,"_",ApplicationType,"_",Commodity))


######## Analysis ###########
#### Seed Applications ----
## Dust ----
#these are the only residues we'll measure as ug/m2 
#First, we will derive the fraction of dust moving across and off the field 
d<-1:299*0.3048 # first, convert the distance to meters
x<-exp(1.68-0.0905*log(d))-1 # estimate the fraction 
orig_conc_at_distance<-(x/1875)*1000 #ng/mm2 to ug/m2 is a 1000 unit conversion


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

daily_conc_on_field<-list()
for(compound_and_crop in 1:length(seed_dust_datasets)){
  seed_dust_data<-seed_dust_datasets[[compound_and_crop]]
  seed_dust_data$Dust_concentration_ug_m2_from_seed<-seed_dust_data$dust_drift_conc #retain as ug/m2
  seed_dust_datasets[[compound_and_crop]]<-seed_dust_data
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  seed_dust_data_on_field<-seed_dust_data[seed_dust_data$d == 0,]
  seed_dust_data_on_field<-seed_dust_data_on_field[rep(seq_len(nrow(seed_dust_data_on_field)), 151),]
  seed_dust_data_on_field[2:151,18]<-0
  seed_dust_data_on_field$day<-0:150
  daily_conc_on_field[[compound_and_crop]]<-seed_dust_data_on_field[,c(1,6,18,19)]
  
}

#first put OG names 
names(daily_conc_on_field)<-names(seed_dust_datasets)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)



## Soil----
#application rate per cm3, at mixing depth; calculate 50% of OG into soil, then subtract flux and runoff values, and then estimate remaining ug/g 
#plus dust soil conc above...

#get the seed treatment data and estimate soil concentration in dust for each seed type
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]
seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_soil_conc<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_soil_conc<-merge(seed_soil_conc, Li[ , c("Compound","KdissSoil")], by = "Compound", all.x=TRUE) # join specifically to get dissipation rate of compound in soil
seed_soil_conc<-seed_soil_conc[with(seed_soil_conc, order(Compound,Commodity)), ] # order by compound, commodity
seed_soil_conc$dust_drift_conc<-seed_soil_conc$ug_m2*seed_soil_conc$deposition #multiply application rate times deposition curve 


#split each set of commodity and compound data into a unique dataframe
seed_soil_datasets<-split(seed_soil_conc, list(seed_soil_conc$Compound, seed_soil_conc$Commodity), drop=T) 

#here, we will estimate the concentration in ug per g, using both dust and amount contained in soil
daily_conc_on_field<-list()
for(compound_and_crop in 1:length(seed_soil_datasets)){
  seed_soil_data_on_field<-seed_soil_datasets[[compound_and_crop]]
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  seed_soil_data_on_field<-seed_soil_data_on_field[seed_soil_data_on_field$d == 0,]
  seed_soil_data_on_field<-seed_soil_data_on_field[rep(seq_len(nrow(seed_soil_data_on_field)), 151),]
  seed_soil_data_on_field$day<-0:150
  
  seed_soil_data_on_field$soil_concentration<-seed_soil_data_on_field$dust_drift_conc * (1/10000) #convert to ug/cm2
  
  conc_from_dust<-(seed_soil_data_on_field$soil_concentration/2 * 1/1.95)*exp(-(seed_soil_data_on_field$k_values*seed_soil_data_on_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  conc_from_application<-( ((0.368*100) /(1.95*0.05*1000))*exp(0-seed_soil_data_on_field$k_values * seed_soil_data_on_field$day)) *seed_soil_data_on_field$AvgRate*1.12085 #now add that amount to the amount in soil using Li (converting AR to kg/ha)estimate for soil concentration. I modified mass fraction to be 40%, and soil bulk density to be 1.44 to match 
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment 
  seed_soil_data_on_field$Soil_concentration_ug_g_from_seed<-conc_from_dust+conc_from_application
  
  daily_conc_on_field[[compound_and_crop]]<-seed_soil_data_on_field[,c(1,6,19,21)] #add compound
  
}

#first put OG names 
names(daily_conc_on_field)<-names(seed_soil_datasets)


#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)


## Pollen and Nectar----
#merge seed treatments and partitioning coefficients
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]
seed_treatments<-seed_treatments[,!names(seed_treatments) %in% "Type"]
seed_treatments<-merge(seed_treatments, part_coeff, by="Compound")

seed_treatments$kg_ha<-seed_treatments$AvgRate*1.12085 #convert original application rate to kg_ha for the soil uptake compartment 
#merge with type of residue (pollen/nectar)
seed_treatments<-merge(seed_treatments,Li) 

#remove nectar for corn
seed_treatments<-seed_treatments[!(seed_treatments$Commodity == "CORN" & seed_treatments$Type == "Nectar"),]

#add in variables
seed_treatments$rgr<-ifelse(seed_treatments$Commodity =="CORN",rgr_corn,rgr_soy) #relative growth rate
seed_treatments$mass<-ifelse(seed_treatments$Commodity =="CORN",m0_corn,m0_soy) #mass of seed

# use conversion and seeding rates to mg ai per seed
seed_treatments$mg_ai_seed<-ifelse(seed_treatments$Commodity == "CORN", seed_treatments$AvgRate*(1/2.20462e-6)*(1/29000),  seed_treatments$AvgRate*(1/2.20462e-6)*(1/60000)) 

##because we have both nectar and pollen, we need to split up the sets into two batches
seed_treatments_pollen<-seed_treatments[seed_treatments$Type == "Pollen",]
seed_treatments_nectar<-seed_treatments[seed_treatments$Type == "Nectar",]


## Pollen
#split by compound, crop, and type (nectar or pollen)
seed_pollen_datasets<-split(seed_treatments_pollen, list(seed_treatments_pollen$Compound, seed_treatments_pollen$ApplicationType,seed_treatments_pollen$Commodity), drop=T)

#x<-seed_pollen_datasets[[2]]
residues_in_nectar_and_pollen_from_seed<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  
  TSCF<-ifelse(x$Commodity == "CORN", 0.784*exp(-((x$logKow-1.78)^2/2.44)), 0.7*exp(-((x$logKow-3.07)^2/2.44))   )
  
  for(i in (n+1):nrow(output)){
    # we have two components: a concentration in plant as growth occurs, and an uptake from soil aspect
    conc_in_plant <-( (x$mg_ai_seed *0.20/x$mass)*TSCF*exp(-(kel_grow+x$rgr)*output[i,2])*as.numeric(x$Fr) )*1000 # units will be ug per g of pollen; the 1000 converts mg to ug
    rud_from_soil_uptake<-(  ((x$`Kp-L`*x$fsoil*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(-x$KdissSoil*(output[i,2])) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(output[i,2]))) ) 
    conc_from_soil_uptake<- rud_from_soil_uptake* x$kg_ha
    output[i,1]<- conc_in_plant +  conc_from_soil_uptake
  }
  output<-as.data.frame(output)
  names(output)<-c(paste0(x$Type,'_concentration_ug_g_from_seed'),"day")
  
  output
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_on_field<-lapply(seed_pollen_datasets,residues_in_nectar_and_pollen_from_seed)


#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)



## Nectar
#split by compound, crop, and type (nectar or pollen)
seed_nectar_datasets<-split(seed_treatments_nectar, list(seed_treatments_nectar$Compound, seed_treatments_nectar$ApplicationType,seed_treatments_nectar$Commodity), drop=T)

residues_in_nectar_and_pollen_from_seed<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  
  TSCF<-ifelse(x$Commodity == "CORN", 0.784*exp(-((x$logKow-1.78)^2/2.44)), 0.7*exp(-((x$logKow-3.07)^2/2.44))   )
  
  for(i in (n+1):nrow(output)){
    # we have two components: a concentration in plant as growth occurs, and an uptake from soil aspect
    conc_in_plant <-( (x$mg_ai_seed *0.20/x$mass)*TSCF*exp(-(kel_grow+x$rgr)*output[i,2])*as.numeric(x$Fr) )*1000 # units will be ug per g of pollen; the 1000 converts mg to ug
    rud_from_soil_uptake<-(  ((x$`Kn-L`*x$fsoil*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(-x$KdissSoil*(output[i,2])) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(output[i,2]))) ) 
    conc_from_soil_uptake<- rud_from_soil_uptake* x$kg_ha
    output[i,1]<- conc_in_plant +  conc_from_soil_uptake
  }
  output<-as.data.frame(output)
  names(output)<-c(paste0(x$Type,'_concentration_ug_g_from_seed'),"day")
  
  output
  
}

#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_on_field<-lapply(seed_nectar_datasets,residues_in_nectar_and_pollen_from_seed)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)


#visualize the original concentration, if desired
# p <- ggplot(testx, aes(days, residues, group=interaction(Compound,Commodity), color=interaction(Compound,Commodity))) +
#   geom_point()+
#   geom_line()+
#   facet_wrap(~Type)+
#   ylab("Residues in Media [ug/g]")+
#   xlab("Time [days]]")+
#   guides(color=guide_legend(title="Compound and Crop"))+
#   theme_bw()
# p




#### Soil Applications ----
## Soil----
#merge seed treatments and partitioning coefficients
soil_treatments<-apprates[apprates$ApplicationType == "Soil",]
soil_treatments<-soil_treatments[,!names(soil_treatments) %in% "Type"]
#get the nectar/pollen types from foliar df
soil_treatments<-merge(x=soil_treatments,y=foliar_type[,c("Compound","Commodity","Type")],by=c("Compound","Commodity"), all.x=T)
soil_treatments<-merge(soil_treatments,Li) 
soil_treatments$kg_ha<-soil_treatments$AvgRate*1.12085 #convert original application rate to kg_ha for the soil uptake compartment 

#split into df with pollen or nectar
soil_treatments_pollen<-soil_treatments[soil_treatments$Type == "Pollen",]
soil_treatments_nectar<-soil_treatments[soil_treatments$Type == "Nectar",]

soil_soil_datasets<-split(soil_treatments, list(soil_treatments$Compound, soil_treatments$ApplicationType, soil_treatments$Commodity, soil_treatments$Type), drop=T)

# 
# residues_in_soil_from_soil_briggsryan<-function(x){
#   output<- matrix(data=0, nrow=151, ncol=2)
#   t<-0:150
#   output[1:151,2]<-t
# 
#   for(i in 1:nrow(output)){
#     output[i,1] <- (x$ug_cm2*(1/15))*(1/1.44)*exp(-(x$k_values*output[i,2]))  # units will be ug per g of soil, depth of 15 cm
#   }
#   output<-as.data.frame(output)
#   output[,3]<-x$Compound # add compound
#   output[,4]<-x$Commodity
#   output<-output[,c(2,3,4,1)]
#   names(output)<-c("day","Compound","Commodity",'Soil_concentration_ug_g_from_soil')
#   output
# 
# }
# 
# daily_conc_on_fieldrb<-lapply(soil_soil_datasets,residues_in_soil_from_soil_briggsryan)


residues_in_soil_from_soil_adhoc<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  t<-0:150
  output[1:151,2]<-t
  
  for(i in 1:nrow(output)){
    
    soil_RUD <-  (0.40*100) /(1.95*0.05*1000) *exp(0-x$k_values*output[i,2])  # units will be ug per g of soil, depth of 5 cm; we convert lb/acre to kg/ha here. we also assume 40% goes to soil.
    soil_conc<-soil_RUD * x$AvgRate*1.12085
    
    output[i,1] <- soil_conc
  }
  output<-as.data.frame(output)
  output[,3]<-x$Compound # add compound
  output[,4]<-x$Commodity
  output<-output[,c(2,3,4,1)]
  names(output)<-c("day","Compound","Commodity",'Soil_concentration_ug_g_from_soil')
  output
  
}

daily_conc_on_field<-lapply(soil_soil_datasets,residues_in_soil_from_soil_adhoc)


#quick comparison of the methods, ryan-briggs and adhoc

# rb<-daily_conc_on_fieldrb[[1]]
# ah<-daily_conc_on_fieldah[[1]]
# 
# rb$Method<-"RyanBriggs"
# ah$Method<-"AdHoc"
# 
# soil_methods<-rbind(rb,ah)
# soil<-ggplot(soil_methods, aes(day, Soil_concentration_ug_g_from_soil,  color=Method)) +
#   geom_point(aes(shape=Commodity))+
#   ylab("Residues")+
#   xlab("Day")+
#   theme_bw()


#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)


## Pollen and Nectar----

#let's get the average partitioning coefficient for pollen; we don't have info about it for bifenthrin, so we'll average the data for neonics
# PC<-part_coeff[part_coeff$Type == "Pollen",]
# PC<-mean(as.numeric(PC$Fr))
# 

#note: we only have one compound for soil, but theoretically if you had more you could add the koc and logkow to the apprates df instead of manually assigning them
# residues_in_pollen_from_soil_briggsryan<-function(x){
#   output<- matrix(data=0, nrow=151, ncol=2)
#   n<-ifelse(x$Commodity == "CORN", 71,61)
#   t<-(n-1):150
#   output[n:151,2]<-t
#   
# 
#   for(i in n:nrow(output)){
#     logKow<-7.14
#     # TSCF<-(-0.0648)*(logKow^2)+0.241*logKow+0.5822
#     # TSCF
#     tscf<-pfm::TSCF(logKow, method = c("dettenmaier09"))
# 
#     koc<-2.24E+5
#     soil_conc<- (x$ug_cm2*(1/15))*(1/1.44)*exp(-(x$k_values*output[i,2]))
#     output[i,1] <- ( soil_conc * (10^(0.95*logKow-2.05)+0.82) * tscf *(bulkdensity/(theta+bulkdensity*koc*foc)) )* PC # units will be ug per g of pollen
#        
#    }                                                                          
#   output<-as.data.frame(output)
#   output[,3]<-x$Compound # add compound
#   output[,4]<-x$Commodity
#   output<-output[,c(2,3,4,1)]
#   names(output)<-c("day","Compound","Commodity",'Pollen_concentration_ug_g_from_soil')
#   output
#   
# }
# 
# 
# daily_conc_on_fieldrb<-lapply(soil_soil_datasets,residues_in_pollen_from_soil_briggsryan)

soil_pollen_datasets<-split(soil_treatments_pollen, list(soil_treatments_pollen$Compound, soil_treatments_pollen$ApplicationType, soil_treatments_pollen$Commodity), drop=T)
soil_nectar_datasets<-split(soil_treatments_nectar, list(soil_treatments_nectar$Compound, soil_treatments_nectar$ApplicationType, soil_treatments_nectar$Commodity), drop=T)


## Pollen
residues_in_pollen_from_soil_adhoc<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  
  for(i in (n+1):nrow(output)){
    rud_from_soil_uptake<- ((x$`Kp-L`*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * ( exp(-x$KdissSoil*(output[i,2])) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(output[i,2]))  ) 
    conc_from_soil_uptake<- rud_from_soil_uptake * x$kg_ha
    output[i,1]<-conc_from_soil_uptake
  }
  
  #only one decay term
  output<-as.data.frame(output)
  output[,3]<-x$Compound # add compound
  output[,4]<-x$Commodity
  output<-output[,c(2,3,4,1)]
  names(output)<-c("day","Compound","Commodity",'Pollen_concentration_ug_g_from_soil')
  output
  
}


daily_conc_on_field<-lapply(soil_pollen_datasets,residues_in_pollen_from_soil_adhoc)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)



## Nectar
residues_in_nectar_from_soil_adhoc<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  
  for(i in (n+1):nrow(output)){
    rud_from_soil_uptake<- ((x$`Kn-L`*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * ( exp(-x$KdissSoil*(output[i,2])) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(output[i,2]))  ) 
    conc_from_soil_uptake<- rud_from_soil_uptake * x$kg_ha
    output[i,1]<-conc_from_soil_uptake
  }
  
  #only one decay term
  output<-as.data.frame(output)
  output[,3]<-x$Compound # add compound
  output[,4]<-x$Commodity
  output<-output[,c(2,3,4,1)]
  names(output)<-c("day","Compound","Commodity",'Nectar_concentration_ug_g_from_soil')
  output
  
}


daily_conc_on_field<-lapply(soil_nectar_datasets,residues_in_nectar_from_soil_adhoc)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)




#### Foliar Applications ----
## Air ----
#these are the only residues we'll measure as ug/m2
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))

#convert to meters
ag_drift$distance<-ag_drift$distance*0.3048

# #visualize the original concentration, if desired
# p <- ggplot(ag_drift, aes(distance,pond_ground_high_vf2f)) +
#   geom_point()+
#   geom_line()+
#   ylab("Deposition (Fraction of In-Field Concentration)")+
#   xlab("Distance[m]")+
#   theme_bw()+
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=14,face="bold"))
# p
# 


#this is optional code to fit the curve using nonlinear least squares; we will just use the raw curve from agdrift here
# fit<-nls(pond_ground_high_vf2f ~ SSasymp(distance, yf, y0, decrate), data = ag_drift)
# summary(fit)
# ag_drift$pred<-predict(fit)
# 
# data<-ag_drift[,c(1,5)]
# names(data)<-c("x","y")
# 
# fo3 <- y ~ 1/(b + x^c) # omit .lin parameter; plinear will add it automatically
# fm3 <- nls(fo3, data = data, start = list(b = 1, c = 1), alg = "plinear")
# 
# 
# 
# Plot <- function(data, fm, main) {
#   plot(y ~ x, data, pch = 20)
#   lines(fitted(fm) ~ x, data, col = "red")
#   legend("topright", bty = "n", cex = 0.7, legend = capture.output(fm))
#   title(main = paste(main, "- AIC:", round(AIC(fm), 2)))
# }  
# 
# Plot(data, fm3, "3 parameters")
# 
# 
# 
# p <- ggplot(ag_drift, aes(distance,pred)) +
#   geom_point()+
#   geom_line()+
#   geom_point(aes(distance,pond_ground_high_vf2f), col='red')+
#   geom_line(aes(distance,pond_ground_high_vf2f), col='red')
# 
# p

agdrift_data<-ag_drift[,c(1,5)] #use ground spray, very fine to fine


#get the seed treatment data and estimate concentration in dust for each seed type
foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
foliar_air_conc<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
foliar_air_conc<-foliar_air_conc[with(foliar_air_conc, order(Compound,Commodity)), ] # order by compound, commodity
#foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_aerial_vf2f
foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_ground_high_vf2f #multiple application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
foliar_air_datasets<-split(foliar_air_conc, list(foliar_air_conc$Compound,foliar_air_conc$ApplicationType, foliar_air_conc$Commodity), drop=T) 

#here, we will estimate the concentration in ug per g

daily_conc_on_field<-list()
for(compound_and_crop in 1:length(foliar_air_datasets)){
  #compound_and_crop<-1
  foliar_air_data<-foliar_air_datasets[[compound_and_crop]]
  foliar_air_data$Air_concentration_ug_m2_from_foliar<-foliar_air_data$air_drift_conc #retain as ug/m2
  foliar_air_datasets[[compound_and_crop]]<-foliar_air_data
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  foliar_air_data_on_field<-foliar_air_data[foliar_air_data$d == 0,]
  foliar_air_data_on_field<-foliar_air_data_on_field[rep(seq_len(nrow(foliar_air_data_on_field)), 151),]
  
  #foliar applications occur 49 days after planting for soybeans, and 60 days after planting for corn
  # if (grepl('SOYBEANS',names(foliar_air_datasets[compound_and_crop]))){ 
  #   foliar_air_data_on_field[c(1:48,50:151),17]<-0
  # } else { 
  #   foliar_air_data_on_field[c(1:59,61:151),17]<-0} 
  
  foliar_air_data_on_field[2:151,17]<-0
  foliar_air_data_on_field$day<-0:150
  daily_conc_on_field[[compound_and_crop]]<-foliar_air_data_on_field[,c(1,6,17,18)]
  
}

#first put OG names 
names(daily_conc_on_field)<-names(foliar_air_datasets)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)



## Soil----
#get the seed treatment data and estimate concentration in dust for each seed type
foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
foliar_soil_conc<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
foliar_soil_conc<-foliar_soil_conc[with(foliar_soil_conc, order(Compound,Commodity)), ] # order by compound, commodity
#foliar_soil_conc$air_drift_conc<-foliar_soil_conc$ug_m2*foliar_soil_conc$pond_aerial_vf2f
foliar_soil_conc$air_drift_conc<-foliar_soil_conc$ug_m2*foliar_soil_conc$pond_ground_high_vf2f #multiple application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
foliar_soil_datasets<-split(foliar_soil_conc, list(foliar_soil_conc$Compound,foliar_soil_conc$ApplicationType, foliar_air_conc$Commodity), drop=T) 


#here, we will estimate the concentration in ug per g
daily_conc_on_field<-list()
for(compound_and_crop in 1:length(foliar_soil_datasets)){
  
  foliar_soil_data_on_field<-foliar_soil_datasets[[compound_and_crop]]
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  foliar_soil_data_on_field<-foliar_soil_data_on_field[seed_dust_data$d == 0,]
  foliar_soil_data_on_field<-foliar_soil_data_on_field[rep(seq_len(nrow(foliar_soil_data_on_field)), 151),]
  
  
  foliar_soil_data_on_field$day<-0:150
  foliar_soil_data_on_field$soil_concentration<- foliar_soil_data_on_field$air_drift_conc * (1/10000) #convert to ug/cm2
  conc_from_drift<- (foliar_soil_data_on_field$soil_concentration/2 * 1/1.95) *exp(-(foliar_soil_data_on_field$k_values*foliar_soil_data_on_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  conc_from_application<-( ((0.368*100) /(1.95*0.05*1000))*exp(0-foliar_soil_data_on_field$k_values * foliar_soil_data_on_field$day)) *foliar_soil_data_on_field$AvgRate*1.12085 #now add that amount to the amount in soil using Li (converting AR to kg/ha)estimate for soil concentration. I modified mass fraction to be 40%, and soil bulk density to be 1.44 to match 
  
  #here we combine the deposition from drift with the concentration already in the soil from the foliar treatment 
  foliar_soil_data_on_field$Soil_concentration_ug_g_from_foliar<-conc_from_dust+conc_from_application
  
  # if (grepl('SOYBEANS',names(foliar_soil_datasets[compound_and_crop]))){ 
  #    foliar_soil_data_on_field$day<-49:199
  #    } else { 
  #      foliar_soil_data_on_field$day<-60:210
  #        } 
  
  daily_conc_on_field[[compound_and_crop]]<- foliar_soil_data_on_field[,c(1,6,17,19)]
  
  
}


#first put OG names 
names(daily_conc_on_field)<-names(foliar_soil_datasets)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)

#testy<-on_field_residue_list[[4]]

## Nectar and Pollen----
foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
foliar_treatments$kg_ha<-foliar_treatments$AvgRate*1.12085 # convert to kg/ha for this one; we will convert to ug per g in a bit; this matches Li
foliar_treatments<-foliar_treatments[,!names(foliar_treatments) %in% "Type"]
foliar_treatments<-merge(foliar_type, foliar_treatments)  #merge with type of residue (pollen/nectar)
foliar_nectarpollen_conc<-merge(foliar_treatments,Li) #merge with the fraction estimated via Krupke


##because we have both nectar and pollen, we need to split up the sets into two batches
foliar_treatments_pollen<-foliar_nectarpollen_conc[foliar_nectarpollen_conc$Type == "Pollen",]
foliar_treatments_nectar<-foliar_nectarpollen_conc[foliar_nectarpollen_conc$Type == "Nectar",]


## Pollen
#split by compound, crop, and type (nectar or pollen)
foliar_pollen_datasets<-split(foliar_treatments_pollen, list(foliar_treatments_pollen$Compound, foliar_treatments_pollen$ApplicationType,foliar_treatments_pollen$Commodity), drop=T)

daily_conc_on_field<-list()
#x<-foliar_pollen_datasets[[10]]

# If running for scenarios:
# residues_in_nectar_and_pollen_from_foliar<-function(x){
#   output<- matrix(data=0, nrow=151, ncol=2)
#   n<-1
#   t<-n:150
#   output[(n+1):151,2]<-t
#   for(i in 7:nrow(output)){
#     # units will be ug per g of pollen
# 
#     pollen_rud <-( ((x$`Kp-L`*x$fsoil*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(-x$KdissSoil*(i)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i))) )  +
# 
#                  ( ((x$`Kp-L`*x$fsurface*x$cf1)/(x$LAI*x$LMA*(1/(1-x$Wleaf))*1000)) * (x$KupSurface/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSurface)) * ( exp(-x$KdissSurface*(i)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i))) )
# 
#     output[i,1]<-(pollen_rud*x$kg_ha)
# 
#   }
#   output<-as.data.frame(output)
# 
#   names(output)<-c(paste0(x$Type,'_concentration_ug_g_from_foliar'),"day")
#   output
# 
# }

#x<-foliar_pollen_datasets[[3]]
# If running for figures:
residues_in_nectar_and_pollen_from_foliar<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  for(i in (n+1):nrow(output)){
    #i<-n+1
    # units will be ug per g of pollen
    pollen_rud <-( (x$`Kp-L`*x$fsoil*x$cf1/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(0-x$KdissSoil*(i-n+6)) - exp(0-(x$KelAir+x$KelDeg+x$KelGrow)*(i-n+6))) )  +
      
      ( (x$`Kp-L`*x$fsurface*x$cf1/(x$LAI*x$LMA*(1/(1-x$Wleaf))*1000)) * (x$KupSurface/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSurface)) * ( exp(-x$KdissSurface*(i-n+6)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i-n+6))) )
    
    output[i,1]<-(pollen_rud*x$kg_ha)
  }
  
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(x$Type,'_concentration_ug_g_from_foliar'),"day")
  output
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_on_field<-lapply(foliar_pollen_datasets,residues_in_nectar_and_pollen_from_foliar)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)

## Nectar
#split by compound, crop, and type (nectar or pollen)
foliar_nectar_datasets<-split(foliar_treatments_nectar, list(foliar_treatments_nectar$Compound, foliar_treatments_nectar$ApplicationType,foliar_treatments_nectar$Commodity), drop=T)

daily_conc_on_field<-list()
#x<-foliar_nectar_datasets[[1]]

# # If running for scenarios:
# residues_in_nectar_and_pollen_from_foliar<-function(x){
#   output<- matrix(data=0, nrow=151, ncol=2)
#   n<-1
#   t<-n:150
#   output[(n+1):151,2]<-t
#   for(i in 7:nrow(output)){
#     # units will be ug per g of pollen
#     pollen_rud <-( ((x$`Kn-L`*x$fsoil*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(-x$KdissSoil*(i)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i))) )  +
# 
#       ( ((x$`Kn-L`*x$fsurface*x$cf1)/(x$LAI*x$LMA*(1/(1-x$Wleaf))*1000)) * (x$KupSurface/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSurface)) * ( exp(-x$KdissSurface*(i)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i))) )
# 
#     output[i,1]<-(pollen_rud*x$kg_ha)
# 
#   }
#   output<-as.data.frame(output)
#   names(output)<-c(paste0(x$Type,'_concentration_ug_g_from_foliar'),"day")
#   output
# 
# }

# If running for figures:
residues_in_nectar_and_pollen_from_foliar<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  for(i in (n+1):nrow(output)){
    # units will be ug per g of pollen
    nectar_rud <-( ((x$`Kn-L`*x$fsoil*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(0-x$KdissSoil*(i-n+6)) - exp(0-(x$KelAir+x$KelDeg+x$KelGrow)*(i-n+6))) )  +
      
      ( ((x$`Kn-L`*x$fsurface*x$cf1)/(x$LAI*x$LMA*(1/(1-x$Wleaf))*1000)) * (x$KupSurface/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSurface)) * ( exp(-x$KdissSurface*(i-n+6)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i-n+6))) )
    
    output[i,1]<-(nectar_rud*x$kg_ha)
  }
  
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(x$Type,'_concentration_ug_g_from_foliar'),"day")
  output
  
}




#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_on_field<-lapply(foliar_nectar_datasets,residues_in_nectar_and_pollen_from_foliar)

#then get names of on_field_residue list that match the seed dust output, rename daily_conc_on_field
names(daily_conc_on_field)<-names(on_field_residue_list)[amatch(names(daily_conc_on_field), names(on_field_residue_list), maxDist = Inf)]

#where the names in the on_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_on_field), names(on_field_residue_list)))
on_field_residue_list<-setNames(mapply(c, on_field_residue_list[keys], daily_conc_on_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
on_field_residue_list<-lapply(on_field_residue_list,set_names_in_list)
# 


# combine data -----
#simple function to arrange data
gather_data<-function(x){
  df<-x
  df<-gather(df, "Media", "Value", 4:ncol(df))
  df
  
}

on_model_data<-lapply(on_field_residue_list,gather_data)
on_model_data<-do.call(rbind,on_model_data)
on_model_data$Value<-ifelse(on_model_data$Value == 0,NA,on_model_data$Value)


#Add in specific media for grouping
on_model_data$MediaSub<- sub("_.*", "", on_model_data$Media)
on_model_data$ApplicationType<- str_to_title(sub('.*\\_', "", on_model_data$Media))



