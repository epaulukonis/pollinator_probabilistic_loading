### Probabilistic Crop Loading 

### 08a Initial Onsite Model Outputs

# Edited by E. Paulukonis August 2023

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
  conc_from_application<-( ((0.368*100) /(1.95*0.05*1000))*exp(0-seed_soil_data_on_field$KdissSoil * seed_soil_data_on_field$day)) *seed_soil_data_on_field$AvgRate*1.12085 #now add that amount to the amount in soil using Li (converting AR to kg/ha)estimate for soil concentration. I modified mass fraction to be 40%, and soil bulk density to be 1.44 to match 
  
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

#x<-seed_pollen_datasets[[4]]
residues_in_nectar_and_pollen_from_seed<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  
  for(i in (n+1):nrow(output)){
    # we have two components: a concentration in plant as growth occurs, and an uptake from soil aspect
    conc_in_plant <-( (x$mg_ai_seed *0.20/x$mass)*exp(-(kel_grow+x$rgr)*output[i,2])*as.numeric(x$Fr) )*1000 # units will be ug per g of pollen; the 1000 converts mg to ug
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
  
  for(i in (n+1):nrow(output)){
    # we have two components: a concentration in plant as growth occurs, and an uptake from soil aspect
    conc_in_plant <-( (x$mg_ai_seed *0.20/x$mass)*exp(-(kel_grow+x$rgr)*output[i,2])*as.numeric(x$Fr) )*1000 # units will be ug per g of pollen; the 1000 converts mg to ug
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

    soil_RUD <-  (0.40*100) /(1.95*0.05*1000) *exp(0-x$KdissSoil*output[i,2])  # units will be ug per g of soil, depth of 5 cm; we convert lb/acre to kg/ha here. we also assume 40% goes to soil.
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

agdrift_data<-ag_drift[,c(1,7)] #use ground spray, very fine to fine


#get the seed treatment data and estimate concentration in dust for each seed type
foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
foliar_air_conc<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
foliar_air_conc<-foliar_air_conc[with(foliar_air_conc, order(Compound,Commodity)), ] # order by compound, commodity
foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_ground_low_vf2f #multiple application rate times deposition curve 

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
foliar_soil_conc$air_drift_conc<-foliar_soil_conc$ug_m2*foliar_soil_conc$pond_ground_low_vf2f #multiple application rate times deposition curve 

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
  conc_from_application<-( ((0.368*100) /(1.95*0.05*1000))*exp(0-seed_soil_data_on_field$KdissSoil * seed_soil_data_on_field$day)) *seed_soil_data_on_field$AvgRate*1.12085 #now add that amount to the amount in soil using Li (converting AR to kg/ha)estimate for soil concentration. I modified mass fraction to be 40%, and soil bulk density to be 1.44 to match 
  
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
#x<-foliar_pollen_datasets[[2]]

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


# If running for figures:
residues_in_nectar_and_pollen_from_foliar<-function(x){
  output<- matrix(data=0, nrow=151, ncol=2)
  n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-n:150
  output[(n+1):151,2]<-t
  for(i in (n+1):nrow(output)){
    # units will be ug per g of pollen
    pollen_rud <-( ((x$`Kp-L`*x$fsoil*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(-x$KdissSoil*(i-n+6)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i-(n-1)+6))) )  +

      ( ((x$`Kp-L`*x$fsurface*x$cf1)/(x$LAI*x$LMA*(1/(1-x$Wleaf))*1000)) * (x$KupSurface/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSurface)) * ( exp(-x$KdissSurface*(i-n+6)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i-(n-1)+6))) )

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
    nectar_rud <-( ((x$`Kn-L`*x$fsoil*x$cf1)/(x$Psoil*x$Hsoil*x$cf2)) * (x$KupSoil/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSoil)) * (exp(-x$KdissSoil*(i-n+6)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i-(n-1)+6))) )  +

      ( ((x$`Kn-L`*x$fsurface*x$cf1)/(x$LAI*x$LMA*(1/(1-x$Wleaf))*1000)) * (x$KupSurface/(x$KelAir+x$KelDeg+x$KelGrow - x$KdissSurface)) * ( exp(-x$KdissSurface*(i-n+6)) - exp(-(x$KelAir+x$KelDeg+x$KelGrow)*(i-(n-1)+6))) )

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

#testy<-on_field_residue_list[[2]]


######## Plots ###########
#### Daily outputs by application type----

foliar<-on_field_residue_list[1:10]
soil<-on_field_residue_list[11:12]
seed<-on_field_residue_list[13:18]


#simple function to arrange data
gather_data<-function(x){
  df<-x
  df<-gather(df, "Media", "Value", 4:ncol(df))
  df
  
}

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(8,"Dark2")
names(myColors) <- unique(apprates$Compound)
colScale <- scale_colour_manual(name = "Compound",values = myColors)

##Get foliar output
foliar_data<-lapply(foliar,gather_data)
foliar_data<-do.call(rbind,foliar_data)
foliar_data$Value<-ifelse(foliar_data$Value == 0,NA,foliar_data$Value)

foliar <- ggplot(foliar_data, aes(day, Value, color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  scale_y_continuous(expand = c(0.1,0))+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()+
  theme( axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))

foliar


##Get seed output
seed_data<-lapply(seed,gather_data)
seed_data<-do.call(rbind,seed_data)
seed_data$Value<-ifelse(seed_data$Value == 0,NA,seed_data$Value)

seed <- ggplot(seed_data, aes(day, Value,color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  scale_y_continuous(expand = c(0.1,0))+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()+
  theme( axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))

seed

##Get seed output
soil_data<-lapply(soil,gather_data)
soil_data<-do.call(rbind,soil_data)
soil_data$Value<-ifelse(soil_data$Value == 0,NA,soil_data$Value)

soil <- ggplot(soil_data, aes(day, Value, color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  scale_y_continuous(expand = c(0.1,0))+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))

soil

compare_between_application_type<-plot_grid(foliar, seed, soil,  ncol = 1, hjust=1, vjust=-0.5,rel_widths = c(2,2,2))
compare_between_application_type

#labels = c('Foliar', 'Seed','Soil'),

#function to extract legend 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 


all_data<-lapply(on_field_residue_list,gather_data)
all_data<-do.call(rbind,all_data)
all_data$Value<-ifelse(all_data$Value == 0,NA,all_data$Value)

all_plot_legend <- ggplot(all_data, aes(day, Value, color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()


legend<-get_only_legend(all_plot_legend)

compare_between_application_type<-plot_grid(compare_between_application_type, legend,ncol = 2, rel_widths = c(6,1))
compare_between_application_type

#### Daily outputs by media type ----

#First designate type
foliar_data$ApplicationType<-"Foliar"
soil_data$ApplicationType<-"Soil"
seed_data$ApplicationType<-"Seed"

#Add in specific media for grouping
foliar_data$MediaSub<- sub("_.*", "", foliar_data$Media)
soil_data$MediaSub<- sub("_.*", "", soil_data$Media)
seed_data$MediaSub<- sub("_.*", "", seed_data$Media)


#combine all 3 types
combine_all<-rbind(foliar_data,soil_data,seed_data)

#extract air/dust
air_df<-combine_all[combine_all$MediaSub == 'Air', ]
#extract air/dust
dust_df<-combine_all[combine_all$MediaSub == 'Dust', ]
#extract soil
soil_df<-combine_all[combine_all$MediaSub == 'Soil', ]
#extract pollen
pollen_df<-combine_all[combine_all$MediaSub == 'Pollen', ]
#extract nectar
nectar_df<-combine_all[combine_all$MediaSub == 'Nectar', ]



# airn<- ggplot(air_df, aes(day, Value, group=Compound, color=Compound)) +
#   geom_point(aes(shape=ApplicationType), size=3)+
#   #scale_shape_manual(values=c(16,4,8))+
#   colScale+
#   facet_wrap(~Commodity,scales = "free", nrow=1)+
#   ylab(expression(paste("Residues [ug/", m^{2},"]")))+
#   xlab("Days Post-Application")+
#  # geom_text(x=125, y=4000, label="Air", color="black", size=6)+
#   theme_bw() +
# #  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
#   theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
# 
# airn
# 
# 
# dustn<- ggplot(dust_df, aes(day, Value, group=Compound, color=Compound)) +
#   geom_point(aes(shape=ApplicationType), size=3)+
#   scale_shape_manual(values=c(16,4,8))+
#   facet_wrap(~Commodity,scales = "free", nrow=1)+
#   ylab(expression(paste("Residues [ug/", m^{2},"]")))+
#   xlab("Days Post-Application")+
# # geom_text(x=125, y=5.5, label="Dust", color="black", size=6)+
#   theme_bw() +
#  # theme(plot.margin = unit(c(1,1,1,1), "cm"))+
#   theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
# 
# dustn


soiln<- ggplot(soil_df, aes(day, (Value), color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  #geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
  scale_x_continuous(limits=c(0, 70), breaks=seq(0,70, by=10))+
 # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(MediaSub), cols = vars(Commodity), scales="free_y")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
 #geom_text(x=125, y=0.05, label="Soil", color="black", size=6)+
  theme_bw()+
  theme()+
  #theme(plot.margin = unit(c(1,1,1,1), "cm"))
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
soiln


pollenn<- ggplot(pollen_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  #scale_x_continuous(limits=c(60, 130), breaks=seq(60,130, by=10),labels =c("0","7","17","27","37","47","57","67"))+
  # ifelse(pollen_df$Commodity == "CORN",
  # scale_x_continuous(limits=c(70, 130), breaks=seq(70,130, by=10),labels =c("7","17","27","37","47","57","67")),
  # scale_x_continuous(limits=c(60, 120), breaks=seq(60,120, by=10),labels =c("7","17","27","37","47","57","67"))
  # )+
 # geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
 # facet_wrap(Commodity ~ .,scales = "free", nrow=1)+
  facet_grid(rows = vars(MediaSub), cols = vars(Commodity), scales="free_x")+
  ylab("")+
  xlab("Days After Pollen/Nectar Emergence")+
# geom_text(x=125, y=7.5, label="Pollen", color="black", size=6)+
  theme_bw()+
  theme()+
#  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
pollenn

c<-seq(67,140, by=10)
s<-seq(56,130, by=10)

pollenn<-pollenn + facetted_pos_scales(
  x = list(
  Commodity == "CORN" ~ scale_x_continuous(limits=c(67, 140), breaks=c,labels =c("0","10","20","30","40","50","60","70")),
  Commodity == "SOYBEANS" ~ scale_x_continuous(limits=c(56, 130),breaks=s,labels =c("0","10","20","30","40","50","60","70"))
  )
)



nectarn<- ggplot(nectar_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  #scale_x_continuous(limits=c(60, 120), breaks=seq(60,120, by=10),labels =c("0","7","17","27","37","47","57"))+
  #geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
  #facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(MediaSub), cols = vars(Commodity), scales="free_x")+
  ylab("")+
  xlab("Days After Pollen/Nectar Emergence")+
 #geom_text(x=125, y=0.9, label="Nectar", color="black", size=6)+
  theme_bw()+
 # theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
nectarn

nectarn<-nectarn + facetted_pos_scales(
  x = list(
    Commodity == "SOYBEANS" ~ scale_x_continuous(limits=c(56, 130),breaks=s,labels =c("0","10","20","30","40","50","60","70"))
  )
)


compare_between_media<-plot_grid(
                                 soiln,
                                 pollenn,
                                 nectarn, 
                                 # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
                                 
                                 hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 1,rel_widths = c(3,3,2))
compare_between_media




all_plot_legend <- ggplot(combine_all, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype=ApplicationType), size=1)+
  colScale+
  facet_wrap(~Commodity,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+ 
  theme_bw()+
 theme( legend.key.size = unit(1, 'cm'), #change legend key size
legend.key.height = unit(1, 'cm'), #change legend key height
legend.key.width = unit(1, 'cm'), #change legend key width
legend.title = element_text(size=16), #change legend title font size
legend.text = element_text(size=14),


)
 


legend<-get_only_legend(all_plot_legend)

compare_between_mediaon<-plot_grid(compare_between_media, legend,ncol = 2, rel_widths = c(6,1))
compare_between_mediaon




### get deposition data for table

air_and_dust<-combine_all[combine_all$MediaSub ==  "Air" | combine_all$MediaSub ==  "Dust",]
air_and_dust<-air_and_dust[air_and_dust$day == 0,]



### the 3rd type is by compound
list_by_compound<-split(combine_all, f= combine_all$Compound)

create_plot_by_compound<-function(x){
  df<-x
  
 out<-ggplot(df, aes(day, Value, color=ApplicationType)) +
   geom_point()+
   geom_line(aes(linetype = Commodity), size=0.8)+
   facet_grid(rows = vars(MediaSub), cols = vars(Commodity), 
              scales="free_y", switch = 'y')+
   ylab("Residues")+
   xlab("Days Post-Planting")+
   ggtitle(paste0(x$Compound)) +
   theme_bw()
out

}

list_of_plots<-lapply(list_by_compound,create_plot_by_compound)


### The 4th is similar to Li et al.2022


list_by_compound<-split(combine_all, f= combine_all$Compound)
#x<-list_by_compound[[1]]

create_plot_by_compound<-function(x){
  df<-x
  df<-df[!df$MediaSub=="Air",]
  
  out<-ggplot(df, aes(day, Value, group=interaction(ApplicationType,MediaSub), color=MediaSub)) +
   # geom_point()+
    geom_line( size=0.8)+
    facet_grid(rows=vars(ApplicationType),cols = vars(Commodity), 
              scales="free_y", switch = 'y')+
    ylab("Residues")+
    xlab("Days Post-Planting")+
    ggtitle(paste0(x$Compound)) +
    theme_bw()
  out
  
}

list_of_plots<-lapply(list_by_compound,create_plot_by_compound)


#### Hazard quotients----
options(scipen = 0)
#first designate 
foliar_data$ApplicationType<-"Foliar"
soil_data$ApplicationType<-"Soil"
seed_data$ApplicationType<-"Seed"

#Add in specific media for grouping
foliar_data$MediaSub<- sub("_.*", "", foliar_data$Media)
soil_data$MediaSub<- sub("_.*", "", soil_data$Media)
seed_data$MediaSub<- sub("_.*", "", seed_data$Media)


#combine all 3 types
combine_all<-rbind(foliar_data,soil_data,seed_data)

#ingestion rate for oral
IR<-0.292 #g/day

#surface area of a bumblebee
SA<-2.216 #cm2


# ###max points
# get_max_eec<-combine_all %>% 
#   group_by(Compound,Commodity,ApplicationType,MediaSub) %>%
#   slice(which.max(Value)) %>%
#   arrange(Compound,Commodity,ApplicationType,MediaSub)
# 
# 
#  get_max_eec$units<-ifelse(get_max_eec$MediaSub == "Dust" | get_max_eec$MediaSub == "Air", "ug/m2","ug/g" )
#  eec_df<-merge(get_max_eec,beetox,by="Compound")
#  split_datasets<-split(eec_df, list(eec_df$ApplicationType, eec_df$Commodity), drop=T) 
#  
#  x<-split_datasets[[5]]
#  create_plot_of_rq<-function(x){
#    df<-x
#    
#    depo<-df[df$MediaSub == "Air" |df$MediaSub == "Dust" , ]
#    micro<-df[df$MediaSub == "Pollen" | df$MediaSub == "Nectar"| df$MediaSub == "Soil", ]
#    
#    #first calculate the estimated RQ based on contact with deposition
#    depo$RQ<-((depo$Value/1000)*SA)/depo$Contact_LD50_ug_bee # convert to cm2, multiple by surface area of bumblebee to get eec in ug/bee, and divide by contact LD50
#    
#    #then estimate RQ based on either ingestion (pollen/nectar) or soil contact
#    micro$RQ<-ifelse(micro$MediaSub == "Soil", (micro$Value)/micro$Contact_LD50_ug_bee, (micro$Value * IR)/micro$Oral_LD50_ug_bee)
#   
#    df<-rbind(micro,depo)
#   
#    out<-ggplot(df, aes(Compound, RQ, color=MediaSub)) +
#      geom_hline(yintercept=0.4)+
#      geom_point( size=3.2, aes(shape=MediaSub))+
#      facet_wrap(~MediaSub, scales = "free")+
#      ylab("Risk Quotient")+
#      xlab("Compound")+
#      theme_bw()+
#      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#      ggtitle(paste0(x$ApplicationType,"-", x$Commodity)) 
#    out
#    
#   }
# 
#  list_of_plots<-lapply(split_datasets,create_plot_of_rq)
#  
 
 ###boxplots
#combine all 3 types
 combine_all<-rbind(foliar_data,soil_data,seed_data)
 combine_all$units<-ifelse(combine_all$MediaSub == "Dust" | combine_all$MediaSub == "Air", "ug/m2","ug/g" )
 eec_df<-merge(combine_all,beetox,by="Compound")
 
 # unique(combine_all$Compound)
 # unique(eec_df$Compound)
 # 
 #customize colors
 library(RColorBrewer)
 myColors <- brewer.pal(5,"Dark2")
 names(myColors) <- unique(eec_df$MediaSub)
 colScale <- scale_colour_manual(name = "MediaSub",values = myColors)
 
 split_datasets<-split(eec_df, list(eec_df$ApplicationType), drop=T) 

 
 create_plot_of_rq<-function(x){
   df<-x
   depo<-df[df$MediaSub == "Air" |df$MediaSub == "Dust" , ]
   micro<-df[df$MediaSub == "Pollen" | df$MediaSub == "Nectar"| df$MediaSub == "Soil", ]
   
   #first calculate the estimated RQ based on contact with deposition
   depo$RQ<-((depo$Value/1000)*SA)/depo$Contact_LD50_ug_bee # convert to cm2, multiple by surface area of bumblebee to get eec in ug/bee, and divide by contact LD50
   
   #then estimate RQ based on either ingestion (pollen/nectar) or soil contact
   micro$RQ<-ifelse(micro$MediaSub == "Soil", (micro$Value)/micro$Contact_LD50_ug_bee, (micro$Value * IR)/micro$Oral_LD50_ug_bee)
   df<-rbind(micro,depo)
   
   #create plot
   out<-ggplot(df, aes(MediaSub, log(RQ), fill=MediaSub)) +
     geom_hline(yintercept=log(0.4))+
     geom_boxplot() +
     colScale+
     scale_y_continuous(limits=c(-30,5), breaks=seq(-30,5, by=10))+
     facet_wrap(Commodity ~ Compound, 
                scales="free_y",nrow=2)+
     #facet_wrap(~Compound, scales = "free", nrow=1)+
     ylab(ifelse(df$ApplicationType == "Foliar","Hazard Quotient", ""))+
     theme_bw()+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
     ggtitle(paste0(x$ApplicationType)) +
     theme(legend.position="none",axis.title=element_text(size=14,face="bold"),axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"), axis.title.x=element_blank())
   out
   
 }
 
 
 list_of_plots<-lapply(split_datasets,create_plot_of_rq)

 compare_RQ<-plot_grid(
   list_of_plots[[1]],
   list_of_plots[[2]],
   list_of_plots[[3]],
   # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
   hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 1,rel_widths = c(3,2,1))
 compare_RQ
 
 
legend_plot<-ggplot(eec_df, aes(MediaSub, Value, fill=MediaSub)) +
   geom_hline(yintercept=log(0.4))+
   geom_boxplot() +
   #geom_point( size=3.2, aes(shape=MediaSub))+
   # scale_y_continuous(limits=c(-30,5), breaks=seq(-30,5, by=10))+
   
   facet_wrap(Commodity ~ Compound, 
              scales="free_y",nrow=2)+
   #facet_wrap(~Compound, scales = "free", nrow=1)+
   ylab("Risk Quotient")+
   xlab("Compound")+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
         legend.key.size = unit(1, 'cm'), #change legend key size
                legend.key.height = unit(1, 'cm'), #change legend key height
                legend.key.width = unit(1, 'cm'), #change legend key width
                legend.title = element_text(size=16), #change legend title font size
                legend.text = element_text(size=14))+
   #ggtitle(paste0(x$ApplicationType)) +
  guides(fill=guide_legend(title="Media Type"))
 legend_plot
 
 
 legend<-get_only_legend(legend_plot)
 
 compare_RQ<-plot_grid(compare_RQ, legend,nrow=1, rel_widths = c(6.5,0.5))
 compare_RQ
 
 
 
 
#### Compare against reported field residues----
 options(scipen=999)
 
 ## read in residues from DER
 res<-read.csv(paste0(pest_dir,"/Residues/DER_ResidueData.csv"))
 res<-na.omit(res) 
 residues<-res

 residues$Residues_DER_ugg<-((as.numeric(residues$Mean_ng_g_normalized_1.3mg_seed)*as.numeric(residues$Original_treatment_mg_seed))/residues$Normalized)/1000
 
 residues$Chemical<-toupper(residues$Chemical)
 residues$Plant<-toupper(residues$Plant)
 residues$Plant<-ifelse(residues$Plant == "SOY","SOYBEANS",residues$Plant)
 
 residues<-residues[,c(4,11,12,14)]
 colnames(residues)<-c("Compound","Commodity","MediaSub","Residues_ugg")
 residues$ApplicationType<-"Seed"
 residues$Source<-"Field"
 
 
 # read in residues from lit review
 litres<-read.csv(paste0(pest_dir,"/Residues/Lit_ResidueData.csv"))
 litres<-litres[,c(1:5)]
 litres<-litres[,c(1,3,2,4,5)]
 
 litres<-na.omit(litres)
 litres$Source<-"Field"
 
 colnames(litres)<-colnames(residues)
 litres$Compound<-toupper(litres$Compound)
 
 # get the estimated data
 combine_all<-rbind(foliar_data,soil_data,seed_data)
 combine_all<-combine_all[,c(2,3,6,7,5)]
 colnames(combine_all)[5]<-"Residues_ugg"
 combine_all<-na.omit(combine_all)
 combine_all$Source<-"Estimated"
 # names(combine_all)
 # names(residues)

 combine_all<-combine_all[,c(1:2,4,5,3,6)]
 
 #combine all together
 residues_all<- rbind(combine_all,residues, litres)
 residues_all<-residues_all[!residues_all$Commodity =="COTTON",]


#install.packages("ggh4x")
 
 list_by_applciationtype<-split(residues_all, f= residues_all$MediaSub)
# x<-list_by_applciationtype[[3]]
 
 create_plot_of_fieldcompare<-function(x){
   df<-x
   
   df<-df %>% 
     group_by(MediaSub, ApplicationType,Compound)%>%
     filter(Residues_ugg > 0.001) %>%
     filter(n_distinct(Source) > 1) 
     
   
  out<-ggplot(df, aes(Source, log(Residues_ugg),fill=Source)) +
   geom_boxplot() +
  geom_hline(yintercept=log(0.001))+
   colScale+
 # scale_y_continuous(limits=c(-7,2), breaks=seq(-7,2, by=2))+
  ggh4x:: facet_nested(.~ApplicationType + Compound, 
              scales="free_y")+
   #facet_wrap(~Compound, scales = "free", nrow=1)+
   ylab(ifelse(df$MediaSub == "Pollen","Log Residues [ug/g]", ""))+
   xlab(ifelse(df$MediaSub == "Nectar", "Estimated vs. Field Residues", ""))+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
   ggtitle(paste0(x$MediaSub)) +
   theme(legend.position="none", axis.text.y = element_text(size=14,face="bold"), axis.text.x = element_text(size=14,face="bold"),  axis.title=element_text(size=14,face="bold"))
  out
 }
 

 list_of_estimate_plots<-lapply(list_by_applciationtype,create_plot_of_fieldcompare)
 
 compare_field_est<-plot_grid(
   # list_of_estimate_plots[[1]],
   # list_of_estimate_plots[[2]],
   list_of_estimate_plots[[5]],
   list_of_estimate_plots[[4]],
   list_of_estimate_plots[[3]],

  
   # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
   hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 3,rel_widths = c(1.5,2,1.5))
 
 compare_field_est


#### Compare directly between application types for same compound ----
combine_all<-rbind(foliar_data,soil_data,seed_data)
forcomparisons<-combine_all[combine_all$MediaSub == "Pollen" | combine_all$MediaSub == "Nectar"| combine_all$MediaSub == "Soil", ]
forcomparisons<-na.omit(forcomparisons)
 
paired_data<-forcomparisons %>% 
   group_by(Compound,Commodity)%>%
   filter(n_distinct(ApplicationType) > 1) 

# paired_data<-paired_data[paired_data$day==1 | paired_data$day == 60 | paired_data$day == 70,]
# ind1<-with(paired_data, (MediaSub == "Soil") &
#   ((day==60) | (day==70)))
# 
# ind2<-with(paired_data, (Commodity == "SOYBEANS") &
#              (day==70))
# 
# paired_dataf<-paired_data[!c(ind1 | ind2),]
options("scipen"=2, "digits"=3)

compare_outputs<-split(paired_data, list(paired_data$Compound, paired_data$Commodity), drop=T) 
#x<-compare_outputs[[1]]



library(RColorBrewer)
myColors <- brewer.pal(3,"Dark2")
names(myColors) <- unique(combine_all$ApplicationType)
colScale <- scale_colour_manual(name = "ApplicationType",values = myColors)
fillScale <- scale_fill_manual(name = "ApplicationType",values = myColors)


create_plot_of_methodcompare<-function(x){
  df<-x

ticks<-  ifelse(df$Compound=="BIFENTHRIN",
            c(-45,-35,-20,-10,-5),
            c(-20,-15,-10,-5,0.5))

logticks<-  ifelse(x$Compound=="BIFENTHRIN",
                c(exp(-45),exp(-35),exp(-20),exp(-10),exp(-5)),
                c(exp(-20),exp(-15),exp(-10),exp(-5), exp(0.5)))


ticks<- ticks[1:5]
logticks<-logticks[1:5]

df$Value<-log(df$Value)

out<-ggplot(df, aes(ApplicationType, (Value),color=ApplicationType, fill=ApplicationType)) +
  geom_boxplot()+
  colScale+
  fillScale+
 scale_y_continuous(breaks=ticks, labels=format(logticks,3))+
  facet_wrap( ~MediaSub)+
  ylab("Residues [ug/g]")+
  xlab(ifelse(df$Compound=="IMIDACLOPRID","Application Type", ""))+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=14,face="bold"), axis.text.x = element_text(size=14,face="bold"),  axis.title=element_text(size=14,face="bold"))+
  ggtitle(paste0(x$Compound, "-", x$Commodity)) 
out

}




out_plots<-lapply(compare_outputs, create_plot_of_methodcompare)

compare_app_methods<-plot_grid(
      out_plots[[1]],
      out_plots[[2]],
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 2,rel_widths = c(3,3))
compare_app_methods


